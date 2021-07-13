module MusiOrder.Core

open FSharp.Control.Tasks.V2.ContextInsensitive
open Microsoft.Data.Sqlite
open MusiOrder.Models
open System

type Helper =
    static member Box (v: string) = box v
    static member Box (v: DateTimeOffset) = box v
    static member Box (v: Guid) = box (v.ToString())
    static member Box (v: decimal) = box v
    static member Box (AuthKey v) = box v
    static member Box (UserId v) = box v
    static member Box (OrderId v) = box v
    static member Box (v: PositiveInteger) = box (PositiveInteger.value v)
    static member Box (v: NotEmptyString) = box v.Value
    static member Box (v: DBNull) = box v

type User = {
    Id: UserId
    FirstName: string
    LastName: string
    AuthKey: AuthKey option
    Role: string
}
module User =
    let isAdmin user =
        user.Role.Equals("admin", StringComparison.InvariantCultureIgnoreCase)

    let getByAuthKey (AuthKey authKey) =
        DB.readSingle
            "SELECT `id`, `firstName`, `lastName`, `keyCode`, `role` FROM `ActiveMember` WHERE `keyCode` = @KeyCode"
            [ ("@KeyCode", Helper.Box authKey) ]
            (fun reader ->
                {
                    Id = reader.GetString(0) |> UserId
                    FirstName = reader.GetString(1)
                    LastName = reader.GetString(2)
                    AuthKey = DB.tryGet reader reader.GetString 3 |> Option.map AuthKey
                    Role = reader.GetString(4)
                }
            )

    let getById (userId: UserId) =
        DB.readSingle
            "SELECT `id`, `firstName`, `lastName`, `keyCode`, `role` FROM `ActiveMember` WHERE `id` = @Id"
            [ ("@Id", Helper.Box userId) ]
            (fun reader ->
                {
                    Id = reader.GetString(0) |> UserId
                    FirstName = reader.GetString(1)
                    LastName = reader.GetString(2)
                    AuthKey = DB.tryGet reader reader.GetString 3 |> Option.map AuthKey
                    Role = reader.GetString(4)
                }
            )

    let getBalance (UserId userId) = task {
        let! totalOrderPrice = DB.readSingle "SELECT COALESCE(SUM(`amount` * `pricePerUnit`), 0) as `price` FROM `Order` WHERE userId = @UserId" [ ("@UserId", userId) ] (fun reader -> reader.GetDecimal(0))
        let! totalBalance = DB.readSingle "SELECT COALESCE(SUM(`amount`), 0) FROM `MemberPayment` WHERE userId = @UserId" [ ("@UserId", userId) ] (fun reader -> reader.GetDecimal(0))
        return (Option.defaultValue 0m totalBalance) - (Option.defaultValue 0m totalOrderPrice)
    }

module Order =
    open MusiOrder.Models.Order

    let getLatestOrdersFromUser (userId: UserId) =
        DB.read
            "SELECT `articleName`, `amount`, datetime(`timestamp`, 'localtime') as `time` FROM `Order` WHERE userId = @UserId AND `time` > @OldestTime ORDER BY `time` DESC"
            [ ("@UserId", Helper.Box userId); ("@OldestTime", DateTimeOffset.Now.AddMonths(-1) |> Helper.Box) ]
            (fun reader -> { Timestamp = reader.GetDateTimeOffset(2); ProductName = reader.GetString(0); Amount = reader.GetInt32(1) })

    let getProductGroups () = task {
        let! groups = DB.read "SELECT `id`, `name` FROM `ArticleGroup` ORDER BY `grade`" [] (fun reader -> (reader.GetString(0), reader.GetString(1)))
        let! products = DB.read "SELECT `id`, `groupId`, `name`, `price` FROM `Article` WHERE `state` = 'enabled' ORDER BY `grade`" [] (fun reader -> (reader.GetString(0), reader.GetString(1), reader.GetString(2), reader.GetDecimal(3)))
        return
            groups
            |> List.choose (fun (groupId, groupName) ->
                let products =
                    products
                    |> List.filter (fun (_, gId, _, _) -> gId = groupId)
                    |> List.map (fun (productId, _, productName, price) ->
                        {
                            Id = ProductId productId
                            Name = productName
                            Price = price
                        }
                    )
                match products with
                | [] -> None
                | x ->
                    Some {
                        Name = groupName
                        Products = x
                    }
            )
    }

    let getUserInfo () = task {
        let query = """
            SELECT `M`.`id`, `M`.`firstName`, `M`.`lastName`, `M`.`keyCode`, COALESCE(`payment`, 0) - COALESCE(`orderPrice`, 0) as `balance`
            FROM `ActiveMember` as `M`
            LEFT OUTER JOIN (SELECT userId, SUM(`amount`) as `payment` FROM `MemberPayment` GROUP BY userId) AS `P` ON `M`.`id` = `P`.`userId`
            LEFT OUTER JOIN (SELECT userId, SUM(`amount` * `pricePerUnit`) as `orderPrice` FROM `Order` GROUP BY userId) AS `O` ON `M`.`id` = `O`.`userId`
            GROUP BY `M`.`id`
            ORDER BY `M`.`lastName`, `M`.`firstName`
        """
        return! DB.read query [] (fun reader ->
            {
                Id = reader.GetString(0) |> UserId
                FirstName = reader.GetString(1)
                LastName = reader.GetString(2)
                AuthKey = DB.tryGet reader reader.GetString 3 |> Option.map AuthKey
                Balance = reader.GetDecimal(4)
            })
    }

    let saveOrder data (userId: UserId) = task {
        let! productData = task {
            let productIds = data |> List.map (fun e -> let (ProductId p) = e.ProductId in p)
            let parameterNames = productIds |> Seq.mapi (fun i _ -> sprintf "@ArticleId%d" i) |> String.concat ", "
            let query = sprintf "SELECT `id`, `name`, `price` FROM `Article` WHERE `id` IN (%s)" parameterNames
            let parameters =
                productIds
                |> List.mapi (fun i productId -> (sprintf "@ArticleId%d" i, productId))
            return! DB.readIndexed query parameters (fun reader -> reader.GetString(0), (reader.GetString(1), reader.GetDecimal(2)))
        }
        let parameters =
            data
            |> List.map (fun entry ->
                let (ProductId productId) = entry.ProductId
                let fn (productName: string, price: decimal) =
                    [
                        ("@Id", Guid.NewGuid() |> Helper.Box)
                        ("@UserId", Helper.Box userId)
                        ("@ArticleName", Helper.Box productName)
                        ("@Amount", Helper.Box entry.Amount)
                        ("@PricePerUnit", Helper.Box price)
                        ("@Timestamp", Helper.Box DateTimeOffset.Now)
                    ]
                Ok fn
                |> Result.apply (Map.tryFind productId productData |> Result.ofOption ProductNotFound)
                |> Result.mapError (fun e -> OrderEntryErrors (entry.ProductId, e))
            )
            |> List.sequence
        match parameters with
        | Ok parameters ->
            do! DB.writeMany "INSERT INTO `Order` (`id`, `userId`, `articleName`, `amount`, `pricePerUnit`, `timestamp`) VALUES (@Id, @UserId, @ArticleName, @Amount, @PricePerUnit, @Timestamp)" parameters
            return Ok ()
        | Error e -> return Error e
    }

module UserPaymentAdministration =
    open MusiOrder.Models.UserPaymentAdministration

    let getUserInfo () = task {
        let query = """
            SELECT `M`.`id`, `M`.`firstName`, `M`.`lastName`, `lastOrderTimestamp`, COALESCE(`payment`, 0) - COALESCE(`orderPrice`, 0) as `balance`
            FROM `ActiveMember` as `M`
            LEFT OUTER JOIN (SELECT userId, SUM(`amount`) as `payment` FROM `MemberPayment` GROUP BY userId) AS `P` ON `M`.`id` = `P`.`userId`
            LEFT OUTER JOIN (SELECT userId, MAX(DATETIME(`Order`.`timestamp`, 'localtime')) as `lastOrderTimestamp`, SUM(`amount` * `pricePerUnit`) as `orderPrice` FROM `Order` GROUP BY userId) AS `O` ON `M`.`id` = `O`.`userId`
            GROUP BY `M`.`id`
            ORDER BY `M`.`lastName`, `M`.`firstName`
        """
        return! DB.read query [] (fun reader ->
            {
                Id = reader.GetString(0) |> UserId
                FirstName = reader.GetString(1)
                LastName = reader.GetString(2)
                LatestOrderTimestamp = DB.tryGet reader reader.GetDateTimeOffset 3
                Balance = reader.GetDecimal(4)
            })
    }

    let savePayment (UserId userId) (amount: decimal) = task {
        let parameters =
            [
                ("@Id", Guid.NewGuid() |> Helper.Box)
                ("@UserId", Helper.Box userId)
                ("@Amount", Helper.Box amount)
                ("@Timestamp", Helper.Box DateTimeOffset.Now)
            ]
        do! DB.write "INSERT INTO `MemberPayment` (`id`, `userId`, `amount`, `timestamp`) VALUES (@Id, @UserId, @Amount, @Timestamp)" parameters
    }

module UserAdministration =
    open MusiOrder.Models.UserAdministration

    let getExistingUsers () = task {
        let query = """
            SELECT `id`, `firstName`, `lastName`, `keyCode`, `role`
            FROM `ActiveMember`
            ORDER BY `lastName`, `firstName`
        """
        return! DB.read query [] (fun reader ->
            let userId = reader.GetString(0) |> UserId
            {
                Id = userId
                Data = {
                    FirstName = reader.GetString(1) |> NotEmptyString.tryCreate |> Option.defaultWith (fun () -> failwithf "DB error: First name of user (%O) is empty" userId)
                    LastName = reader.GetString(2) |> NotEmptyString.tryCreate |> Option.defaultWith (fun () -> failwithf "DB error: Last name of user (%O) is empty" userId)
                    AuthKey = DB.tryGet reader reader.GetString 3 |> Option.map AuthKey
                    Role = reader.GetString(4) |> fun roleName -> UserRole.tryParse roleName |> Option.defaultWith (fun () -> failwithf "DB error: Can't parse user role \"%s\"" roleName)
                }
            })
    }

    let private isDuplicateKeyCodeException (e: SqliteException) =
        e.Message = "SQLite Error 19: 'UNIQUE constraint failed: Member.keyCode'."

    let createUser data = task {
        try
            let newUserId = sprintf "%O" (Guid.NewGuid()) |> UserId
            do!
                DB.write
                    "INSERT INTO `Member` (`id`, `firstName`, `lastName`, `keyCode`, `role`) VALUES (@Id, @FirstName, @LastName, @KeyCode, @Role)"
                    [
                        ("@Id", Helper.Box newUserId)
                        ("@FirstName", Helper.Box data.FirstName)
                        ("@LastName", Helper.Box data.LastName)
                        ("@KeyCode", data.AuthKey |> Option.map Helper.Box |> Option.defaultValue (Helper.Box DBNull.Value))
                        ("@Role", UserRole.toString data.Role |> Helper.Box)
                    ]
            return Ok newUserId
        with
        | :? SqliteException as e when isDuplicateKeyCodeException e ->
            let! user = data.AuthKey |> Option.bindTask User.getByAuthKey
            let userName = user |> Option.map (fun v -> sprintf "%s %s" v.LastName v.FirstName)
            return Error (KeyCodeTaken userName)
    }

    let updateUser (userId: UserId) data = task {
        try
            // TODO only allow updating not deleted users?
            do!
                DB.write
                    "UPDATE `Member` SET `firstName` = @FirstName, `lastName` = @LastName, `keyCode` = @KeyCode, `role` = @Role WHERE `id` = @Id"
                    [
                        ("@Id", Helper.Box userId)
                        ("@FirstName", Helper.Box data.FirstName)
                        ("@LastName", Helper.Box data.LastName)
                        ("@KeyCode", data.AuthKey |> Option.map Helper.Box |> Option.defaultValue (Helper.Box DBNull.Value))
                        ("@Role", UserRole.toString data.Role |> Helper.Box)
                    ]
            return Ok ()
        with
        | :? SqliteException as e when isDuplicateKeyCodeException e ->
            let! user = data.AuthKey |> Option.bindTask User.getByAuthKey
            let userName = user |> Option.map (fun v -> sprintf "%s %s" v.LastName v.FirstName)
            return Error (KeyCodeTaken userName)
    }

    let deleteUser (userId: UserId) = task {
        return! DB.write "UPDATE `Member` SET `keyCode` = NULL, `deleteTimestamp` = @DeleteTimestamp WHERE `id` = @Id" [ ("@Id", Helper.Box userId); ("@DeleteTimestamp", Helper.Box DateTimeOffset.Now) ]
    }

module OrderAdministration =
    open MusiOrder.Models.OrderAdministration

    let getOrders () = task {
        let query = """
            SELECT `O`.`id`, `M`.`firstName`, `M`.`lastName`, `O`.`articleName`, `O`.`amount`, `O`.`pricePerUnit`, `O`.`timestamp`
            FROM `Order` AS `O`
            JOIN `Member` AS `M` ON `M`.id = `O`.userId
            WHERE `O`.`timestamp` > @OldestTime
            ORDER BY `O`.`timestamp` DESC
        """
        return!
            DB.read
                query
                [ ("@OldestTime", DateTimeOffset.Now.AddMonths(-1) |> Helper.Box) ]
                (fun reader ->
                    {
                        Id = reader.GetString(0) |> OrderId
                        FirstName = reader.GetString(1)
                        LastName = reader.GetString(2)
                        ProductName = reader.GetString(3)
                        Amount = reader.GetInt32(4)
                        PricePerUnit = reader.GetDecimal(5)
                        Timestamp = reader.GetDateTimeOffset(6)
                    }
                )
    }

    let deleteOrder (orderId: OrderId) = task {
        do! DB.write "DELETE FROM `Order` WHERE `id` = @Id" [ ("@Id", Helper.Box orderId) ]
    }
