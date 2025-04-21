module MusiOrder.Core

open FSharp.Control.Tasks.V2.ContextInsensitive
open Microsoft.Data.Sqlite
open MusiOrder.Models
open System
open System.Text.Json

type Helper =
    static member Box (v: string) = box v
    static member Box (v: DateTimeOffset) = box v
    static member Box (v: Guid) = box (v.ToString())
    static member Box (v: decimal) = box v
    static member Box (UserId v) = box v
    static member Box (ProductId v) = box v
    static member Box (ProductGroupId v) = box v
    static member Box (OrderId v) = box v
    static member Box (v: PositiveInteger) = box (PositiveInteger.value v)
    static member Box (v: NotEmptyString) = box v.Value
    static member Box (v: NonNegativeDecimal) = box (NonNegativeDecimal.value v)
    static member Box (v: DBNull) = box v

type DbAuthKey = {
    keyType: string
    keyCode: string
}
module DbAuthKey =
    let toDb = function
        | NFCAuthKey keyCode -> ("nfc", keyCode)
    let toDomain v =
        if String.Equals(v.keyType, "nfc", StringComparison.InvariantCultureIgnoreCase) then NFCAuthKey v.keyCode
        else failwith $"Unknown key type \"{v.keyType}\""

type UserRole = User | OrderAssistant | Admin

module UserRole =
    let tryParse = function
        | "admin" -> Some Admin
        | "order-assistant" -> Some OrderAssistant
        | "user" -> Some User
        | _ -> None

type User = {
    Id: UserId
    FirstName: string
    LastName: string
    AuthKeys: AuthKey list
    Role: UserRole
}
module User =
    let isAdmin user = user.Role.IsAdmin

    let canOrderForOtherUsers user =
        match user.Role with
        | User -> false
        | OrderAssistant
        | Admin -> true

    let getByAuthKey authKey =
        let (keyType, keyCode) = DbAuthKey.toDb authKey
        DB.readSingle
            "SELECT m.id, m.firstName, m.lastName, m.authKeys, m.role FROM ActiveMember m, json_each(authKeys) WHERE json_each.value ->> '$.keyCode' = @KeyCode AND json_each.value ->> '$.keyType' = @KeyType"
            [ ("@KeyType", Helper.Box keyType); ("@KeyCode", Helper.Box keyCode)]
            (fun reader ->
                {
                    Id = reader.GetString(0) |> UserId
                    FirstName = reader.GetString(1)
                    LastName = reader.GetString(2)
                    AuthKeys = JsonSerializer.Deserialize<DbAuthKey[]>(reader.GetString(3)) |> Seq.map DbAuthKey.toDomain |> Seq.toList
                    Role = reader.GetString(4) |> fun roleName -> UserRole.tryParse roleName |> Option.defaultWith (fun () -> failwithf "DB error: Can't parse user role \"%s\"" roleName)
                }
            )

    let getById (userId: UserId) =
        DB.readSingle
            "SELECT `id`, `firstName`, `lastName`, `authKeys`, `role` FROM `ActiveMember` WHERE `id` = @Id"
            [ ("@Id", Helper.Box userId) ]
            (fun reader ->
                {
                    Id = reader.GetString(0) |> UserId
                    FirstName = reader.GetString(1)
                    LastName = reader.GetString(2)
                    AuthKeys = JsonSerializer.Deserialize<DbAuthKey[]>(reader.GetString(3)) |> Seq.map DbAuthKey.toDomain |> Seq.toList
                    Role = reader.GetString(4) |> fun roleName -> UserRole.tryParse roleName |> Option.defaultWith (fun () -> failwithf "DB error: Can't parse user role \"%s\"" roleName)
                }
            )

    let getBalance (UserId userId) = task {
        let! totalOrderPrice = DB.readSingle "SELECT COALESCE(SUM(`amount` * (`pricePerUnit` * 100)), 0) as `price` FROM `Order` WHERE userId = @UserId" [ ("@UserId", userId) ] (fun reader -> reader.GetDecimal(0) / 100m)
        let! totalBalance = DB.readSingle "SELECT COALESCE(SUM(`amount` * 100), 0) FROM `MemberPayment` WHERE userId = @UserId" [ ("@UserId", userId) ] (fun reader -> reader.GetDecimal(0) / 100m)
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
            SELECT `M`.`id`, `M`.`firstName`, `M`.`lastName`, COALESCE(`payment`, 0) - COALESCE(`orderPrice`, 0) as `balance`
            FROM `ActiveMember` as `M`
            LEFT OUTER JOIN (SELECT userId, SUM(`amount` * 100) as `payment` FROM `MemberPayment` GROUP BY userId) AS `P` ON `M`.`id` = `P`.`userId`
            LEFT OUTER JOIN (SELECT userId, SUM(`amount` * (`pricePerUnit` * 100)) as `orderPrice` FROM `Order` GROUP BY userId) AS `O` ON `M`.`id` = `O`.`userId`
            GROUP BY `M`.`id`
            ORDER BY `M`.`lastName`, `M`.`firstName`
        """
        return! DB.read query [] (fun reader ->
            {
                Id = reader.GetString(0) |> UserId
                FirstName = reader.GetString(1)
                LastName = reader.GetString(2)
                Balance = reader.GetDecimal(3) / 100m
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
            LEFT OUTER JOIN (SELECT userId, SUM(`amount` * 100) as `payment` FROM `MemberPayment` GROUP BY userId) AS `P` ON `M`.`id` = `P`.`userId`
            LEFT OUTER JOIN (SELECT userId, MAX(DATETIME(`Order`.`timestamp`, 'localtime')) as `lastOrderTimestamp`, SUM(`amount` * (`pricePerUnit` * 100)) as `orderPrice` FROM `Order` GROUP BY userId) AS `O` ON `M`.`id` = `O`.`userId`
            GROUP BY `M`.`id`
            ORDER BY `M`.`lastName`, `M`.`firstName`
        """
        return! DB.read query [] (fun reader ->
            {
                Id = reader.GetString(0) |> UserId
                FirstName = reader.GetString(1)
                LastName = reader.GetString(2)
                LatestOrderTimestamp = DB.tryGet reader reader.GetDateTimeOffset 3
                Balance = reader.GetDecimal(4) / 100m
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
            SELECT `id`, `firstName`, `lastName`, `authKeys`, `role`
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
                    AuthKeys = JsonSerializer.Deserialize<DbAuthKey[]>(reader.GetString(3)) |> Seq.map DbAuthKey.toDomain |> Seq.toList
                    Role = reader.GetString(4) |> fun roleName -> UserRole.tryParse roleName |> Option.defaultWith (fun () -> failwithf "DB error: Can't parse user role \"%s\"" roleName)
                }
            })
    }

    let private isDuplicateAuthKeyException (e: SqliteException) =
        e.Message = "SQLite Error 19: 'UNIQUE constraint failed: AuthKey.keyCode, AuthKey.keyType'."

    let createUser (data: ExistingUserData) = task {
        try
            let newUserId = sprintf "%O" (Guid.NewGuid()) |> UserId
            do! DB.executeCommands [
                (
                    "INSERT INTO `Member` (`id`, `firstName`, `lastName`, `role`) VALUES (@Id, @FirstName, @LastName, @Role)",
                    [
                        ("@Id", Helper.Box newUserId)
                        ("@FirstName", Helper.Box data.FirstName)
                        ("@LastName", Helper.Box data.LastName)
                        ("@Role", UserRole.toString data.Role |> Helper.Box)
                    ]
                )
                yield! data.AuthKeys |> List.map (fun authKey ->
                    let (keyType, keyCode) = DbAuthKey.toDb authKey
                    "INSERT INTO `AuthKey` (`keyCode`, `keyType`, `userId`) VALUES (@KeyCode, @KeyType, @UserId)",
                    [
                        ("@KeyCode", Helper.Box keyCode)
                        ("@KeyType", Helper.Box keyType)
                        ("@UserId", Helper.Box newUserId)
                    ]
                )
            ]
            return Ok newUserId
        with
        | :? SqliteException as e when isDuplicateAuthKeyException e ->
            let! users = data.AuthKeys |> List.map User.getByAuthKey |> System.Threading.Tasks.Task.WhenAll
            let userNames = users |> Seq.choose (Option.map (fun v -> $"{v.LastName} {v.FirstName}")) |> Seq.toList
            return Error (KeyCodeTaken userNames)
    }

    let updateUser (userId: UserId) (data: PatchUserData) = task {
        try
            // TODO only allow updating not deleted users?
            let fields = [
                match data.FirstName with
                | Some v -> ("firstName", Helper.Box v)
                | None -> ()

                match data.LastName with
                | Some v -> ("lastName", Helper.Box v)
                | None -> ()

                match data.Role with
                | Some v -> ("role", UserRole.toString v |> Helper.Box)
                | None -> ()
            ]
            let updateFields = fields |> Seq.map (fst >> fun v -> $"`%s{v}`=@%s{v}") |> String.concat ", "
            do!
                DB.executeCommands [
                    if not <| List.isEmpty fields then
                        (
                            $"UPDATE `Member` SET %s{updateFields} WHERE `id`=@Id",
                            [
                                ("@Id", Helper.Box userId)
                                yield! fields |> List.map (fun (name, value) -> ($"@%s{name}", value))
                            ]
                        )
                    yield! data.AddAuthKeys |> List.map (fun authKey ->
                        let (keyType, keyCode) = DbAuthKey.toDb authKey
                        "INSERT INTO `AuthKey` (`keyCode`, `keyType`, `userId`, `creationTime`) VALUES (@KeyCode, @KeyType, @UserId, @CreationTime)",
                        [
                            ("@KeyCode", Helper.Box keyCode)
                            ("@KeyType", Helper.Box keyType)
                            ("@UserId", Helper.Box userId)
                            ("@CreationTime", Helper.Box DateTimeOffset.Now)
                        ]
                    )
                    yield! data.RemoveAuthKeys |> List.map (fun authKey ->
                        let (keyType, keyCode) = DbAuthKey.toDb authKey
                        "DELETE FROM `AuthKey` WHERE `keyCode`=@KeyCode AND `keyType`=@KeyType AND `userId`=@UserId",
                        [
                            ("@KeyCode", Helper.Box keyCode)
                            ("@KeyType", Helper.Box keyType)
                            ("@UserId", Helper.Box userId)
                        ]
                    )
                ]
            return Ok ()
        with
        | :? SqliteException as e when isDuplicateAuthKeyException e ->
            let! users = data.AddAuthKeys |> List.map User.getByAuthKey |> System.Threading.Tasks.Task.WhenAll
            let userNames = users |> Seq.choose (Option.map (fun v -> $"{v.LastName} {v.FirstName}")) |> Seq.toList
            return Error (KeyCodeTaken userNames)
    }

    let deleteUser (userId: UserId) = task {
        return! DB.executeCommands [
            (
                "UPDATE `Member` SET `deleteTimestamp` = @DeleteTimestamp WHERE `id` = @Id",
                [ ("@Id", Helper.Box userId); ("@DeleteTimestamp", Helper.Box DateTimeOffset.Now) ]
            )
            (
                "DELETE FROM `AuthKey` WHERE `userId`=@Id",
                [ ("@Id", Helper.Box userId) ]
            )
        ]
    }

module ProductAdministration =
    open MusiOrder.Models.ProductAdministration

    let getProducts () = task {
        let! groups =
            DB.read
                "SELECT `id`, `name` FROM `ArticleGroup` ORDER BY `grade`"
                []
                (fun reader ->
                    let productGroupId = ProductGroupId (reader.GetString(0))
                    {
                        Id = productGroupId
                        Data = {
                            Name = reader.GetString(1) |> NotEmptyString.tryCreate |> Option.defaultWith (fun () -> failwithf "DB error: Name of product group (%O) is empty" productGroupId)
                        }
                        Products = []
                    }
                )
        let! products =
            DB.read
                "SELECT `id`, `groupId`, `name`, `price`, `state` FROM `Article` ORDER BY `grade`"
                []
                (fun reader ->
                    let productId = ProductId (reader.GetString(0))
                    let product = {
                        Id = productId
                        Data = {
                            Name = reader.GetString(2) |> NotEmptyString.tryCreate |> Option.defaultWith (fun () -> failwithf "DB error: Name of product (%O) is empty" productId)
                            Price = reader.GetDecimal(3) |> NonNegativeDecimal.tryCreate |> Option.defaultWith (fun () -> failwithf "DB error: Price of product (%O) is invalid" productId)
                            State =
                                let state = reader.GetString(4)
                                ProductState.tryParse state |> Option.defaultWith (fun () -> failwithf "DB error: Unknown product state (\"%s\")" state)
                        }
                    }
                    let productGroupId = ProductGroupId (reader.GetString(1))
                    (productGroupId, product)
                )
        return
            groups
            |> List.map (fun group ->
                let products =
                    products
                    |> List.filter (fun (groupId, product) -> groupId = group.Id)
                    |> List.map snd
                { group with Products = products }
            )
    }

    let createProductGroup data = task {
        let newProductGroupId = sprintf "%O" (Guid.NewGuid()) |> ProductGroupId
        do!
            DB.write
                "INSERT INTO `ArticleGroup` (`id`, `grade`, `name`) VALUES (@Id, (SELECT COALESCE(MAX(`grade`) + 1, 1) FROM `ArticleGroup`), @Name)"
                [
                    "@Id", Helper.Box newProductGroupId
                    "@Name", Helper.Box data.Name
                ]
        return newProductGroupId
    }

    let updateProductGroup (productGroupId: ProductGroupId) data =
        DB.write
            "UPDATE `ArticleGroup` SET `name` = @Name Where `id` = @Id"
            [
                "@Id", Helper.Box productGroupId
                "@Name", Helper.Box data.Name
            ]

    let moveUpProductGroup (productGroupId: ProductGroupId) =
        DB.write """
            WITH `a` AS (
              SELECT `id`, `grade` FROM `ArticleGroup` AS `g` WHERE `id` = @Id
            ), `b` AS (
              SELECT `id`, `grade` FROM `a`
              UNION
              SELECT `id`, `grade` FROM `ArticleGroup` WHERE `grade` IN (SELECT `grade` - 1 FROM `a`)
            ), `c` AS (
              SELECT `b1`.`id`, `b1`.`grade` `oldGrade`, `b2`.`grade` `newGrade` FROM `b` AS `b1` CROSS JOIN `b` AS `b2` WHERE `b1`.`grade` <> `b2`.`grade`
            )
            UPDATE `ArticleGroup`
            SET `grade` = (SELECT `newGrade` FROM `c` WHERE `c`.`id` = `ArticleGroup`.`id`)
            WHERE `id` IN (SELECT `id` FROM `c`)
            """
            [
                "@Id", Helper.Box productGroupId
            ]

    let moveDownProductGroup (productGroupId: ProductGroupId) =
        DB.write """
            WITH `a` AS (
              SELECT `id`, `grade` FROM `ArticleGroup` AS `g` WHERE `id` = @Id
            ), `b` AS (
              SELECT `id`, `grade` FROM `a`
              UNION
              SELECT `id`, `grade` FROM `ArticleGroup` WHERE `grade` IN (SELECT `grade` + 1 FROM `a`)
            ), `c` AS (
              SELECT `b1`.`id`, `b1`.`grade` `oldGrade`, `b2`.`grade` `newGrade` FROM `b` AS `b1` CROSS JOIN `b` AS `b2` WHERE `b1`.`grade` <> `b2`.`grade`
            )
            UPDATE `ArticleGroup`
            SET `grade` = (SELECT `newGrade` FROM `c` WHERE `c`.`id` = `ArticleGroup`.`id`)
            WHERE `id` IN (SELECT `id` FROM `c`)
            """
            [
                "@Id", Helper.Box productGroupId
            ]

    let deleteProductGroup (productGroupId: ProductGroupId) = task {
        try
            do! DB.write "DELETE FROM `ArticleGroup` WHERE `id` = @Id" [ "@Id", Helper.Box productGroupId ]
            return true
        with :? SqliteException as e when e.SqliteErrorCode = 19 -> // FOREIGN KEY constraint failed
            return false
    }

    let createProduct (productGroupId: ProductGroupId) (data: ProductData) = task {
        let newProductId = sprintf "%O" (Guid.NewGuid()) |> ProductGroupId
        do!
            DB.write """
                INSERT INTO `Article` (`id`, `groupId`, `state`, `grade`, `name`, `price`)
                VALUES (@Id, @GroupId, @State, (SELECT COALESCE(MAX(`grade`) + 1, 1) FROM `Article` WHERE `groupId` = @GroupId), @Name, @Price)
                """
                [
                    "@Id", Helper.Box newProductId
                    "@GroupId", Helper.Box productGroupId
                    "@State", Helper.Box (ProductState.toString data.State)
                    "@Name", Helper.Box data.Name
                    "@Price", Helper.Box data.Price
                ]
        return newProductId
    }

    let updateProduct (productId: ProductId) data =
        DB.write
            "UPDATE `Article` SET `state` = @State, `name` = @Name, `price` = @Price Where `id` = @Id"
            [
                "@Id", Helper.Box productId
                "@State", Helper.Box (ProductState.toString data.State)
                "@Name", Helper.Box data.Name
                "@Price", Helper.Box data.Price
            ]

    let moveUpProduct (productId: ProductId) =
        DB.write """
            WITH `a` AS (
              SELECT `id`, `groupId`, `grade` FROM `Article` AS `g` WHERE `id` = @Id
            ), `b` AS (
              SELECT `id`, `grade` FROM `a`
              UNION
              SELECT `id`, `grade` FROM `Article` WHERE `grade` IN (SELECT `grade` - 1 FROM `a` WHERE `Article`.`groupId` = `a`.`groupId`)
            ), `c` AS (
              SELECT `b1`.`id`, `b1`.`grade` `oldGrade`, `b2`.`grade` `newGrade` FROM `b` AS `b1` CROSS JOIN `b` AS `b2` WHERE `b1`.`grade` <> `b2`.`grade`
            )
            UPDATE `Article`
            SET `grade` = (SELECT `newGrade` FROM `c` WHERE `c`.`id` = `Article`.`id`)
            WHERE `id` IN (SELECT `id` FROM `c`)
            """
            [
                "@Id", Helper.Box productId
            ]

    let moveDownProduct (productId: ProductId) =
        DB.write """
            WITH `a` AS (
              SELECT `id`, `groupId`, `grade` FROM `Article` AS `g` WHERE `id` = @Id
            ), `b` AS (
              SELECT `id`, `grade` FROM `a`
              UNION
              SELECT `id`, `grade` FROM `Article` WHERE `grade` IN (SELECT `grade` + 1 FROM `a` WHERE `Article`.`groupId` = `a`.`groupId`)
            ), `c` AS (
              SELECT `b1`.`id`, `b1`.`grade` `oldGrade`, `b2`.`grade` `newGrade` FROM `b` AS `b1` CROSS JOIN `b` AS `b2` WHERE `b1`.`grade` <> `b2`.`grade`
            )
            UPDATE `Article`
            SET `grade` = (SELECT `newGrade` FROM `c` WHERE `c`.`id` = `Article`.`id`)
            WHERE `id` IN (SELECT `id` FROM `c`)
            """
            [
                "@Id", Helper.Box productId
            ]

    let deleteProduct (productId: ProductId) =
        DB.write "DELETE FROM `Article` WHERE `id` = @Id" [ "@Id", Helper.Box productId ]

module OrderAdministration =
    open MusiOrder.Models.OrderAdministration

    let getOrders () = task {
        let query = """
            SELECT `O`.`id`, `M`.`firstName`, `M`.`lastName`, `O`.`articleName`, `O`.`amount`, `O`.`pricePerUnit`, `O`.`timestamp`
            FROM `Order` AS `O`
            LEFT JOIN `Member` AS `M` ON `M`.id = `O`.userId
            WHERE `O`.`timestamp` >= @OldestTime
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

module OrderStatistics =
    open MusiOrder.Models.OrderStatistics

    let getOrders (startTime: DateTime) (endTime: DateTime) = task {
        let query = """
            SELECT `O`.`id`, `M`.`firstName`, `M`.`lastName`, `O`.`articleName`, `O`.`amount`, `O`.`pricePerUnit`, `O`.`timestamp`
            FROM `Order` AS `O`
            LEFT JOIN `Member` AS `M` ON `M`.id = `O`.userId
            WHERE `O`.`timestamp` >= @OldestTime AND `O`.`timestamp` < @NewestTime
        """
        return!
            DB.read
                query
                [
                    "@OldestTime", Helper.Box startTime
                    "@NewestTime", Helper.Box endTime
                ]
                (fun reader ->
                    {
                        FirstName = reader.GetString(1)
                        LastName = reader.GetString(2)
                        ProductName = reader.GetString(3)
                        Amount = reader.GetInt32(4)
                        PricePerUnit = reader.GetDecimal(5)
                        Timestamp = reader.GetDateTimeOffset(6)
                    }
                )
    }
