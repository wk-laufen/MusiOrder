module MusiOrder.Server.HttpHandlers

open FSharp.Control.Tasks.V2.ContextInsensitive
open Giraffe
open Microsoft.AspNetCore.Http
open Microsoft.Data.Sqlite
open MusiOrder.Models
open System

module Option =
    let bindTask fn o = task {
        match o with
        | Some v -> return! fn v
        | None -> return None
    }

module Result =
    let ofOption e = function
        | Some v -> Ok v
        | None -> Error e

    let apply r fn =
        match fn, r with
        | Ok fn, Ok r -> Ok (fn r)
        | Error fn, Ok r -> Error fn
        | Ok fn, Error r -> Error [ r ]
        | Error fn, Error r -> Error (fn @ [ r ])

module List =
    let sequence l =
        (l, Ok [])
        ||> List.foldBack (fun item state ->
            match state, item with
            | Ok x, Ok v -> Ok (v :: x)
            | Error e, Ok v -> Error e
            | Ok e, Error v -> Error [ v ]
            | Error e, Error ve -> Error (ve :: e)
        )

let handleGetGroupedProducts =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            let! groups = DB.read "SELECT `id`, `name` FROM `ArticleGroup` ORDER BY `grade`" [] (fun reader -> (reader.GetString(0), reader.GetString(1)))
            let! articles = DB.read "SELECT `id`, `groupId`, `name`, `price` FROM `Article` WHERE `state` = 'enabled' ORDER BY `grade`" [] (fun reader -> (reader.GetString(0), reader.GetString(1), reader.GetString(2), reader.GetDecimal(3)))
            let response =
                groups
                |> List.choose (fun (groupId, groupName) ->
                    let articles =
                        articles
                        |> List.filter (fun (_, gId, _, _) -> gId = groupId)
                        |> List.map (fun (articleId, _, articleName, price) ->
                            {
                                Id = ProductId articleId
                                Name = articleName
                                Price = price
                            }
                        )
                    match articles with
                    | [] -> None
                    | x ->
                        Some {
                            Name = groupName
                            Products = x
                        }
                )
            return! Successful.OK response next ctx
        }

let handlePostOrder =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            let! data = ctx.BindModelAsync<Order>()
            let! articleData = task {
                let articleIds = data.Entries |> List.map (fun e -> let (ProductId p) = e.ProductId in p)
                let parameterNames = articleIds |> Seq.mapi (fun i _ -> sprintf "@ArticleId%d" i) |> String.concat ", "
                let query = sprintf "SELECT `id`, `name`, `price` FROM `Article` WHERE `id` IN (%s)" parameterNames
                let parameters =
                    articleIds
                    |> List.mapi (fun i productId -> (sprintf "@ArticleId%d" i, productId))
                return! DB.readIndexed query parameters (fun reader -> reader.GetString(0), (reader.GetString(1), reader.GetDecimal(2)))
            }
            match! DB.getUser data.AuthKey with
            | Some user ->
                let parameters =
                    data.Entries
                    |> List.map (fun entry ->
                        let (ProductId productId) = entry.ProductId
                        let fn (articleName: string, price: decimal) =
                            [
                                ("@Id", sprintf "%O" (Guid.NewGuid()) |> box)
                                ("@UserId", box user.Id)
                                ("@ArticleName", box articleName)
                                ("@Amount", PositiveInteger.value entry.Amount |> box)
                                ("@PricePerUnit", box price)
                                ("@Timestamp", box DateTimeOffset.Now)
                            ]
                        Ok fn
                        |> Result.apply (Map.tryFind productId articleData |> Result.ofOption ArticleNotFound)
                        |> Result.mapError (fun e -> OrderEntryErrors (entry.ProductId, e))
                    )
                    |> List.sequence
                match parameters with
                | Ok parameters ->
                    do! DB.writeMany "INSERT INTO `Order` (`id`, `userId`, `articleName`, `amount`, `pricePerUnit`, `timestamp`) VALUES (@Id, @UserId, @ArticleName, @Amount, @PricePerUnit, @Timestamp)" parameters
                    return! Successful.OK () next ctx
                | Error errors ->
                    return! RequestErrors.BAD_REQUEST errors next ctx
            | None ->
                return! RequestErrors.BAD_REQUEST [ AddOrderError.InvalidAuthKey ] next ctx
        }

let handleGetOrderSummary =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            match! ctx.TryGetQueryStringValue "authKey" |> Option.bindTask (AuthKey >> DB.getUser) with
            | Some user ->
                let! balance = DB.getUserBalance user.Id
                let! latestOrders = DB.read "SELECT `articleName`, `amount`, datetime(`timestamp`, 'localtime') as `time` FROM `Order` WHERE userId = @UserId AND `time` > @OldestTime ORDER BY `time` DESC" [ ("@UserId", box user.Id); ("@OldestTime", DateTimeOffset.Now.AddMonths(-1) |> box) ] (fun reader -> { Timestamp = reader.GetDateTimeOffset(2); ProductName = reader.GetString(0); Amount = reader.GetInt32(1) })
                let result =
                    {
                        ClientFullName = sprintf "%s %s" user.FirstName user.LastName
                        Balance = balance
                        LatestOrders = latestOrders
                    }
                return! Successful.OK result next ctx
            | None -> return! RequestErrors.BAD_REQUEST LoadOrderSummaryError.InvalidAuthKey next ctx
        }

let handleGetUserInfo =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            match! ctx.TryGetQueryStringValue "authKey" |> Option.bindTask (AuthKey >> DB.getUser) with
            | Some user when DB.User.isAdmin user ->
                let query = """
                    SELECT `Member`.`id`, `Member`.`firstName`, `Member`.`lastName`, `Member`.`keyCode`, `lastOrderTimestamp`, coalesce(`payment`, 0) - coalesce(`orderPrice`, 0) as `balance`
                    FROM `Member`
                    LEFT OUTER JOIN (SELECT userId, sum(`amount`) as `payment` FROM `MemberPayment` GROUP BY userId) AS P ON `Member`.`id` = `P`.`userId`
                    LEFT OUTER JOIN (SELECT userId, max(datetime(`Order`.`timestamp`, 'localtime')) as `lastOrderTimestamp`, sum(`amount` * `pricePerUnit`) as `orderPrice` FROM `Order` GROUP BY userId) AS O ON `Member`.`id` = `O`.`userId`
                    GROUP BY `Member`.`id`
                    ORDER BY `Member`.`lastName`, `Member`.`firstName`
                """
                let! result = DB.read query [] (fun reader -> { Id = reader.GetString(0); FirstName = reader.GetString(1); LastName = reader.GetString(2); AuthKey = DB.tryGet reader reader.GetString 3 |> Option.map AuthKey; LatestOrderTimestamp = DB.tryGet reader reader.GetDateTimeOffset 4; Balance = reader.GetDecimal(5) })
                return! Successful.OK result next ctx
            | Some _ -> return! RequestErrors.BAD_REQUEST LoadUserDataError.NotAuthorized next ctx
            | None -> return! RequestErrors.BAD_REQUEST LoadUserDataError.InvalidAuthKey next ctx
        }

let handleGetUserData =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            match! ctx.TryGetQueryStringValue "authKey" |> Option.bindTask (AuthKey >> DB.getUser) with
            | Some user when DB.User.isAdmin user ->
                let query = """
                    SELECT `id`, `firstName`, `lastName`, `keyCode`, `role`
                    FROM `Member`
                    ORDER BY `Member`.`lastName`, `Member`.`firstName`
                """
                let! result = DB.read query [] (fun reader ->
                    let userId = reader.GetString(0)
                    {
                        Id = userId
                        Data = {
                            FirstName = reader.GetString(1) |> NotEmptyString.tryCreate |> Option.defaultWith (fun () -> failwithf "DB error: First name of user with id \"%s\" is empty" userId)
                            LastName = reader.GetString(2) |> NotEmptyString.tryCreate |> Option.defaultWith (fun () -> failwithf "DB error: Last name of user with id \"%s\" is empty" userId)
                            AuthKey = DB.tryGet reader reader.GetString 3 |> Option.map AuthKey
                            Role = reader.GetString(4) |> fun roleName -> UserRole.tryParse roleName |> Option.defaultWith (fun () -> failwithf "DB error: Can't parse user role \"%s\"" roleName)
                        }
                    })
                return! Successful.OK result next ctx
            | Some _ -> return! RequestErrors.BAD_REQUEST LoadUserDataError.NotAuthorized next ctx
            | None -> return! RequestErrors.BAD_REQUEST LoadUserDataError.InvalidAuthKey next ctx
        }

let handlePostPayment =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            let! data = ctx.BindModelAsync<Payment>()
            match! DB.getUser data.AuthKey with
            | Some user when DB.User.isAdmin user ->
                let parameters =
                    [
                        ("@Id", sprintf "%O" (Guid.NewGuid()) |> box)
                        ("@UserId", box data.UserId)
                        ("@Amount", box data.Amount)
                        ("@Timestamp", box DateTimeOffset.Now)
                    ]
                do! DB.write "INSERT INTO `MemberPayment` (`id`, `userId`, `amount`, `timestamp`) VALUES (@Id, @UserId, @Amount, @Timestamp)" parameters
                let! balance = DB.getUserBalance data.UserId
                return! Successful.OK balance next ctx
            | Some _ -> return! RequestErrors.BAD_REQUEST AddPaymentError.InvalidAuthKey next ctx
            | None -> return! RequestErrors.BAD_REQUEST AddPaymentError.NotAuthorized next ctx
        }

let handleGetOrderInfo =
    fun (next: HttpFunc) (ctx: HttpContext) ->
        task {
            match! ctx.TryGetQueryStringValue "authKey" |> Option.bindTask (AuthKey >> DB.getUser) with
            | Some user when DB.User.isAdmin user ->
                let query = """
                    SELECT `O`.`id`, `M`.`firstName`, `M`.`lastName`, `O`.`articleName`, `O`.`amount`, `O`.`pricePerUnit`, `O`.`timestamp`
                    FROM `Order` AS `O`
                    JOIN `Member` AS `M` ON `M`.id = `O`.userId
                    WHERE `O`.`timestamp` > @OldestTime
                    ORDER BY `O`.`timestamp` DESC
                """
                let! result = DB.read query [ ("@OldestTime", DateTimeOffset.Now.AddMonths(-1) |> box) ] (fun reader -> { Id = reader.GetString(0); FirstName = reader.GetString(1); LastName = reader.GetString(2); ArticleName = reader.GetString(3); Amount = reader.GetInt32(4); PricePerUnit = reader.GetDecimal(5); Timestamp = reader.GetDateTimeOffset(6) })
                return! Successful.OK result next ctx
            | Some _ -> return! RequestErrors.BAD_REQUEST LoadOrderInfoError.NotAuthorized next ctx
            | None -> return! RequestErrors.BAD_REQUEST LoadOrderInfoError.InvalidAuthKey next ctx
        }

let handleDeleteOrder orderId =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            match! ctx.TryGetQueryStringValue "authKey" |> Option.bindTask (AuthKey >> DB.getUser) with
            | Some user when DB.User.isAdmin user ->
                do! DB.write "DELETE FROM `Order` WHERE `id` = @Id" [ ("@Id", box orderId) ]
                return! Successful.OK () next ctx
            | Some _ -> return! RequestErrors.BAD_REQUEST DeleteOrderError.NotAuthorized next ctx
            | None -> return! RequestErrors.BAD_REQUEST DeleteOrderError.InvalidAuthKey next ctx
        }

let private saveUser fn =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            let! data = ctx.BindModelAsync<UserData>()
            match! ctx.TryGetQueryStringValue "authKey" |> Option.bindTask (AuthKey >> DB.getUser) with
            | Some user when DB.User.isAdmin user ->
                try
                    return! fn user data next ctx
                with
                    | :? SqliteException as e when e.Message = "SQLite Error 19: 'UNIQUE constraint failed: Member.keyCode'." ->
                        let! userName = task {
                            try
                                let! userName =
                                    DB.readSingle
                                        "SELECT `lastName`, `firstName` FROM `Member` WHERE `keyCode` = @KeyCode"
                                        [ ("@KeyCode", data.AuthKey |> Option.get |> AuthKey.toString |> box) ]
                                        (fun reader -> sprintf "%s %s" (reader.GetString(0)) (reader.GetString(1)))
                                return userName
                            with _ -> return None
                        }
                        return! RequestErrors.BAD_REQUEST (KeyCodeTaken userName) next ctx
            | Some _ -> return! RequestErrors.BAD_REQUEST SaveUserError.NotAuthorized next ctx
            | None -> return! RequestErrors.BAD_REQUEST SaveUserError.InvalidAuthKey next ctx
        }

let handleCreateUser : HttpHandler =
    saveUser (fun admin data next ctx -> task {
        let newUserId = sprintf "%O" (Guid.NewGuid())
        do!
            DB.write
                "INSERT INTO `Member` (`id`, `firstName`, `lastName`, `keyCode`, `role`) VALUES(@Id, @FirstName, @LastName, @KeyCode, @Role)"
                [
                    ("@Id", box newUserId)
                    ("@FirstName", box data.FirstName.Value)
                    ("@LastName", box data.LastName.Value)
                    ("@KeyCode", data.AuthKey |> Option.map (AuthKey.toString >> box) |> Option.defaultValue (box DBNull.Value))
                    ("@Role", box (UserRole.toString data.Role))
                ]
        return! Successful.OK newUserId next ctx
    })

let handleUpdateUser userId =
    saveUser (fun admin data next ctx -> task {
        if admin.Id = userId && data.Role <> Admin then
            return! RequestErrors.BAD_REQUEST DowngradeSelfNotAllowed next ctx
        else
            do!
                DB.write
                    "UPDATE `Member` SET `firstName` = @FirstName, `lastName` = @LastName, `keyCode` = @KeyCode, `role` = @Role WHERE `id` = @Id"
                    [
                        ("@Id", box userId)
                        ("@FirstName", box data.FirstName.Value)
                        ("@LastName", box data.LastName.Value)
                        ("@KeyCode", data.AuthKey |> Option.map (AuthKey.toString >> box) |> Option.defaultValue (box DBNull.Value))
                        ("@Role", box (UserRole.toString data.Role))
                    ]
            return! Successful.OK () next ctx
    })
