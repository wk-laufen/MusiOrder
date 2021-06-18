module MusiOrder.Server.HttpHandlers

open FSharp.Control.Tasks.V2.ContextInsensitive
open Giraffe
open Microsoft.AspNetCore.Http
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

type OrderEntryError =
    | ArticleNotFound

type AddOrderError =
    | InvalidAuthKey
    | OrderEntryErrors of ProductId * OrderEntryError list

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
                return! RequestErrors.BAD_REQUEST InvalidAuthKey next ctx
        }

type GetOrderSummaryError =
    | InvalidAuthKey

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
            | None -> return! RequestErrors.BAD_REQUEST InvalidAuthKey next ctx
        }

let handleGetUsers =
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
                let! result = DB.read query [] (fun reader -> { Id = reader.GetString(0); FirstName = reader.GetString(1); LastName = reader.GetString(2); AuthKey = AuthKey <| reader.GetString(3); LatestOrderTimestamp = DB.tryGet reader reader.GetDateTimeOffset 4; Balance = reader.GetDecimal(5) })
                return! Successful.OK result next ctx
            | Some _ -> return! RequestErrors.FORBIDDEN () next ctx
            | None -> return! RequestErrors.BAD_REQUEST InvalidAuthKey next ctx
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
            | _ -> return! RequestErrors.FORBIDDEN () next ctx
        }
