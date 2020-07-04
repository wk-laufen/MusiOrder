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
                                Price = float price
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
            let! user = DB.getUser data.AuthKey
            let articleIds = data.Entries |> List.map (fun e -> let (ProductId p) = e.ProductId in p) |> List.toArray |> String.concat ","
            let! articleData = DB.readIndexed "SELECT `id`, `name`, `price` FROM `Article` WHERE `id` IN (@ArticleIds)" [ ("@ArticleIds", articleIds) ] (fun reader -> reader.GetString(0), (reader.GetString(1), reader.GetDecimal(2)))
            match user with
            | Some user ->
                let parameters =
                    data.Entries
                    |> List.map (fun entry ->
                        let (ProductId productId) = entry.ProductId
                        let (articleName, price) = Map.find productId articleData
                        [
                            ("@Id", sprintf "%O" (Guid.NewGuid()) |> box)
                            ("@UserId", box user.Id)
                            ("@ArticleName", box articleName)
                            ("@Amount", box entry.Amount)
                            ("@PricePerUnit", box price)
                            ("@OrderedAt", box DateTimeOffset.Now)
                        ]
                    )
                do! DB.writeMany "INSERT INTO `Order` (`id`, `userId`, `articleName`, `amount`, `pricePerUnit`, `timestamp`) VALUES (@Id, @UserId, @ArticleName, @Amount, @PricePerUnit, @OrderedAt)" parameters
                return! Successful.OK () next ctx
            | None ->
                return! RequestErrors.badRequest (setBodyFromString "Unknown auth key") next ctx
        }

let handleGetOrderSummary =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            match! ctx.TryGetQueryStringValue "authKey" |> Option.bindTask (AuthKey >> DB.getUser) with
            | Some user ->
                let! latestOrders = DB.read "SELECT `articleName`, `amount`, datetime(`timestamp`, 'localtime') as `time` FROM `Order` WHERE userId = @UserId AND `time` > @OldestTime ORDER BY `time` DESC" [ ("@UserId", box user.Id); ("@OldestTime", DateTimeOffset.Now.AddMonths(-1) |> box) ] (fun reader -> { Timestamp = reader.GetDateTimeOffset(2); ProductName = reader.GetString(0); Amount = reader.GetInt32(1) })
                let! totalOrderPrice = DB.readSingle "SELECT sum(`amount` * `pricePerUnit`) as `price` FROM `Order` WHERE userId = @UserId" [ ("@UserId", user.Id) ] (fun reader -> reader.GetDecimal(0))
                let! totalBalance = DB.readSingle "SELECT sum(`amount`) FROM `MemberPayment` WHERE userId = @UserId" [ ("@UserId", user.Id) ] (fun reader -> reader.GetDecimal(0))
                let result =
                    {
                        ClientFullName = sprintf "%s %s" user.FirstName user.LastName
                        Balance = float ((Option.defaultValue 0m totalBalance) - (Option.defaultValue 0m totalOrderPrice))
                        LatestOrders = latestOrders
                    }
                return! Successful.OK result next ctx
            | None -> return! RequestErrors.badRequest (setBodyFromString "Invalid auth key") next ctx
        }
