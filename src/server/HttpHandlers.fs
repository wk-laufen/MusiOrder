module MusiOrder.Server.HttpHandlers

open FSharp.Control.Tasks.V2.ContextInsensitive
open Giraffe
open Microsoft.AspNetCore.Http
open MusiOrder.Models

let handleGetGroupedProducts =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            let response =
                [
                    {
                        Name = "Getränke"
                        Products = [
                            {
                                Id = ProductId "20ac42f8-9b56-49a9-b432-6f04a645d1fb"
                                Name = "Bier"
                                Price = 2.5
                            }
                            {
                                Id = ProductId "b66041ba-f9f8-4611-af5c-e26df1317a42"
                                Name = "Cola"
                                Price = 1.5
                            }
                            {
                                Id = ProductId "be8ce543-b5ac-4887-8ae9-367e9bcca454"
                                Name = "Mineral"
                                Price = 1.0
                            }
                            {
                                Id = ProductId "41184fed-32db-46a7-9f76-2289ceb8a13a"
                                Name = "Almdudler"
                                Price = 1.5
                            }
                        ]
                    }
                    {
                        Name = "Speisen"
                        Products = [
                            {
                                Id = ProductId "63292321-0747-49fe-9e91-cce5db2fd49f"
                                Name = "Chips"
                                Price = 1.2
                            }
                            {
                                Id = ProductId "0adc81e9-fe04-4312-bd6a-f16c113362b9"
                                Name = "Soletti"
                                Price = 0.8
                            }
                        ]
                    }
                ]
                |> List.toArray
            return! Successful.OK response next ctx
        }

let handlePostOrder =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            let! data = ctx.BindModelAsync<Order>()
            let (AuthKey authKey) = data.AuthKey
            if authKey.StartsWith "12" then
                printfn "Placing order %A" data.Entries
                return! Successful.OK () next ctx
            else
                return! RequestErrors.badRequest (setBodyFromString "Invalid auth key") next ctx
        }

let handleGetOrderSummary =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            match ctx.TryGetQueryStringValue "authKey" with
            | Some authKey ->
                if authKey.StartsWith "12" then
                    let result =
                        {
                            ClientFullName = "Johannes Egger"
                            Balance = 43.7
                            LatestOrders = [
                                {
                                    Timestamp = System.DateTime.Today.AddDays(-1.).Add(System.TimeSpan(20, 17, 0))
                                    ProductName = "Bier"
                                    Amount = 2
                                }
                                {
                                    Timestamp = System.DateTime.Today.AddDays(-1.).Add(System.TimeSpan(20, 15, 0))
                                    ProductName = "Spritzer"
                                    Amount = 143
                                }
                                {
                                    Timestamp = System.DateTime.Today.AddDays(-1.).Add(System.TimeSpan(19, 37, 0))
                                    ProductName = "Almdudler"
                                    Amount = 2
                                }
                                {
                                    Timestamp = System.DateTime.Today.AddDays(-8.).Add(System.TimeSpan(19, 32, 0))
                                    ProductName = "Bier"
                                    Amount = 2
                                }
                                {
                                    Timestamp = System.DateTime.Today.AddDays(-15.).Add(System.TimeSpan(19, 32, 0))
                                    ProductName = "Bier"
                                    Amount = 2
                                }
                                {
                                    Timestamp = System.DateTime.Today.AddDays(-22.).Add(System.TimeSpan(19, 32, 0))
                                    ProductName = "Bier"
                                    Amount = 2
                                }
                                {
                                    Timestamp = System.DateTime.Today.AddDays(-29.).Add(System.TimeSpan(19, 32, 0))
                                    ProductName = "Bier"
                                    Amount = 2
                                }
                            ]
                        }
                    return! Successful.OK result next ctx
                else
                    return! RequestErrors.badRequest (setBodyFromString "Invalid auth key") next ctx
            | None -> return! RequestErrors.badRequest (setBodyFromString "No auth key provided") next ctx
        }
