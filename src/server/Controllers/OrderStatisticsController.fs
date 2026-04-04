namespace MusiOrder.Server.Controllers

open FSharp.Control.Tasks.V2.ContextInsensitive
open Microsoft.AspNetCore.Mvc
open MusiOrder.Core
open MusiOrder.Core.OrderStatistics
open MusiOrder.Models
open System

type OrderStatisticsDto = {
    FirstName: string
    LastName: string
    ProductName: string
    Amount: int
    PricePerUnit: decimal
    Timestamp: DateTimeOffset
}

[<Route("api/administration/report")>]
[<Produces("application/json")>]
type OrderStatisticsController() =

    [<HttpGet("orders")>]
    member _.GetOrders([<FromQuery>] authKey: string, [<FromQuery>] startTime: string, [<FromQuery>] endTime: string) =
        task {
            match! User.getByAuthKeyDto authKey with
            | Some user when User.isAdmin user ->
                let parsedStart = startTime |> Option.ofObj |> Option.bind DateTime.tryParseDate
                let parsedEnd = endTime |> Option.ofObj |> Option.bind DateTime.tryParseDate

                match parsedStart, parsedEnd with
                | Some st, Some et ->
                    let! result = getOrders st et

                    let dto =
                        result
                        |> List.map (fun o -> {
                            FirstName = o.FirstName
                            LastName = o.LastName
                            ProductName = o.ProductName
                            Amount = o.Amount
                            PricePerUnit = o.PricePerUnit
                            Timestamp = o.Timestamp
                        })
                        |> List.toArray

                    return OkObjectResult(dto) :> IActionResult
                | _ -> return BadRequestObjectResult("MissingTimeRange") :> IActionResult
            | Some _ -> return BadRequestObjectResult("NotAuthorized") :> IActionResult
            | None -> return BadRequestObjectResult("InvalidAuthKey") :> IActionResult
        }
