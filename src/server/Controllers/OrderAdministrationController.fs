namespace MusiOrder.Server.Controllers

open FSharp.Control.Tasks.V2.ContextInsensitive
open Microsoft.AspNetCore.Mvc
open MusiOrder.Core
open MusiOrder.Core.OrderAdministration
open MusiOrder.Models
open System

type OrderInfoDto = {
    Id: string
    FirstName: string
    LastName: string
    ProductName: string
    Amount: int
    PricePerUnit: decimal
    Timestamp: DateTimeOffset
}

[<Route("api/administration/order/orders")>]
[<Produces("application/json")>]
type OrderAdministrationController() =

    [<HttpGet>]
    member _.GetOrders([<FromQuery>] authKey: string) =
        task {
            match! User.getByAuthKeyDto authKey with
            | Some user when User.isAdmin user ->
                let! result = getOrders ()

                let dto =
                    result
                    |> List.map (fun o ->
                        let (OrderId s) = o.Id

                        {
                            Id = s
                            FirstName = o.FirstName
                            LastName = o.LastName
                            ProductName = o.ProductName
                            Amount = o.Amount
                            PricePerUnit = o.PricePerUnit
                            Timestamp = o.Timestamp
                        })
                    |> List.toArray

                return OkObjectResult(dto) :> IActionResult
            | Some _ -> return BadRequestObjectResult("NotAuthorized") :> IActionResult
            | None -> return BadRequestObjectResult("InvalidAuthKey") :> IActionResult
        }

    [<HttpDelete("{orderId}")>]
    member _.DeleteOrder([<FromRoute>] orderId: string, [<FromQuery>] authKey: string) =
        task {
            match! User.getByAuthKeyDto authKey with
            | Some user when User.isAdmin user ->
                do! deleteOrder (OrderId orderId)
                return OkResult() :> IActionResult
            | Some _ -> return BadRequestObjectResult("NotAuthorized") :> IActionResult
            | None -> return BadRequestObjectResult("InvalidAuthKey") :> IActionResult
        }
