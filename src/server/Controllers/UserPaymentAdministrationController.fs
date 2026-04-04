namespace MusiOrder.Server.Controllers

open FSharp.Control.Tasks.V2.ContextInsensitive
open Microsoft.AspNetCore.Mvc
open MusiOrder.Core
open MusiOrder.Core.UserPaymentAdministration
open MusiOrder.Models
open System

type PaymentDto = { Amount: decimal }

type UserPaymentInfoDto = {
    Id: string
    FirstName: string
    LastName: string
    LatestOrderTimestamp: DateTimeOffset option
    Balance: decimal
    SuggestedBalanceChanges: decimal[]
}

[<Route("api/administration/user-payment")>]
[<Produces("application/json")>]
type UserPaymentAdministrationController() =

    [<HttpGet("users")>]
    member _.GetUsers([<FromQuery>] authKey: string) =
        task {
            match! User.getByAuthKeyDto authKey with
            | Some user when User.isAdmin user ->
                let! result = getUserInfo ()

                let dto =
                    result
                    |> List.map (fun u ->
                        let (UserId s) = u.Id

                        {
                            Id = s
                            FirstName = u.FirstName
                            LastName = u.LastName
                            LatestOrderTimestamp = u.LatestOrderTimestamp
                            Balance = u.Balance
                            SuggestedBalanceChanges = u.SuggestedBalanceChanges |> List.toArray
                        })
                    |> List.toArray

                return OkObjectResult(dto) :> IActionResult
            | Some _ -> return BadRequestObjectResult("NotAuthorized") :> IActionResult
            | None -> return BadRequestObjectResult("InvalidAuthKey") :> IActionResult
        }

    [<HttpPost("{userId}")>]
    member _.PostPayment([<FromRoute>] userId: string, [<FromQuery>] authKey: string, [<FromBody>] dto: PaymentDto) =
        task {
            match! User.getByAuthKeyDto authKey with
            | Some user when User.isAdmin user ->
                let uid = UserId userId
                do! savePayment uid dto.Amount
                let! balance = User.getBalance uid
                return OkObjectResult(balance) :> IActionResult
            | Some _ -> return BadRequestObjectResult("NotAuthorized") :> IActionResult
            | None -> return BadRequestObjectResult("InvalidAuthKey") :> IActionResult
        }
