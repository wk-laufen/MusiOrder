module Api

open Fable.Core
open MusiOrder.Models
open Thoth.Fetch
open Thoth.Json

let loadOrderSummary authKey : Async<OrderSummary> = async {
    let url = sprintf "/api/order/summary?authKey=%s" (AuthKey.toString authKey |> JS.encodeURIComponent)
    return! Fetch.``get``(url, caseStrategy = CamelCase) |> Async.AwaitPromise
}

type LoadUsersError =
    | Forbidden
    | Other of string

let loadUsers authKey = async {
    let url = sprintf "/api/users?authKey=%s" (AuthKey.toString authKey |> JS.encodeURIComponent)
    let coders =
        Extra.empty
        |> Extra.withCustom AuthKey.encode AuthKey.decoder
        |> Extra.withDecimal
    match! Fetch.``tryGet``(url, caseStrategy = CamelCase, extra = coders) |> Async.AwaitPromise with
    | Ok (users: UserInfo list) -> return Ok users
    | Error (FetchFailed response) when response.Status = 403 -> return Error Forbidden
    | Error e -> return Error (Other (Helper.message e))
}
