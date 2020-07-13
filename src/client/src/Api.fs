module Api

open Fable.Core
open MusiOrder.Models
open Thoth.Fetch
open Thoth.Json

let loadOrderSummary authKey : Async<OrderSummary> = async {
    let url = sprintf "/api/order/summary?authKey=%s" (AuthKey.toString authKey |> JS.encodeURIComponent)
    return! Fetch.``get``(url, caseStrategy = CamelCase) |> Async.AwaitPromise
}
