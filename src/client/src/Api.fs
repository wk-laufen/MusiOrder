module Api

open Fable.Core
open MusiOrder.Models
open Thoth.Fetch
open Thoth.Json

let loadProducts = async {
    let coders =
        Extra.empty
        |> Extra.withCustom ProductId.encode ProductId.decoder
        |> Extra.withDecimal
    let! (products: ProductGroup list) = Fetch.get("/api/grouped-products", caseStrategy = CamelCase, extra = coders) |> Async.AwaitPromise
    return products
}

let loadOrderSummary authKey : Async<OrderSummary> = async {
    let coders =
        Extra.empty
        |> Extra.withDecimal
    let url = sprintf "/api/order/summary?authKey=%s" (AuthKey.toString authKey |> JS.encodeURIComponent)
    return! Fetch.get(url, caseStrategy = CamelCase, extra = coders) |> Async.AwaitPromise
}

type FetchError =
    | Forbidden
    | Other of string

let loadUsers authKey = async {
    let url = sprintf "/api/users?authKey=%s" (AuthKey.toString authKey |> JS.encodeURIComponent)
    let coders =
        Extra.empty
        |> Extra.withCustom AuthKey.encode AuthKey.decoder
        |> Extra.withDecimal
    match! Fetch.tryGet(url, caseStrategy = CamelCase, extra = coders) |> Async.AwaitPromise with
    | Ok (users: UserInfo list) -> return Ok users
    | Error (FetchFailed response) when response.Status = 403 -> return Error Forbidden
    | Error e -> return Error (Other (Helper.message e))
}

let sendOrder authKey order = async {
    let body =
        {
            AuthKey = authKey
            Entries =
                order
                |> Map.toList
                |> List.choose (fun (productId, amount) ->
                    PositiveInteger.tryCreate amount
                    |> Option.map (fun amount -> { ProductId = productId; Amount = amount })
                )
        }
    let coders =
        Extra.empty
        |> Extra.withCustom ProductId.encode ProductId.decoder
        |> Extra.withCustom AuthKey.encode AuthKey.decoder
        |> Extra.withCustom PositiveInteger.encode PositiveInteger.decoder
    do! Fetch.post("/api/order", body, caseStrategy = CamelCase, extra = coders) |> Async.AwaitPromise
}

let addPayment (payment: Payment) = async {
    let coders =
        Extra.empty
        |> Extra.withCustom AuthKey.encode AuthKey.decoder
        |> Extra.withDecimal
    let! (totalAmount: decimal) = Fetch.post("/api/payment", payment, caseStrategy = CamelCase, extra = coders) |> Async.AwaitPromise
    return (payment.UserId, totalAmount)
}

let loadOrderInfo authKey = async {
    let coders =
        Extra.empty
        |> Extra.withDecimal
    let url = sprintf "/api/order/info?authKey=%s" (AuthKey.toString authKey |> JS.encodeURIComponent)
    match! Fetch.tryGet(url, caseStrategy = CamelCase, extra = coders) |> Async.AwaitPromise with
    | Ok (orders: OrderInfo list) -> return Ok orders
    | Error (FetchFailed response) when response.Status = 403 -> return Error Forbidden
    | Error e -> return Error (Other (Helper.message e))
}

let deleteOrder authKey orderId = async {
    let url = sprintf "/api/order/%s?authKey=%s" (JS.encodeURIComponent orderId) (AuthKey.toString authKey |> JS.encodeURIComponent)
    do! Fetch.delete(url, caseStrategy = CamelCase) |> Async.AwaitPromise
}
