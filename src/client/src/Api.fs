module Api

open Fable.Core
open MusiOrder.Models
open Thoth.Fetch
open Thoth.Json

let loadProducts = async {
    let! (products: ProductGroup list) = Fetch.get("/api/grouped-products", caseStrategy = CamelCase, extra = Json.coders) |> Async.AwaitPromise
    return products
}

let loadOrderSummary authKey = async {
    let url = sprintf "/api/order/summary?authKey=%s" (AuthKey.toString authKey |> JS.encodeURIComponent)
    let! (orderSummary: OrderSummary) = Fetch.get(url, caseStrategy = CamelCase, extra = Json.coders) |> Async.AwaitPromise
    return orderSummary
}

type FetchError =
    | Forbidden
    | Other of string

let loadUserInfo authKey = async {
    let url = sprintf "/api/user/info?authKey=%s" (AuthKey.toString authKey |> JS.encodeURIComponent)
    match! Fetch.tryGet(url, caseStrategy = CamelCase, extra = Json.coders) |> Async.AwaitPromise with
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
    do! Fetch.post("/api/order", body, caseStrategy = CamelCase, extra = Json.coders) |> Async.AwaitPromise
}

let addPayment (payment: Payment) = async {
    let! (totalAmount: decimal) = Fetch.post("/api/payment", payment, caseStrategy = CamelCase, extra = Json.coders) |> Async.AwaitPromise
    return (payment.UserId, totalAmount)
}

let loadOrderInfo authKey = async {
    let url = sprintf "/api/order/info?authKey=%s" (AuthKey.toString authKey |> JS.encodeURIComponent)
    match! Fetch.tryGet(url, caseStrategy = CamelCase, extra = Json.coders) |> Async.AwaitPromise with
    | Ok (orders: OrderInfo list) -> return Ok orders
    | Error (FetchFailed response) when response.Status = 403 -> return Error Forbidden
    | Error e -> return Error (Other (Helper.message e))
}

let deleteOrder authKey orderId = async {
    let url = sprintf "/api/order/%s?authKey=%s" (JS.encodeURIComponent orderId) (AuthKey.toString authKey |> JS.encodeURIComponent)
    do! Fetch.delete(url, caseStrategy = CamelCase, extra = Json.coders) |> Async.AwaitPromise
}

let loadUserData authKey = async {
    let url = sprintf "/api/user?authKey=%s" (AuthKey.toString authKey |> JS.encodeURIComponent)
    match! Fetch.tryGet(url, caseStrategy = CamelCase, extra = Json.coders) |> Async.AwaitPromise with
    | Ok (users: ExistingUserData list) -> return Ok users
    | Error (FetchFailed response) when response.Status = 403 -> return Error Forbidden
    | Error e -> return Error (Other (Helper.message e))
}

type SaveUserHttpError =
    | SaveUserError of SaveUserError
    | Other of string

let updateUser authKey userId (user: UserData) = async {
    match! Fetch.tryPut(sprintf "/api/user/%s?authKey=%s" userId (AuthKey.toString authKey |> JS.encodeURIComponent), user, caseStrategy = CamelCase, extra = Json.coders) |> Async.AwaitPromise with
    | Ok () -> return Ok ()
    | Error (FetchFailed response) when response.Status = 400 ->
        let! e = response.text() |> Async.AwaitPromise
        match Decode.Auto.fromString(e, caseStrategy = CamelCase, extra = Json.coders) with
        | Ok e -> return Error (SaveUserError e)
        | Error e -> return Error (Other e)
    | Error e -> return Error (Other (Helper.message e))
}

let createUser authKey (user: UserData) = async {
    match! Fetch.tryPost(sprintf "/api/user?authKey=%s" (AuthKey.toString authKey |> JS.encodeURIComponent), user, caseStrategy = CamelCase, extra = Json.coders) |> Async.AwaitPromise with
    | Ok (userId: string) -> return Ok userId
    | Error (FetchFailed response) when response.Status = 400 ->
        let! e = response.text() |> Async.AwaitPromise
        match Decode.Auto.fromString(e, caseStrategy = CamelCase, extra = Json.coders) with
        | Ok e -> return Error (SaveUserError e)
        | Error e -> return Error (Other e)
    | Error e -> return Error (Other (Helper.message e))
}
