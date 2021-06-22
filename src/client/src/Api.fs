module Api

open Fable.Core
open MusiOrder.Models
open Thoth.Fetch
open Thoth.Json

let inline tryGet url = async {
    return!
        Fetch.tryGet(url, caseStrategy = CamelCase, extra = Json.coders)
        |> Async.AwaitPromise
}

let inline tryPut url data = async {
    return!
        Fetch.tryPut(url, data, caseStrategy = CamelCase, extra = Json.coders)
        |> Async.AwaitPromise
}

let inline tryPost url data = async {
    return!
        Fetch.tryPost(url, data, caseStrategy = CamelCase, extra = Json.coders)
        |> Async.AwaitPromise
}

let inline tryDelete url = async {
    return!
        Fetch.tryDelete(url, caseStrategy = CamelCase, extra = Json.coders)
        |> Async.AwaitPromise
}

type ApiError<'a> =
    | ExpectedError of 'a
    | UnexpectedError of string

let inline handleErrors request = async {
    match! request with
    | Ok result -> return Ok result
    | Error (FetchFailed response) ->
        let! e = response.text() |> Async.AwaitPromise
        match Decode.Auto.fromString(e, caseStrategy = CamelCase, extra = Json.coders) with
        | Ok e -> return Error (ExpectedError e)
        | Error e -> return Error (UnexpectedError e)
    | Error e -> return Error (UnexpectedError (Helper.message e))
}

let loadProducts : Async<Result<ProductGroup list, string>> = async {
    let! result = tryGet "/api/grouped-products"
    return result |> Result.mapError Helper.message
}

let loadOrderSummary authKey : Async<Result<OrderSummary, ApiError<LoadOrderSummaryError>>> = async {
    let url = sprintf "/api/order/summary?authKey=%s" (AuthKey.toString authKey |> JS.encodeURIComponent)
    return! tryGet url |> handleErrors
}

let loadUserInfo authKey : Async<Result<UserInfo list, ApiError<LoadUserDataError>>> = async {
    let url = sprintf "/api/user/info?authKey=%s" (AuthKey.toString authKey |> JS.encodeURIComponent)
    return! tryGet url |> handleErrors
}

let sendOrder authKey order : Async<Result<unit, ApiError<AddOrderError list>>> = async {
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
    return! tryPost "/api/order" body |> handleErrors
}

let addPayment (payment: Payment) : Async<Result<decimal, ApiError<AddPaymentError>>> = async {
    return! tryPost "/api/payment" payment |> handleErrors
}

let loadOrderInfo authKey : Async<Result<OrderInfo list, ApiError<LoadOrderInfoError>>> = async {
    let url = sprintf "/api/order/info?authKey=%s" (AuthKey.toString authKey |> JS.encodeURIComponent)
    return! tryGet url |> handleErrors
}

let deleteOrder authKey orderId : Async<Result<unit, ApiError<DeleteOrderError>>> = async {
    let url = sprintf "/api/order/%s?authKey=%s" (JS.encodeURIComponent orderId) (AuthKey.toString authKey |> JS.encodeURIComponent)
    return! tryDelete url |> handleErrors
}

let loadUserData authKey : Async<Result<ExistingUserData list, ApiError<LoadUserDataError>>> = async {
    let url = sprintf "/api/user?authKey=%s" (AuthKey.toString authKey |> JS.encodeURIComponent)
    return! tryGet url |> handleErrors
}

let updateUser authKey userId (user: UserData) : Async<Result<unit, ApiError<SaveUserError>>> = async {
    let url = sprintf "/api/user/%s?authKey=%s" userId (AuthKey.toString authKey |> JS.encodeURIComponent)
    return! tryPut url user |> handleErrors
}

let createUser authKey (user: UserData) : Async<Result<string, ApiError<SaveUserError>>> = async {
    let url = sprintf "/api/user?authKey=%s" (AuthKey.toString authKey |> JS.encodeURIComponent)
    return! tryPost url user |> handleErrors
}
