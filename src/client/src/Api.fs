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

module Order =
    open MusiOrder.Models.Order

    let loadProducts : Async<Result<ProductGroup list, string>> = async {
        let! result = tryGet "/api/order/products"
        return result |> Result.mapError Helper.message
    }

    let loadOrderSummary authKey userId : Async<Result<OrderSummary, ApiError<LoadOrderSummaryError>>> = async {
        let userIdParam =
            match userId with
            | Some (UserId userId) -> sprintf "&userId=%s" (JS.encodeURIComponent userId)
            | None -> ""
        let url = sprintf "/api/order/summary?authKey=%s%s" (AuthKey.toString authKey |> JS.encodeURIComponent) userIdParam
        return! tryGet url |> handleErrors
    }

    let loadUsers authKey : Async<Result<UserInfo list, ApiError<LoadUsersError>>> = async {
        let url = sprintf "/api/order/users?authKey=%s" (AuthKey.toString authKey |> JS.encodeURIComponent)
        return! tryGet url |> handleErrors
    }

    let sendOrder authKey userId order : Async<Result<unit, ApiError<AddOrderError list>>> = async {
        let body =
            order
            |> Map.toList
            |> List.choose (fun (productId, amount) ->
                PositiveInteger.tryCreate amount
                |> Option.map (fun amount -> { ProductId = productId; Amount = amount })
            )
        let userIdParam =
            match userId with
            | Some (UserId userId) -> sprintf "&userId=%s" (JS.encodeURIComponent userId)
            | None -> ""
        let url = sprintf "/api/order?authKey=%s%s" (AuthKey.toString authKey |> JS.encodeURIComponent) userIdParam
        return! tryPost url body |> handleErrors
    }

module UserPaymentAdministration =
    open MusiOrder.Models.UserPaymentAdministration

    let loadUsers authKey : Async<Result<UserInfo list, ApiError<LoadUsersError>>> = async {
        let url = sprintf "/api/administration/user-payment/users?authKey=%s" (AuthKey.toString authKey |> JS.encodeURIComponent)
        return! tryGet url |> handleErrors
    }

    let addPayment authKey (UserId userId) (payment: Payment) : Async<Result<decimal, ApiError<AddPaymentError>>> = async {
        let url = sprintf "/api/administration/user-payment/%s?authKey=%s" (JS.encodeURIComponent userId) (AuthKey.toString authKey |> JS.encodeURIComponent)
        return! tryPost url payment |> handleErrors
    }

module UserAdministration =
    open MusiOrder.Models.UserAdministration

    let loadUserData authKey : Async<Result<ExistingUserData list, ApiError<LoadExistingUsersError>>> = async {
        let url = sprintf "/api/administration/user/users?authKey=%s" (AuthKey.toString authKey |> JS.encodeURIComponent)
        return! tryGet url |> handleErrors
    }

    let createUser authKey (user: UserData) : Async<Result<UserId, ApiError<SaveUserError>>> = async {
        let url = sprintf "/api/administration/user/users?authKey=%s" (AuthKey.toString authKey |> JS.encodeURIComponent)
        return! tryPost url user |> handleErrors
    }

    let updateUser authKey (UserId userId) (user: UserData) : Async<Result<unit, ApiError<SaveUserError>>> = async {
        let url = sprintf "/api/administration/user/users/%s?authKey=%s" (JS.encodeURIComponent userId) (AuthKey.toString authKey |> JS.encodeURIComponent)
        return! tryPut url user |> handleErrors
    }

    let deleteUser authKey (UserId userId) : Async<Result<DeleteUserWarning list, ApiError<DeleteUserError>>> = async {
        let url = sprintf "/api/administration/user/users/%s?authKey=%s" (JS.encodeURIComponent userId) (AuthKey.toString authKey |> JS.encodeURIComponent)
        return! tryDelete url |> handleErrors
    }

    let forceDeleteUser authKey (UserId userId) : Async<Result<unit, ApiError<ForceDeleteUserError>>> = async {
        let url = sprintf "/api/administration/user/users/%s?authKey=%s&force=true" (JS.encodeURIComponent userId) (AuthKey.toString authKey |> JS.encodeURIComponent)
        return! tryDelete url |> handleErrors
    }

module OrderAdministration =
    open MusiOrder.Models.OrderAdministration

    let loadOrderInfo authKey : Async<Result<OrderInfo list, ApiError<LoadOrderInfoError>>> = async {
        let url = sprintf "/api/administration/order/orders?authKey=%s" (AuthKey.toString authKey |> JS.encodeURIComponent)
        return! tryGet url |> handleErrors
    }

    let deleteOrder authKey (OrderId orderId) : Async<Result<unit, ApiError<DeleteOrderError>>> = async {
        let url = sprintf "/api/administration/order/orders/%s?authKey=%s" (JS.encodeURIComponent orderId) (AuthKey.toString authKey |> JS.encodeURIComponent)
        return! tryDelete url |> handleErrors
    }
