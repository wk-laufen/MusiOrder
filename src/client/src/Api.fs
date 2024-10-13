module Api

open Fable.Core
open Fetch
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

let inline tryDecodeError (response: Response) = async {
    let! e = response.text() |> Async.AwaitPromise
    match Decode.Auto.fromString(e, caseStrategy = CamelCase, extra = Json.coders) with
    | Ok e -> return Error (ExpectedError e)
    | Error e -> return Error (UnexpectedError e)
}

let inline handleErrors request = async {
    match! request with
    | Ok result -> return Ok result
    | Error (FetchFailed response) -> return! tryDecodeError response
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

    let loadUserData authKey : Async<Result<ExistingUser list, ApiError<LoadExistingUsersError>>> = async {
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

module ProductAdministration =
    open MusiOrder.Models.ProductAdministration

    let loadProductData authKey : Async<Result<ExistingProductGroup list, ApiError<LoadExistingProductsError>>> = async {
        let url = sprintf "/api/administration/product/products?authKey=%s" (AuthKey.toString authKey |> JS.encodeURIComponent)
        return! tryGet url |> handleErrors
    }

    let createProductGroup authKey (productGroup: ProductGroupData) : Async<Result<ProductGroupId, ApiError<SaveProductGroupError>>> = async {
        let url = sprintf "/api/administration/product/groups?authKey=%s" (AuthKey.toString authKey |> JS.encodeURIComponent)
        return! tryPost url productGroup |> handleErrors
    }

    let updateProductGroup authKey (ProductGroupId productGroupId) (productGroup: ProductGroupData) : Async<Result<unit, ApiError<SaveProductGroupError>>> = async {
        let url = sprintf "/api/administration/product/groups/%s?authKey=%s" (JS.encodeURIComponent productGroupId) (AuthKey.toString authKey |> JS.encodeURIComponent)
        return! tryPut url productGroup |> handleErrors
    }

    let deleteProductGroup authKey (ProductGroupId productGroupId) : Async<Result<unit, ApiError<DeleteProductGroupError>>> = async {
        let url = sprintf "/api/administration/product/groups/%s?authKey=%s" (JS.encodeURIComponent productGroupId) (AuthKey.toString authKey |> JS.encodeURIComponent)
        return! tryDelete url |> handleErrors
    }

    let moveUpProductGroup authKey (ProductGroupId productGroupId) : Async<Result<unit, ApiError<MoveProductGroupError>>> = async {
        let url = sprintf "/api/administration/product/groups/%s/move-up?authKey=%s" (JS.encodeURIComponent productGroupId) (AuthKey.toString authKey |> JS.encodeURIComponent)
        return! tryPost url () |> handleErrors
    }

    let moveDownProductGroup authKey (ProductGroupId productGroupId) : Async<Result<unit, ApiError<MoveProductGroupError>>> = async {
        let url = sprintf "/api/administration/product/groups/%s/move-down?authKey=%s" (JS.encodeURIComponent productGroupId) (AuthKey.toString authKey |> JS.encodeURIComponent)
        return! tryPost url () |> handleErrors
    }

    let createProduct authKey (ProductGroupId productGroupId,  product: ProductData) : Async<Result<ProductId, ApiError<SaveProductError>>> = async {
        let url = sprintf "/api/administration/product/groups/%s/products?authKey=%s" productGroupId (AuthKey.toString authKey |> JS.encodeURIComponent)
        return! tryPost url product |> handleErrors
    }

    let updateProduct authKey (ProductId productId) (product: ProductData) : Async<Result<unit, ApiError<SaveProductError>>> = async {
        let url = sprintf "/api/administration/product/products/%s?authKey=%s" (JS.encodeURIComponent productId) (AuthKey.toString authKey |> JS.encodeURIComponent)
        return! tryPut url product |> handleErrors
    }

    let moveUpProduct authKey (ProductId productId) : Async<Result<unit, ApiError<MoveProductError>>> = async {
        let url = sprintf "/api/administration/product/products/%s/move-up?authKey=%s" (JS.encodeURIComponent productId) (AuthKey.toString authKey |> JS.encodeURIComponent)
        return! tryPost url () |> handleErrors
    }

    let moveDownProduct authKey (ProductId productId) : Async<Result<unit, ApiError<MoveProductError>>> = async {
        let url = sprintf "/api/administration/product/products/%s/move-down?authKey=%s" (JS.encodeURIComponent productId) (AuthKey.toString authKey |> JS.encodeURIComponent)
        return! tryPost url () |> handleErrors
    }

    let deleteProduct authKey (ProductId productId) : Async<Result<unit, ApiError<DeleteProductError>>> = async {
        let url = sprintf "/api/administration/product/products/%s?authKey=%s" (JS.encodeURIComponent productId) (AuthKey.toString authKey |> JS.encodeURIComponent)
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
