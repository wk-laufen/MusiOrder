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

let private queryParam name = Option.map (fun v -> (name, v))
let private authKeyQueryParam authKey =
    authKey
    |> Option.map AuthKey.toString
    |> queryParam "authKey"

let private queryString =
    List.choose id
    >> List.map (fun (key, value) -> $"%s{key}=%s{JS.encodeURIComponent value}")
    >> String.concat "&"
    >> function
    | "" -> ""
    | v -> $"?%s{v}"

module Order =
    open MusiOrder.Models.Order

    let loadProducts : Async<Result<ProductGroup list, string>> = async {
        let! result = tryGet "/api/order/products"
        return result |> Result.mapError Helper.message
    }

    let loadOrderSummary authKey userId : Async<Result<OrderSummary, ApiError<LoadOrderSummaryError>>> = async {
        let query = queryString [
            userId |> Option.map (fun (UserId userId) -> userId) |> queryParam "userId"
            authKeyQueryParam authKey
        ]
        let url = $"/api/order/summary%s{query}"
        return! tryGet url |> handleErrors
    }

    let loadUsers authKey : Async<Result<UserInfo list, ApiError<LoadUsersError>>> = async {
        let query = queryString [
            authKeyQueryParam authKey
        ]
        let url = $"/api/order/users%s{query}"
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
        let query = queryString [
            userId |> Option.map (fun (UserId userId) -> userId) |> queryParam "userId"
            authKeyQueryParam authKey
        ]
        let url = $"/api/order%s{query}"
        return! tryPost url body |> handleErrors
    }

module UserPaymentAdministration =
    open MusiOrder.Models.UserPaymentAdministration

    let loadUsers authKey : Async<Result<UserInfo list, ApiError<LoadUsersError>>> = async {
        let query = queryString [
            authKeyQueryParam authKey
        ]
        let url = $"/api/administration/user-payment/users%s{query}"
        return! tryGet url |> handleErrors
    }

    let addPayment authKey (UserId userId) (payment: Payment) : Async<Result<decimal, ApiError<AddPaymentError>>> = async {
        let query = queryString [
            authKeyQueryParam authKey
        ]
        let url = $"/api/administration/user-payment/%s{JS.encodeURIComponent userId}%s{query}"
        return! tryPost url payment |> handleErrors
    }

module UserAdministration =
    open MusiOrder.Models.UserAdministration

    let loadUserData authKey : Async<Result<ExistingUser list, ApiError<LoadExistingUsersError>>> = async {
        let query = queryString [
            authKeyQueryParam authKey
        ]
        let url = $"/api/administration/user/users%s{query}"
        return! tryGet url |> handleErrors
    }

    let createUser authKey (user: UserData) : Async<Result<UserId, ApiError<SaveUserError>>> = async {
        let query = queryString [
            authKeyQueryParam authKey
        ]
        let url = $"/api/administration/user/users%s{query}"
        return! tryPost url user |> handleErrors
    }

    let updateUser authKey (UserId userId) (user: UserData) : Async<Result<unit, ApiError<SaveUserError>>> = async {
        let query = queryString [
            authKeyQueryParam authKey
        ]
        let url = $"/api/administration/user/users/%s{JS.encodeURIComponent userId}%s{query}"
        return! tryPut url user |> handleErrors
    }

    let deleteUser authKey (UserId userId) : Async<Result<DeleteUserWarning list, ApiError<DeleteUserError>>> = async {
        let query = queryString [
            authKeyQueryParam authKey
        ]
        let url = $"/api/administration/user/users/%s{JS.encodeURIComponent userId}%s{query}"
        return! tryDelete url |> handleErrors
    }

    let forceDeleteUser authKey (UserId userId) : Async<Result<unit, ApiError<ForceDeleteUserError>>> = async {
        let query = queryString [
            Some ("force", "true")
            authKeyQueryParam authKey
        ]
        let url = $"/api/administration/user/users/%s{JS.encodeURIComponent userId}%s{query}"
        return! tryDelete url |> handleErrors
    }

module ProductAdministration =
    open MusiOrder.Models.ProductAdministration

    let loadProductData authKey : Async<Result<ExistingProductGroup list, ApiError<LoadExistingProductsError>>> = async {
        let query = queryString [
            authKeyQueryParam authKey
        ]
        let url = $"/api/administration/product/products%s{query}"
        return! tryGet url |> handleErrors
    }

    let createProductGroup authKey (productGroup: ProductGroupData) : Async<Result<ProductGroupId, ApiError<SaveProductGroupError>>> = async {
        let query = queryString [
            authKeyQueryParam authKey
        ]
        let url = $"/api/administration/product/groups%s{query}"
        return! tryPost url productGroup |> handleErrors
    }

    let updateProductGroup authKey (ProductGroupId productGroupId) (productGroup: ProductGroupData) : Async<Result<unit, ApiError<SaveProductGroupError>>> = async {
        let query = queryString [
            authKeyQueryParam authKey
        ]
        let url = $"/api/administration/product/groups/%s{JS.encodeURIComponent productGroupId}%s{query}"
        return! tryPut url productGroup |> handleErrors
    }

    let deleteProductGroup authKey (ProductGroupId productGroupId) : Async<Result<unit, ApiError<DeleteProductGroupError>>> = async {
        let query = queryString [
            authKeyQueryParam authKey
        ]
        let url = $"/api/administration/product/groups/%s{JS.encodeURIComponent productGroupId}%s{query}"
        return! tryDelete url |> handleErrors
    }

    let moveUpProductGroup authKey (ProductGroupId productGroupId) : Async<Result<unit, ApiError<MoveProductGroupError>>> = async {
        let query = queryString [
            authKeyQueryParam authKey
        ]
        let url = $"/api/administration/product/groups/%s{JS.encodeURIComponent productGroupId}/move-up%s{query}"
        return! tryPost url () |> handleErrors
    }

    let moveDownProductGroup authKey (ProductGroupId productGroupId) : Async<Result<unit, ApiError<MoveProductGroupError>>> = async {
        let query = queryString [
            authKeyQueryParam authKey
        ]
        let url = $"/api/administration/product/groups/%s{JS.encodeURIComponent productGroupId}/move-down%s{query}"
        return! tryPost url () |> handleErrors
    }

    let createProduct authKey (ProductGroupId productGroupId,  product: ProductData) : Async<Result<ProductId, ApiError<SaveProductError>>> = async {
        let query = queryString [
            authKeyQueryParam authKey
        ]
        let url = $"/api/administration/product/groups/%s{JS.encodeURIComponent productGroupId}/products%s{query}"
        return! tryPost url product |> handleErrors
    }

    let updateProduct authKey (ProductId productId) (product: ProductData) : Async<Result<unit, ApiError<SaveProductError>>> = async {
        let query = queryString [
            authKeyQueryParam authKey
        ]
        let url = $"/api/administration/product/products/%s{JS.encodeURIComponent productId}%s{query}"
        return! tryPut url product |> handleErrors
    }

    let moveUpProduct authKey (ProductId productId) : Async<Result<unit, ApiError<MoveProductError>>> = async {
        let query = queryString [
            authKeyQueryParam authKey
        ]
        let url = $"/api/administration/product/products/%s{JS.encodeURIComponent productId}/move-up%s{query}"
        return! tryPost url () |> handleErrors
    }

    let moveDownProduct authKey (ProductId productId) : Async<Result<unit, ApiError<MoveProductError>>> = async {
        let query = queryString [
            authKeyQueryParam authKey
        ]
        let url = $"/api/administration/product/products/%s{JS.encodeURIComponent productId}/move-down%s{query}"
        return! tryPost url () |> handleErrors
    }

    let deleteProduct authKey (ProductId productId) : Async<Result<unit, ApiError<DeleteProductError>>> = async {
        let query = queryString [
            authKeyQueryParam authKey
        ]
        let url = $"/api/administration/product/products/%s{JS.encodeURIComponent productId}%s{query}"
        return! tryDelete url |> handleErrors
    }

module OrderAdministration =
    open MusiOrder.Models.OrderAdministration

    let loadOrderInfo authKey : Async<Result<OrderInfo list, ApiError<LoadOrderInfoError>>> = async {
        let query = queryString [
            authKeyQueryParam authKey
        ]
        let url = $"/api/administration/order/orders%s{query}"
        return! tryGet url |> handleErrors
    }

    let deleteOrder authKey (OrderId orderId) : Async<Result<unit, ApiError<DeleteOrderError>>> = async {
        let query = queryString [
            authKeyQueryParam authKey
        ]
        let url = $"/api/administration/order/orders/%s{JS.encodeURIComponent orderId}%s{query}"
        return! tryDelete url |> handleErrors
    }

module DataExport =
    open MusiOrder.Models.DataExport

    let exportDatabase authKey : Async<Result<Browser.Types.Blob, ApiError<ExportDatabaseError>>> = async {
        let url = sprintf "/api/administration/data-export/export-db?authKey=%s" (AuthKey.toString authKey |> JS.encodeURIComponent)
        let! response = fetchUnsafe url [ Method HttpMethod.GET ] |> Async.AwaitPromise
        if response.Ok then
            let! data = response.blob() |> Async.AwaitPromise
            return Ok data
        else
            return! tryDecodeError response
    }
