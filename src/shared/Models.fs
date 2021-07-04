namespace MusiOrder.Models

open System
#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

type PositiveInteger = private PositiveInteger of int

module PositiveInteger =
    let tryCreate = function
        | v when v > 0 -> Some (PositiveInteger v)
        | _ -> None
    let value (PositiveInteger v) = v
    let encode : Encoder<_> = fun (PositiveInteger v) -> Encode.int v
    let decoder : Decoder<_> =
        Decode.int
        |> Decode.andThen (tryCreate >> function
            | Some v -> Decode.succeed v
            | None -> Decode.fail "Must be positive"
        )

type NotEmptyString =
    private NotEmptyString of string
    with member x.Value = let (NotEmptyString v) = x in v
module NotEmptyString =
    let tryCreate v =
        if String.IsNullOrWhiteSpace v then None
        else Some (NotEmptyString v)
    let encode : Encoder<_> = fun (NotEmptyString v) -> Encode.string v
    let decoder : Decoder<_> =
        Decode.string
        |> Decode.andThen (tryCreate >> function
            | Some v -> Decode.succeed v
            | None -> Decode.fail "Must be non-empty"
        )

type AuthKey = AuthKey of string
module AuthKey =
    let encode : Encoder<_> = fun (AuthKey v) -> Encode.string v
    let decoder : Decoder<_> = Decode.string |> Decode.map AuthKey
    let toString (AuthKey authKey) = authKey

type ProductId = ProductId of string
module ProductId =
    let encode : Encoder<_> = fun (ProductId v) -> Encode.string v
    let decoder : Decoder<_> = Decode.string |> Decode.map ProductId

type UserId = UserId of string
module UserId =
    let encode : Encoder<_> = fun (UserId v) -> Encode.string v
    let decoder : Decoder<_> = Decode.string |> Decode.map UserId

module Order =
    type Product = {
        Id: ProductId
        Name: string
        Price: decimal
    }

    type ProductGroup = {
        Name: string
        Products: Product list
    }

    type UserInfo = {
        FirstName: string
        LastName: string
        AuthKey: AuthKey option
        Balance: decimal
    }

    type NewOrderEntry = {
        ProductId: ProductId
        Amount: PositiveInteger
    }

    type NewOrder = NewOrderEntry list

    type HistoricOrder = {
        Timestamp: DateTimeOffset
        ProductName: string
        Amount: int
    }

    type OrderSummary = {
        ClientFullName: string
        Balance: decimal
        LatestOrders: HistoricOrder list
    }

    type LoadUsersError =
        | InvalidAuthKey
        | NotAuthorized

    type LoadOrderSummaryError =
        | InvalidAuthKey

    type OrderEntryError =
        | ProductNotFound

    type AddOrderError =
        | InvalidAuthKey
        | OrderEntryErrors of ProductId * OrderEntryError list

module UserPaymentAdministration =
    type UserInfo = {
        Id: UserId
        FirstName: string
        LastName: string
        LatestOrderTimestamp: DateTimeOffset option
        Balance: decimal
    }

    type Payment = {
        Amount: decimal
    }

    type LoadUsersError =
        | InvalidAuthKey
        | NotAuthorized

    type AddPaymentError =
        | InvalidAuthKey
        | NotAuthorized

module UserAdministration =
    type UserRole = Admin | User
    module UserRole =
        let tryParse v =
            if String.Equals(v, "admin", StringComparison.InvariantCultureIgnoreCase) then Some Admin
            elif String.Equals(v, "user", StringComparison.InvariantCultureIgnoreCase) then Some User
            else None
        let toString = function
            | Admin -> "admin"
            | User -> "user"
        let label = function
            | Admin -> "Administrator"
            | User -> "Benutzer"

    type UserData = {
        FirstName: NotEmptyString
        LastName: NotEmptyString
        AuthKey: AuthKey option
        Role: UserRole
    }

    type ExistingUserData = {
        Id: UserId
        Data: UserData
    }
    module ExistingUserData =
        let create userId userData =
            {
                Id = userId
                Data = userData
            }

    type LoadExistingUsersError =
        | InvalidAuthKey
        | NotAuthorized

    type SaveUserError =
        | DowngradeSelfNotAllowed
        | KeyCodeTaken of string option
        | InvalidAuthKey
        | NotAuthorized

module OrderAdministration =
    type OrderInfo = {
        Id: string
        FirstName: string
        LastName: string
        ProductName: string
        Amount: int
        PricePerUnit: decimal
        Timestamp: DateTimeOffset
    }

    type LoadOrderInfoError =
        | InvalidAuthKey
        | NotAuthorized

    type DeleteOrderError =
        | InvalidAuthKey
        | NotAuthorized

module Json =
    let coders =
        Extra.empty
        |> Extra.withCustom ProductId.encode ProductId.decoder
        |> Extra.withCustom UserId.encode UserId.decoder
        |> Extra.withCustom AuthKey.encode AuthKey.decoder
        |> Extra.withCustom PositiveInteger.encode PositiveInteger.decoder
        |> Extra.withCustom NotEmptyString.encode NotEmptyString.decoder
        |> Extra.withDecimal
