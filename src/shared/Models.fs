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

type NonNegativeDecimal = private NonNegativeDecimal of decimal

module NonNegativeDecimal =
    let tryCreate = function
        | v when v > 0m -> Some (NonNegativeDecimal v)
        | _ -> None
    let tryParse (v: string) =
        match System.Decimal.TryParse(v) with
        | (true, v) -> tryCreate v
        | (false, _) -> None
    let value (NonNegativeDecimal v) = v
    let encode : Encoder<_> = fun (NonNegativeDecimal v) -> Encode.decimal v
    let decoder : Decoder<_> =
        Decode.decimal
        |> Decode.andThen (tryCreate >> function
            | Some v -> Decode.succeed v
            | None -> Decode.fail "Must be non-negative"
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

type ProductGroupId = ProductGroupId of string
module ProductGroupId =
    let encode : Encoder<_> = fun (ProductGroupId v) -> Encode.string v
    let decoder : Decoder<_> = Decode.string |> Decode.map ProductGroupId

type ProductId = ProductId of string
module ProductId =
    let encode : Encoder<_> = fun (ProductId v) -> Encode.string v
    let decoder : Decoder<_> = Decode.string |> Decode.map ProductId

type UserId = UserId of string
module UserId =
    let encode : Encoder<_> = fun (UserId v) -> Encode.string v
    let decoder : Decoder<_> = Decode.string |> Decode.map UserId

type OrderId = OrderId of string
module OrderId =
    let encode : Encoder<_> = fun (OrderId v) -> Encode.string v
    let decoder : Decoder<_> = Decode.string |> Decode.map OrderId

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
        Id: UserId
        FirstName: string
        LastName: string
        AuthKey: AuthKey option
        Balance: decimal
    }

    type UserList = {
        Self: UserInfo option
        Others: UserInfo list
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
        | NotAuthorized
        | NoOrderSummaryUser

    type OrderEntryError =
        | ProductNotFound

    type AddOrderError =
        | InvalidAuthKey
        | NotAuthorized
        | NoOrderUser
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
    type UserRole = Admin | OrderAssistant | User
    module UserRole =
        let tryParse v =
            if String.Equals(v, "admin", StringComparison.InvariantCultureIgnoreCase) then Some Admin
            elif String.Equals(v, "order-assistant", StringComparison.InvariantCultureIgnoreCase) then Some OrderAssistant
            elif String.Equals(v, "user", StringComparison.InvariantCultureIgnoreCase) then Some User
            else None
        let toString = function
            | Admin -> "admin"
            | OrderAssistant -> "order-assistant"
            | User -> "user"
        let label = function
            | Admin -> "Wirt"
            | OrderAssistant -> "Kellner"
            | User -> "Benutzer"

    type ExistingUserData = {
        FirstName: NotEmptyString
        LastName: NotEmptyString
        AuthKey: AuthKey option
        Role: UserRole
    }

    type PatchUserData = {
        FirstName: NotEmptyString option
        LastName: NotEmptyString option
        AuthKey: AuthKey option
        SetAuthKey: bool
        Role: UserRole option
    }
    module PatchUserData =
        let empty = { FirstName = None; LastName = None; AuthKey = None; SetAuthKey = false; Role = None }

    type ExistingUser = {
        Id: UserId
        Data: ExistingUserData
    }
    module ExistingUserData =
        let create userId userData =
            {
                Id = userId
                Data = userData
            }

    type DeleteUserWarning =
        | AuthKeyPresent
        | CurrentBalanceNotZero of decimal

    type LoadExistingUsersError =
        | InvalidAuthKey
        | NotAuthorized

    type SaveUserError =
        | DowngradeSelfNotAllowed
        | RemoveKeyCodeNotAllowed
        | KeyCodeTaken of string option
        | InvalidAuthKey
        | NotAuthorized

    type DeleteUserError =
        | InvalidAuthKey
        | NotAuthorized

    type ForceDeleteUserError =
        | InvalidAuthKey
        | NotAuthorized

module ProductAdministration =
    type ProductState = Enabled | Disabled
    module ProductState =
        let tryParse = function
            | "enabled" -> Some Enabled
            | "disabled" -> Some Disabled
            | _ -> None
        let toString = function
            | Enabled -> "enabled"
            | Disabled -> "disabled"
        let label = function
            | Enabled -> "Aktiv"
            | Disabled -> "Inaktiv"

    type ProductData = {
        Name: NotEmptyString
        Price: NonNegativeDecimal
        State: ProductState
    }

    type ExistingProduct = {
        Id: ProductId
        Data: ProductData
    }

    type ProductGroupData = {
        Name: NotEmptyString
    }

    type ExistingProductGroup = {
        Id: ProductGroupId
        Data: ProductGroupData
        Products: ExistingProduct list
    }

    type LoadExistingProductsError =
        | InvalidAuthKey
        | NotAuthorized

    type SaveProductGroupError =
        | InvalidAuthKey
        | NotAuthorized

    type MoveProductGroupError =
        | InvalidAuthKey
        | NotAuthorized

    type DeleteProductGroupError =
        | InvalidAuthKey
        | NotAuthorized
        | GroupNotEmpty

    type SaveProductError =
        | InvalidAuthKey
        | NotAuthorized

    type MoveProductError =
        | InvalidAuthKey
        | NotAuthorized

    type DeleteProductError =
        | InvalidAuthKey
        | NotAuthorized

module OrderAdministration =
    type OrderInfo = {
        Id: OrderId
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

module OrderStatistics =
    type OrderInfo = {
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
        | MissingTimeRange

module DataExport =
    type ExportDatabaseError =
        | InvalidAuthKey
        | NotAuthorized

module Json =
    let coders =
        Extra.empty
        |> Extra.withCustom ProductId.encode ProductId.decoder
        |> Extra.withCustom ProductGroupId.encode ProductGroupId.decoder
        |> Extra.withCustom UserId.encode UserId.decoder
        |> Extra.withCustom OrderId.encode OrderId.decoder
        |> Extra.withCustom AuthKey.encode AuthKey.decoder
        |> Extra.withCustom PositiveInteger.encode PositiveInteger.decoder
        |> Extra.withCustom NotEmptyString.encode NotEmptyString.decoder
        |> Extra.withCustom NonNegativeDecimal.encode NonNegativeDecimal.decoder
        |> Extra.withDecimal
