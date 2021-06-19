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

type AuthKey = AuthKey of string
module AuthKey =
    let encode : Encoder<_> = fun (AuthKey v) -> Encode.string v
    let decoder : Decoder<_> = Decode.string |> Decode.map AuthKey
    let toString (AuthKey authKey) = authKey

type ProductId = ProductId of string
module ProductId =
    let encode : Encoder<_> = fun (ProductId v) -> Encode.string v
    let decoder : Decoder<_> = Decode.string |> Decode.map ProductId

type Product = {
    Id: ProductId
    Name: string
    Price: decimal
}

type ProductGroup = {
    Name: string
    Products: Product list
}

type OrderEntry = {
    ProductId: ProductId
    Amount: PositiveInteger
}

type Order = {
    AuthKey: AuthKey
    Entries: OrderEntry list
}

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

type UserInfo = {
    Id: string
    FirstName: string
    LastName: string
    AuthKey: AuthKey option
    LatestOrderTimestamp: DateTimeOffset option
    Balance: decimal
}

type Payment = {
    AuthKey: AuthKey
    UserId: string
    Amount: decimal
}

type OrderInfo = {
    Id: string
    FirstName: string
    LastName: string
    ArticleName: string
    Amount: int
    PricePerUnit: decimal
    Timestamp: DateTimeOffset
}

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

type ExistingUserData = {
    Id: string
    FirstName: string
    LastName: string
    AuthKey: AuthKey option
    Role: UserRole
}

type NewUserData = {
    FirstName: string
    LastName: string
    AuthKey: AuthKey option
    Role: UserRole
}
