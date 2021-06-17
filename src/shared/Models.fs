namespace MusiOrder.Models

open System
#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

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

type OrderEntry = {
    ProductId: ProductId
    Amount: PositiveInteger
}

type AuthKey = AuthKey of string
module AuthKey =
    let encode : Encoder<_> = fun (AuthKey v) -> Encode.string v
    let decoder : Decoder<_> = Decode.string |> Decode.map AuthKey
    let toString (AuthKey authKey) = authKey

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
    AuthKey: AuthKey
    LatestOrderTimestamp: DateTimeOffset option
    Balance: decimal
}

type Payment = {
    AuthKey: AuthKey
    UserId: string
    Amount: decimal
}
