namespace MusiOrder.Models

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
    Price: float
}

type ProductGroup = {
    Name: string
    Products: Product list
}

type OrderEntry = {
    ProductId: ProductId
    Amount: int
}

type AuthKey = AuthKey of string
module AuthKey =
    let encode : Encoder<_> = fun (AuthKey v) -> Encode.string v
    let decoder : Decoder<_> = Decode.string |> Decode.map AuthKey

type Order = {
    AuthKey: AuthKey
    Entries: OrderEntry list
}
