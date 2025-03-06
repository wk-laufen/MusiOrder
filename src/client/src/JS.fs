module JS

open Fable.Core

let [<Import("default","moment")>] moment: System.DateTimeOffset -> obj = jsNative

// see https://stackoverflow.com/a/51411310/1293659
[<Emit("new Intl.NumberFormat(navigator.language).formatToParts(1.1).find(part => part.type === 'decimal').value")>]
let decimalPoint : string = jsNative

let tryParseDecimalInput (v: string) =
    match v.Replace(decimalPoint, ".") |> System.Decimal.TryParse with
    | (true, v) -> Some v
    | (false, _) -> None
