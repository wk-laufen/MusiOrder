namespace global

open FSharp.Control.Tasks.V2.ContextInsensitive

module Option =
    let bindTask fn o = task {
        match o with
        | Some v -> return! fn v
        | None -> return None
    }

module Result =
    let ofOption e = function
        | Some v -> Ok v
        | None -> Error e

    let apply r fn =
        match fn, r with
        | Ok fn, Ok r -> Ok (fn r)
        | Error fn, Ok r -> Error fn
        | Ok fn, Error r -> Error [ r ]
        | Error fn, Error r -> Error (fn @ [ r ])

module List =
    let sequence l =
        (l, Ok [])
        ||> List.foldBack (fun item state ->
            match state, item with
            | Ok x, Ok v -> Ok (v :: x)
            | Error e, Ok v -> Error e
            | Ok e, Error v -> Error [ v ]
            | Error e, Error ve -> Error (ve :: e)
        )

module DateTime =
    open System.Globalization
    let tryParseDate (v: string) =
        match System.DateTime.TryParseExact(v, [|"yyyy-MM-dd"|], CultureInfo.InvariantCulture, DateTimeStyles.None) with
        | (true, v) -> Some v
        | (false, _) -> None
