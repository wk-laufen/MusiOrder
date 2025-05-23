module React

open Browser.Dom
open Feliz
open Fetch.Types
open MusiOrder.Models

type AuthenticationError =
    | ReaderNotAvailable

let private useKeyboardAuthentication setAuthKey =
    let mutable key = ""
    let mutable timeoutId = 0.
    let rec finishAuthKey () =
        setAuthKey (Ok (NFCAuthKey key))
        key <- ""
    and listener (e: Browser.Types.Event) =
        e.preventDefault()
        e.stopPropagation()
        window.clearTimeout timeoutId
        let newKey = (e :?> Browser.Types.KeyboardEvent).key
        if newKey = "Enter" then finishAuthKey ()
        else
            key <- key + newKey
            timeoutId <- window.setTimeout (finishAuthKey, 500)
    window.addEventListener("keydown", listener)
    React.createDisposable (fun () -> window.removeEventListener("keydown", listener))

let private useRemoteAuthentication setAuthKey =
    let abortController = Fetch.newAbortController()
    Fetch.fetchUnsafe "http://localhost:8080/nfc-reader/card-id" [ Signal abortController.signal ]
    |> Promise.bind (fun v ->
        if v.Ok then v.text() |> Promise.map (NFCAuthKey >> Ok >> Some)
        elif v.Status >= 400 && v.Status < 500 then Promise.lift (Some (Ok (NFCAuthKey "")))
        else Promise.lift (Some (Error ReaderNotAvailable))
    )
    |> Promise.catch (fun _e -> None) // request cancelled or server not running -> ignore
    |> Promise.iter (function
        | Some result -> setAuthKey result
        | None -> ()
    )
    React.createDisposable (fun () -> abortController.abort())

let useAuthentication isActive setAuthKey =
    React.useEffect (
        fun () ->
            if isActive then
                React.createDisposable(
                    useKeyboardAuthentication setAuthKey,
                    useRemoteAuthentication setAuthKey
                )
            else
                React.createDisposable id
        ,
        [| box isActive |]
    )
