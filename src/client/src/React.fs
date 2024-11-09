module React

open Browser.Dom
open Feliz
open Fetch.Types
open MusiOrder.Models

type AuthenticationError =
    | ReaderNotAvailable
    | UnknownError

let private useKeyboardAuthentication setAuthKey =
    let mutable key = ""
    let mutable timeoutId = 0.
    let rec finishAuthKey () =
        setAuthKey (Ok (AuthKey key))
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
        if v.Ok then v.text() |> Promise.map (AuthKey >> Ok >> Some)
        else Promise.lift (Some (Error ReaderNotAvailable)) // TODO not all errors are reader errors
    )
    |> Promise.catch (fun _e ->
        if abortController.signal.aborted then None
        else (Some (Error UnknownError))
    )
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
