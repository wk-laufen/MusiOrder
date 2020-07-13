module React

open Browser.Dom
open Feliz
open MusiOrder.Models

let useAuthentication isActive setAuthKey =
    React.useEffect (
        fun () ->
            if isActive then
                let mutable key = ""
                let mutable timeoutId = 0.
                let rec finishAuthKey () =
                    setAuthKey (AuthKey key)
                    key <- ""
                and listener (e: Browser.Types.Event) =
                    window.clearTimeout timeoutId
                    let newKey = (e :?> Browser.Types.KeyboardEvent).key
                    if newKey = "Enter" then finishAuthKey ()
                    else
                        key <- key + newKey
                        timeoutId <- window.setTimeout (finishAuthKey, 500)
                window.addEventListener("keydown", listener)
                React.createDisposable (fun () -> window.removeEventListener("keydown", listener))
            else
                React.createDisposable id
        ,
        [| box isActive |]
    )
