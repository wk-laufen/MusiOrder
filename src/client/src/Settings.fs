module Settings

open Browser.Dom
open Feliz

[<Literal>]
let private TimeoutStorageKey = "timeout-notification-delay-seconds"

[<Literal>]
let private DefaultTimeoutSeconds = 300

[<Literal>]
let private MinTimeoutSeconds = 5

let private parseSeconds (text: string) =
    match System.Int32.TryParse text with
    | true, n when n >= MinTimeoutSeconds -> Some n
    | _ -> None

[<ReactComponent>]
let Settings _authKey _setAuthKeyInvalid (_setMenuItems: ReactElement list -> ReactElement) =
    let storedValue = window.localStorage.getItem TimeoutStorageKey |> Option.ofObj
    let (enabled, setEnabled) = React.useState storedValue.IsSome

    let (secondsText, setSecondsText) =
        storedValue
        |> Option.defaultValue (string DefaultTimeoutSeconds)
        |> React.useState

    let parsedSeconds = parseSeconds secondsText

    let persist isEnabled parsed =
        match isEnabled, parsed with
        | true, Some secs -> window.localStorage.setItem (TimeoutStorageKey, string secs)
        | true, None -> () // invalid input -> don't persist
        | false, _ -> window.localStorage.removeItem TimeoutStorageKey

    Html.div [
        prop.className "container flex flex-col gap-4"
        prop.children [
            Html.label [
                prop.className "inline-flex items-center gap-2"
                prop.children [
                    Html.input [
                        prop.type' "checkbox"
                        prop.isChecked enabled
                        prop.onChange (fun (isChecked: bool) ->
                            setEnabled isChecked
                            persist isChecked parsedSeconds)
                    ]
                    Html.span "Bildschirmschoner-Timeout:"
                    Html.input [
                        prop.type' "number"
                        prop.min MinTimeoutSeconds
                        prop.disabled (not enabled)
                        prop.classes [
                            "w-24"
                            if enabled && parsedSeconds.IsNone then
                                "border-musi-red"
                        ]
                        prop.value secondsText
                        prop.onChange (fun (value: string) ->
                            setSecondsText value
                            persist enabled (parseSeconds value))
                    ]
                    Html.span "Sekunden"
                ]
            ]
        ]
    ]
