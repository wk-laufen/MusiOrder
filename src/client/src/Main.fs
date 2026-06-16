module Main

open Browser.Dom
open Browser.Types
open Elmish
open Fable.Core
open Fable.Core.JsInterop
open Feliz
open Feliz.Router
open Feliz.UseElmish
open global.JS
open MusiOrder.Models

importAll "../styles/main.css"
importAll "@fortawesome/fontawesome-free/css/all.css"

importAll "simple-keyboard/build/css/index.css"

[<Import("setupKeyboards", "./on-screen-keyboard.js")>]
let setupKeyboards: unit -> unit = jsNative

[<Import("setupTimeoutNotification", "./timeout-notification.js")>]
let setupTimeoutNotification: Element -> unit = jsNative

type Model = { CurrentUrl: string list }

type Msg = UrlChanged of string list

let init = { CurrentUrl = Router.currentUrl () }, Cmd.none

let update msg state =
    match msg with
    | UrlChanged segments -> { state with CurrentUrl = segments }, Cmd.none

let nav bgClass (title: string) =
    Html.div [
        prop.className $"flex justify-between %s{bgClass}"
        prop.children [
            Html.h1 [
                prop.className "px-4 py-2 text-5xl"
                prop.children [
                    Html.span [ prop.text title ]
                    Html.a [ prop.className "cursor-pointer"; prop.href "/"; prop.text "🍻" ]
                ]
            ]
            Html.div [
                prop.className "flex items-center px-4"
                prop.children [
                    Html.div [
                        prop.id "timeout-progress"
                        prop.className "w-48"
                        prop.children [
                            Html.div [
                                prop.className $"h-4 %s{bgClass} brightness-75"
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let adminPageButton =
    Html.a [
        prop.className "btn"
        prop.href (Router.format ("administration"))
        prop.children [
            Html.span [
                prop.className "inline-flex items-center gap-2"
                prop.children [
                    Html.i [ prop.className "fas fa-cogs" ]
                    Html.span [ prop.text "Administration" ]
                ]
            ]
        ]
    ]

[<ReactComponent>]
let Main () =
    let (state, dispatch) = React.useElmish (init, update, [||])

    React.useEffectOnce (fun () -> setupTimeoutNotification (document.querySelector "#timeout-progress"))

    React.router [
        router.onUrlChanged (UrlChanged >> dispatch)

        router.children [
            Html.div [
                prop.className "flex flex-col h-full"
                prop.children [
                    match state.CurrentUrl with
                    | Administration.route :: subUrl ->
                        nav "bg-musi-red" "MusiOrder - Administration"

                        let activeTab =
                            Administration.allTabs
                            |> List.tryFind (fun v -> subUrl = [ Administration.Tab.toRoute v ])
                            |> Option.defaultValue (List.head Administration.allTabs)

                        Administration.Administration activeTab
                    | _ ->
                        nav "bg-musi-gold" "MusiOrder"
                        OrderForm.OrderForm [ OrderSummary.OrderSummary() ] [ adminPageButton ]
                ]
            ]
        ]
    ]

[<Emit("navigator.language")>]
let language: string = jsNative

moment?locale language

ReactDOM.createRoot(document.getElementById "app").render (Main())

setupKeyboards ()
