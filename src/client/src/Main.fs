module Main

open Browser.Dom
open Elmish
open Fable.Core.JsInterop
open Fable.FontAwesome
open Feliz
open Feliz.Router
open Feliz.UseElmish
open global.JS
open MusiOrder.Models

importAll "../styles/main.scss"
importAll "../styles/main.css"

type Model = {
    CurrentUrl: string list
}

type Msg = UrlChanged of string list

let init = { CurrentUrl = Router.currentUrl() }, Cmd.none

let update msg state =
    match msg with
    | UrlChanged segments -> { state with CurrentUrl = segments }, Cmd.none

let nav classes (title: string) =
    Html.div [
        prop.className $"%s{classes}"
        prop.children [
            Html.h1 [
                prop.className "px-4 py-2 text-5xl"
                prop.children [
                    Html.span [ prop.text title ]
                    Html.a [
                        prop.className "cursor-pointer"
                        prop.href "/"
                        prop.text "🍻"
                    ]
                ]
            ]
        ]
    ]

let adminPageButton =
    Html.a [
        prop.className "btn"
        prop.href (Router.format("administration"))
        prop.children [
            Html.span [
                prop.className "inline-flex items-center gap-2"
                prop.children [
                    Fa.i [ Fa.Solid.Cogs ] []
                    Html.span [ prop.text "Administration" ]
                ]
            ]
        ]
    ]

[<ReactComponent>]
let Main () =
    let (state, dispatch) = React.useElmish(init, update, [||])

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
                        OrderForm.OrderForm
                            [ OrderSummary.OrderSummary () ]
                            [ adminPageButton ]
                ]
            ]
        ]
    ]

moment?locale("de-AT")

ReactDOM.render(Main, document.getElementById "app")
