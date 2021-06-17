module Main

open Browser.Dom
open Elmish
open Fable.Core.JsInterop
open Fable.FontAwesome
open Feliz
open Feliz.Bulma
open Feliz.Router
open Feliz.UseElmish
open global.JS
open MusiOrder.Models

importAll "../styles/main.scss"

type Model = {
    CurrentUrl: string list
}

type Msg = UrlChanged of string list

let init = { CurrentUrl = Router.currentUrl() }, Cmd.none

let update msg state =
    match msg with
    | UrlChanged segments -> { state with CurrentUrl = segments }, Cmd.none

let nav bgColor (title: string) =
    Bulma.navbar [
        bgColor
        prop.className "navigation"
        prop.children [
            Bulma.navbarBrand.div [
                prop.style [
                    style.alignItems.baseline
                ]
                prop.children [
                    Bulma.navbarItem.div [
                        Bulma.title.h1 [
                            prop.text title
                        ]
                    ]
                    Bulma.navbarItem.div [
                        Bulma.icon [
                            prop.onClick (fun _ -> window.location.href <- "/")
                            prop.children [
                                Fa.i [ Fa.Solid.Beer; Fa.Size Fa.Fa2x ] []
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let adminPageButton =
    Bulma.button.a [
        prop.href (Router.format("administration"))
        prop.children [
            Bulma.icon [ Fa.i [ Fa.Solid.Cogs ] [] ]
            Html.span [ prop.text "Administration" ]
        ]
    ]

[<ReactComponent>]
let Main () =
    let (state, dispatch) = React.useElmish(init, update, [||])

    React.router [
        router.onUrlChanged (UrlChanged >> dispatch)

        router.children [
            Html.div [
                prop.className "main"
                prop.children [
                    match state.CurrentUrl with
                    | Administration.route :: subUrl ->
                        nav color.hasBackgroundDanger "MusiOrder - Administration"
                        // TODO Use `Administration.Tab.route`
                        match subUrl with
                        | [ "bestellungen" ] ->
                            Administration.Administration Administration.Orders
                        | [ "guthaben" ]
                        | _ ->
                            Administration.Administration Administration.UserPayment
                    | _ ->
                        nav color.hasBackgroundPrimary "MusiOrder"
                        OrderForm.OrderForm
                            [ OrderSummary.OrderSummary () ]
                            [ adminPageButton ]
                ]
            ]
        ]
    ]

moment?locale("de-AT")

ReactDOM.render(Main, document.getElementById "app")
