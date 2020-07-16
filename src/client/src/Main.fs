module Main

open Browser.Dom
open Fable.Core.JsInterop
open Fable.FontAwesome
open Feliz
open Feliz.Bulma
open global.JS
open MusiOrder.Models

importAll "../styles/main.scss"

let nav =
    Bulma.navbar [
        color.hasBackgroundPrimary
        prop.className "navigation"
        prop.children [
            Bulma.navbarBrand.div [
                prop.style [
                    style.alignItems.baseline
                ]
                prop.children [
                    Bulma.navbarItem.div [
                        Bulma.title.h1 [
                            prop.text "MusiOrder"
                        ]
                    ]
                    Bulma.navbarItem.div [
                        Bulma.icon [
                            prop.onClick (fun _ -> window.location.reload ())
                            prop.children [
                                Fa.i [ Fa.Solid.Beer; Fa.Size Fa.Fa2x ] []
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let main =
    Html.div [
        prop.className "main"
        prop.children [
            nav
            OrderForm.view
                {|
                    UserButtons = [ OrderSummary.view () ]
                    AdminButtons = [ Administration.view () ]
                |}
        ]
    ]

ReactDOM.render(main, document.getElementById "app")
