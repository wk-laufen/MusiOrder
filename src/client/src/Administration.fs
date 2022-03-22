module Administration

open Elmish
open Fable.Core.JsInterop
open Fable.FontAwesome
open Fable.React
open Feliz
open Feliz.Bulma
open Feliz.Bulma.Operators
open Feliz.UseElmish
open Feliz.Router
open global.JS
open MusiOrder.Models

[<Literal>]
let route = "administration"

type Tab =
    | UserPayment
    | Orders
    | Users
    | Products
module Tab =
    let title = function
        | UserPayment -> "Guthaben"
        | Orders -> "Bestellungen"
        | Users -> "Benutzer"
        | Products -> "Artikel"
    let toRoute = function
        | UserPayment -> "guthaben"
        | Orders -> "bestellungen"
        | Users -> "benutzer"
        | Products -> "artikel"
let allTabs = [ UserPayment; Orders; Users; Products ]

type AuthKeyState =
    | NoAuthKeyProvided
    | AuthKeyProvided of AuthKey
    | InvalidAuthKeyProvided

type Model = {
    ActiveTab: Tab
    AuthKey: AuthKeyState
}

type Msg =
    | Show
    | Close

let init authKey activeTab =
    let state = {
        ActiveTab = activeTab
        AuthKey = authKey
    }
    (state, Cmd.none)

let update msg (state: Model) =
    match msg with
    | Show -> { state with AuthKey = NoAuthKeyProvided }, Cmd.none
    | Close -> { state with AuthKey = NoAuthKeyProvided }, Cmd.none

[<ReactComponent>]
let Administration activeTab =
    let (authKey, setAuthKey) = React.useState(NoAuthKeyProvided)
    let (state, dispatch) = React.useElmish(init authKey activeTab, update, [| authKey :> obj; activeTab :> obj |])
    let acceptsAuthKey =
        match state.AuthKey with
        | NoAuthKeyProvided
        | InvalidAuthKeyProvided -> true
        | AuthKeyProvided _ -> false
    React.useAuthentication acceptsAuthKey (AuthKeyProvided >> setAuthKey)

    let tabMenuContainerRef = React.useRef(None)
    let setTabMenuItems (content: ReactElement list) =
        match tabMenuContainerRef.current with
        | Some node -> ReactDOM.createPortal(!!content, node)
        | None -> nothing

    let abortButton =
        Bulma.button.a [
            spacing.mt6
            prop.href (Router.format(""))
            prop.text "Abbrechen"
        ]

    React.fragment [
        match state.AuthKey with
        | NoAuthKeyProvided ->
            Bulma.section [
                text.hasTextCentered
                prop.children [
                    View.authForm
                    abortButton
                ]
            ]
        | InvalidAuthKeyProvided ->
            Bulma.section [
                text.hasTextCentered
                ++ color.hasTextDanger
                ++ spacing.px2
                prop.children [
                    Bulma.container [
                        Fa.stack [ Fa.Stack.Size Fa.Fa4x ] [
                            Fa.i [ Fa.Solid.Key; Fa.Stack1x ] []
                            Fa.i [ Fa.Solid.Ban; Fa.Stack2x ] []
                        ]
                        Bulma.title.p [
                            color.hasTextDanger
                            prop.children [
                                Html.text "Schlüssel ist nicht authorisiert."
                                Html.br []
                                Html.text "Versuche es nochmal mit einem Administrator-Schlüssel."
                            ]
                        ]
                    ]
                    abortButton
                ]
            ]
        | AuthKeyProvided _ ->
            Bulma.section [
                prop.className "main-content"
                prop.children [
                    Bulma.tabs [
                        Html.ul [
                            for tab in allTabs ->
                                Bulma.tab [
                                    if tab = activeTab then Bulma.tab.isActive
                                    prop.children [
                                        Html.a [
                                            prop.text (Tab.title tab)
                                            prop.href (Router.format(route, Tab.toRoute tab))
                                        ]
                                    ]
                                ]
                        ]
                    ]
                    let authKeyOpt =
                        match authKey with
                        | NoAuthKeyProvided
                        | InvalidAuthKeyProvided -> None
                        | AuthKeyProvided authKey -> Some authKey
                    match activeTab with
                    | UserPayment -> UserPaymentAdministration.UserPaymentAdministration authKeyOpt (fun () -> setAuthKey InvalidAuthKeyProvided) setTabMenuItems
                    | Orders -> OrderAdministration.OrderAdministration authKeyOpt (fun () -> setAuthKey InvalidAuthKeyProvided) setTabMenuItems
                    | Users -> UserAdministration.UserAdministration authKeyOpt (fun () -> setAuthKey InvalidAuthKeyProvided) setTabMenuItems
                    | Products -> ProductAdministration.ProductAdministration authKeyOpt (fun () -> setAuthKey InvalidAuthKeyProvided) setTabMenuItems
                ]
            ]
            Bulma.section [
                prop.className "controls"
                prop.children [
                    Bulma.container [
                        Bulma.level [
                            Bulma.levelLeft [
                                prop.ref tabMenuContainerRef
                            ]

                            Bulma.levelRight [
                                Bulma.levelItem [
                                    Bulma.button.a [
                                        prop.href (Router.format(""))
                                        prop.children [
                                            Bulma.icon [ Fa.i [ Fa.Solid.Check ] [] ]
                                            Html.span [ prop.text "Fertig" ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
    ]
