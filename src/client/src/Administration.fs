module Administration

open Elmish
open Fable.Core.JsInterop
open Fable.React
open Feliz
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
    | OrderStatistics
    | DataExport
module Tab =
    let title = function
        | UserPayment -> "Guthaben"
        | Orders -> "Bestellungen"
        | Users -> "Benutzer"
        | Products -> "Artikel"
        | OrderStatistics -> "Bericht"
        | DataExport -> "Datenexport"
    let toRoute = function
        | UserPayment -> "guthaben"
        | Orders -> "bestellungen"
        | Users -> "benutzer"
        | Products -> "artikel"
        | OrderStatistics -> "bericht"
        | DataExport -> "data-export"
let allTabs = [ UserPayment; Orders; Users; Products; OrderStatistics; DataExport ]

type AuthKeyState =
    | NoAuthKeyProvided
    | AuthKeyProvided of Result<AuthKey, React.AuthenticationError>
    | InvalidAuthKeyProvided

[<ReactComponent>]
let Administration activeTab =
    let (authKey, setAuthKey) = React.useState(NoAuthKeyProvided)
    let acceptsAuthKey =
        match authKey with
        | NoAuthKeyProvided
        | InvalidAuthKeyProvided -> true
        | AuthKeyProvided _ -> false
    React.useAuthentication acceptsAuthKey (AuthKeyProvided >> setAuthKey)

    let tabMenuContainerRef = React.useRef(None)
    let setTabMenuItems (content: ReactElement list) =
        match tabMenuContainerRef.current with
        | Some node -> ReactDOM.createPortal(!!content, node)
        | None -> Html.none

    let abortButton =
        Html.a [
            prop.className "btn"
            prop.href (Router.format(""))
            prop.text "Abbrechen"
        ]

    React.fragment [
        match authKey with
        | NoAuthKeyProvided ->
            Html.div [
                prop.className "flex flex-col items-center gap-4 p-8"
                prop.children [
                    View.authForm
                    abortButton
                ]
            ]
        | InvalidAuthKeyProvided ->
            Html.div [
                prop.className "flex flex-col items-center gap-4 p-8"
                prop.children [
                    Html.span [
                        prop.className "fa fa-stack fa-4x text-musi-red"
                        prop.children [
                            Html.i [ prop.className "fas fa-key fa-stack-1x" ]
                            Html.i [ prop.className "fas fa-ban fa-stack-2x" ]
                        ]
                    ]
                    Html.span [
                        prop.className "text-center text-2xl text-musi-red"
                        prop.children [
                            Html.text "Schlüssel ist nicht autorisiert."
                            Html.br []
                            Html.text "Versuche es nochmal mit einem Administrator-Schlüssel."
                        ]
                    ]
                    abortButton
                ]
            ]
        | AuthKeyProvided (Error error) ->
            Html.div [
                prop.className "flex flex-col gap-4 p-8"
                prop.children [
                    View.authError error (fun () -> setAuthKey NoAuthKeyProvided)
                    Html.div [
                        prop.className "flex flex-col items-center"
                        prop.children [
                            abortButton
                        ]
                    ]
                ]
            ]
        | AuthKeyProvided (Ok authKey) ->
            Html.div [
                prop.className "overflow-y-auto grow p-8"
                prop.children [
                    Html.div [
                        prop.className "container mb-4"
                        prop.children [
                            Html.ul [
                                prop.className "flex border-b border-slate-200"
                                prop.children [
                                    for tab in allTabs ->
                                        Html.li [
                                            Html.a [
                                                prop.classes [
                                                    "inline-block px-4 py-2 -mb-px hover:border-b hover:border-musi-blue hover:text-musi-blue"
                                                    if tab = activeTab then "border-b border-musi-blue text-musi-blue"
                                                ]
                                                prop.text (Tab.title tab)
                                                prop.href (Router.format(route, Tab.toRoute tab))
                                            ]
                                        ]
                                ]
                            ]
                        ]
                    ]
                    match activeTab with
                    | UserPayment -> UserPaymentAdministration.UserPaymentAdministration (Some authKey) (fun () -> setAuthKey InvalidAuthKeyProvided) setTabMenuItems
                    | Orders -> OrderAdministration.OrderAdministration (Some authKey) (fun () -> setAuthKey InvalidAuthKeyProvided) setTabMenuItems
                    | Users -> UserAdministration.UserAdministration (Some authKey) (fun () -> setAuthKey InvalidAuthKeyProvided) setTabMenuItems
                    | Products -> ProductAdministration.ProductAdministration (Some authKey) (fun () -> setAuthKey InvalidAuthKeyProvided) setTabMenuItems
                    | OrderStatistics -> OrderStatistics.OrderStatistics (Some authKey) (fun () -> setAuthKey InvalidAuthKeyProvided) setTabMenuItems
                    | DataExport -> DataExport.DataExport (Some authKey) (fun () -> setAuthKey InvalidAuthKeyProvided) setTabMenuItems
                ]
            ]
            Html.div [
                prop.className "p-8"
                prop.children [
                    Html.div [
                        prop.className "container flex justify-between"
                        prop.children [
                            Html.div [
                                prop.className "flex items-center gap-2"
                                prop.ref tabMenuContainerRef
                            ]
                            Html.div [
                                prop.className "flex gap-2"
                                prop.children [
                                    Html.a [
                                        prop.className "!flex items-center gap-2 btn"
                                        prop.href (Router.format(""))
                                        prop.children [
                                            Html.i [ prop.className "fas fa-check" ]
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
