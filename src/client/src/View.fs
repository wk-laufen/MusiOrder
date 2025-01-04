module View

open Fable.Core.JsInterop
open Fable.FontAwesome
open Feliz
open Feliz.Bulma
open Feliz.Bulma.Operators
open global.JS

let retryButton onRetry =
    Bulma.button.button [
        color.isSuccess
        prop.onClick (ignore >> onRetry)
        prop.children [
            Bulma.icon [ Fa.i [ Fa.Solid.SyncAlt ] [] ]
            Html.span [ prop.text "Wiederholen" ]
        ]
    ]

let errorNotificationWithRetry (message: string) onRetry =
    Bulma.notification [
        color.isDanger
        prop.children [
            Bulma.level [
                Bulma.levelLeft [
                    Bulma.levelItem [
                        Bulma.icon [
                            Fa.i [ Fa.Solid.ExclamationTriangle; Fa.Size Fa.Fa2x ] []
                        ]
                    ]
                    Bulma.levelItem [
                        Bulma.title.p [
                            title.is4
                            prop.text message
                        ]
                    ]
                    Bulma.levelItem [
                        retryButton onRetry
                    ]
                ]
            ]
        ]
    ]

let infoNotification (message: string) =
    Bulma.notification [
        color.isInfo
        prop.children [
            Bulma.levelItem [
                Bulma.title.p [
                    title.is4
                    prop.text message
                ]
            ]
        ]
    ]

let loadIconBig =
    Html.div [
        text.hasTextCentered
        ++ color.hasTextPrimary
        ++ spacing.py2
        prop.children [
            Fa.i [ Fa.Solid.Spinner; Fa.Pulse; Fa.Size Fa.Fa8x ] []
        ]
    ]

let modal (title: string) onHide (body: ReactElement list) (footer: ReactElement list) =
    ReactDOM.createPortal (
        Bulma.modal [
            helpers.isActive
            prop.children [
                Bulma.modalBackground [
                    prop.onClick (ignore >> onHide)
                ]
                Bulma.modalCard [
                    Bulma.modalCardHead [
                        Bulma.modalCardTitle [
                            prop.text title
                        ]
                        Html.a [
                            prop.className "delete"
                            prop.onClick (ignore >> onHide)
                        ]
                    ]
                    Bulma.modalCardBody body
                    Bulma.modalCardFoot footer
                ]
            ]
        ],
        Browser.Dom.document.body
    )

let authForm =
    Bulma.container [
        text.hasTextCentered
        ++ color.hasTextPrimary
        ++ spacing.px2
        prop.children [
            Fa.i [ Fa.Solid.Key; Fa.Size Fa.Fa8x ] []
            Bulma.title.p [
                color.hasTextPrimary
                spacing.mt4
                prop.text "Authentifiziere dich mit deinem Musischlüssel"
            ]
        ]
    ]

let modalAuthForm title onHide =
    modal title onHide [ authForm ] []

let authError error onRetry =
    match error with
    | React.ReaderNotAvailable -> 
        errorNotificationWithRetry "Lesegerät nicht erkannt." onRetry

let modalAuthError title error onRetry onHide =
    modal title onHide [ authError error onRetry ] []

let bulmaBalanceColor balance =
    if balance >= 5.m then color.isSuccess
    elif balance >= 0.m then color.isWarning
    else color.isDanger

let balanceColor balance =
    if balance >= 5.m then "text-musi-green"
    elif balance >= 0.m then "text-musi-blue"
    else "text-musi-red"

module Order =
    open MusiOrder.Models.Order

    let orderSummary (orderSummary: OrderSummary) =
        Html.div [
            prop.className "flex flex-col gap-2 text-center"
            prop.children [
                Html.span [
                    Html.text "Dein aktuelles Guthaben beträgt: "
                    Html.span [
                        prop.className $"text-lg %s{balanceColor orderSummary.Balance}"
                        prop.text $"%.2f{orderSummary.Balance}€"
                    ]
                ]
                Html.div [
                    prop.className "flex flex-col gap-2"
                    prop.children [
                        match orderSummary.LatestOrders with
                        | [] -> Html.text "Keine Bestellungen in der letzten Zeit. 😱"
                        | orders ->
                            Html.span [ prop.text "Deine letzten Bestellungen waren:" ]
                            Html.ul [
                                prop.className "flex flex-col gap-2"
                                prop.children [
                                    for order in orders do
                                        Html.li [
                                            Html.textf "%s: " (moment(order.Timestamp)?fromNow())
                                            Html.span [
                                                prop.className "inline-block px-2 py-1 bg-musi-blue text-white rounded"
                                                prop.text $"%d{order.Amount} x %s{order.ProductName}"
                                            ]
                                        ]
                                ]
                            ]
                    ]
                ]
            ]
        ]

    let userCards users onClick =
        Html.div [
        prop.className "flex flex-wrap gap-2"
        prop.children [
            for user in users do
                Html.div [
                    prop.className "flex flex-col grow shadow rounded p-2 cursor-pointer"
                    prop.onClick (fun _ -> onClick user)
                    prop.children [
                        Html.span [
                            prop.className "text-center"
                            prop.text $"%s{user.LastName.ToUpper()} %s{user.FirstName}"
                        ]
                        Html.span [
                            prop.className $"text-center text-sm %s{balanceColor user.Balance}"
                            prop.text $"%.2f{user.Balance}€"
                        ]
                    ]
                ]
        ]
    ]
