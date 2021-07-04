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
                    Bulma.delete [
                        prop.onClick (ignore >> onHide)
                    ]
                ]
                Bulma.modalCardBody body
                Bulma.modalCardFoot footer
            ]
        ]
    ]

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

let balanceColor balance =
    if balance >= 5.m then color.isSuccess
    elif balance >= 0.m then color.isWarning
    else color.isDanger

module Order =
    open MusiOrder.Models.Order

    let orderSummary (orderSummary: OrderSummary) =
        [
            Html.text "Dein aktuelles Guthaben beträgt: "
            Bulma.tag [
                balanceColor orderSummary.Balance
                control.isLarge
                prop.textf "%.2f€" orderSummary.Balance
            ]
            Html.br []
            match orderSummary.LatestOrders with
            | [] -> Html.text "Keine Bestellungen in der letzten Zeit. 😱"
            | orders ->
                Html.text "Deine letzten Bestellungen waren:"
                Html.ul [
                    for order in orders ->
                        Html.li [
                            Html.textf "%s: " (moment(order.Timestamp)?fromNow())
                            Bulma.tag [
                                color.isInfo
                                control.isMedium
                                spacing.mt2
                                prop.textf "%d x %s" order.Amount order.ProductName
                            ]
                        ]
                ]
        ]
