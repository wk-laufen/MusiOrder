module View

open Fable.Core.JsInterop
open Fable.FontAwesome
open Feliz
open Feliz.Bulma
open global.JS
open MusiOrder.Models

let retryButton onRetry =
    Bulma.button.button [
        color.isSuccess
        prop.onClick (ignore >> onRetry)
        prop.children [
            Bulma.icon [ Fa.i [ Fa.Solid.SyncAlt ] [] ]
            Html.span [ prop.text "Retry" ]
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

let loadIconBig =
    Html.div [
        color.hasTextPrimary
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
                Bulma.modalCardBody [
                    text.hasTextCentered
                    prop.children body
                ]
                Bulma.modalCardFoot footer
            ]
        ]
    ]

let authForm title onHide =
    modal title onHide [
        Bulma.container [
            color.hasTextPrimary
            spacing.px2
            prop.children [
                Fa.i [ Fa.Solid.Key; Fa.Size Fa.Fa8x ] []
                Bulma.title.p [
                    color.hasTextPrimary
                    prop.text "Authentifiziere dich mit deinem Musischlüssel"
                ]
            ]
        ]
    ] []

let balanceColor balance =
    if balance >= 5. then color.isSuccess
    elif balance >= 0. then color.isWarning
    else color.isDanger

let orderSummary (orderSummary: OrderSummary) =
    [
        Html.text "Dein aktuelles Guthaben beträgt: "
        Bulma.tag [
            balanceColor orderSummary.Balance
            control.isLarge
            prop.textf "%.2f€" orderSummary.Balance
        ]
        Html.br []
        Html.textf "Deine letzten Bestellungen waren:"
        Html.ul [
            for order in orderSummary.LatestOrders ->
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
