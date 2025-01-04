module View

open Fable.Core.JsInterop
open Fable.FontAwesome
open Feliz
open global.JS

let retryButton onClick =
    Html.button [
        prop.className "!flex items-center gap-2 btn btn-solid btn-green"
        prop.onClick (ignore >> onClick)
        prop.children [
            Fa.i [ Fa.Solid.SyncAlt ] []
            Html.span [ prop.text "Wiederholen" ]
        ]
    ]

let errorNotificationWithRetry (message: string) onRetry =
    Html.div [
        prop.className "flex items-center gap-4 bg-musi-red rounded text-white px-8 py-4"
        prop.children [
            Fa.i [ Fa.Solid.ExclamationTriangle; Fa.Size Fa.Fa2x ] []
            Html.span [
                prop.className "text-2xl"
                prop.text message
            ]
            retryButton onRetry
        ]
    ]

let infoNotification (message: string) (elements: ReactElement list) =
    Html.div [
        prop.className "flex items-center gap-4 bg-musi-blue rounded px-8 py-4 text-white"
        prop.children [
            Html.span [
                prop.className "text-2xl"
                prop.text message
            ]
            yield! elements
        ]
    ]

let loadIconBig =
    Html.div [
        prop.className "text-center text-musi-gold py-2"
        prop.children [
            Fa.i [ Fa.Solid.Spinner; Fa.Pulse; Fa.Size Fa.Fa8x ] []
        ]
    ]

let modal (title: string) onHide (body: ReactElement list) (footer: ReactElement list) =
    ReactDOM.createPortal (
        Html.div [
            prop.className "fixed inset-0 grid h-screen w-screen place-items-center bg-slate-800 bg-opacity-60 backdrop-blur-sm animate-fadeIn"
            prop.onClick (ignore >> onHide)
            prop.children [
                Html.div [
                    prop.className "w-[640px] flex flex-col"
                    prop.onClick (fun e -> e.stopPropagation())
                    prop.children [
                        Html.div [
                            prop.className "grow-0 bg-slate-200 rounded-t-lg px-8 py-4 text-2xl"
                            prop.text title
                        ]
                        Html.div [
                            prop.className "grow-1 p-8 bg-white"
                            prop.children body
                        ]
                        Html.div [
                            prop.className "grow-0 flex justify-end gap-2 bg-slate-200 text-white rounded-b-lg px-4 py-2"
                            prop.children [
                                yield! footer
                                Html.button [
                                    prop.className "btn btn-white"
                                    prop.onClick (ignore >> onHide)
                                    prop.text "Schließen"
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ],
        Browser.Dom.document.body
    )

let authForm =
    Html.div [
        prop.className "flex flex-col gap-2 items-center text-musi-gold"
        prop.children [
            Fa.i [ Fa.Solid.Key; Fa.Size Fa.Fa8x ] []
            Html.span [
                prop.className "text-center text-3xl"
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
