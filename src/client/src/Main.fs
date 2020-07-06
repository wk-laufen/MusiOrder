module Main

open Browser.Dom
open Fable.Core
open Fable.Core.JsInterop
open Fable.FontAwesome
open Feliz
open Feliz.Bulma
open Feliz.UseDeferred
open MusiOrder.Models
open Thoth.Fetch
open Thoth.Json

importAll "../styles/main.scss"

let [<Import("*","moment")>] moment: System.DateTimeOffset -> obj = jsNative
moment?locale("de-AT")

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

let modal (title: string) onHide (body: ReactElement list) =
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
            ]
        ]
    ]

let authForm title onHide =
    modal title onHide [
        Html.div [
            color.hasTextPrimary
            spacing.px2
            prop.children [
                Fa.i [ Fa.Solid.Key; Fa.Size Fa.Fa8x ] []
                Bulma.title.p [
                    prop.text "Authentifiziere dich mit deinem Musischlüssel"
                ]
            ]
        ]
    ]

let orderSummaryView orderSummary =
    [
        Html.text "Dein aktuelles Guthaben beträgt: "
        Bulma.tag [
            if orderSummary.Balance >= 5. then color.isSuccess
            elif orderSummary.Balance >= 0. then color.isWarning
            else color.isDanger
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

module React =
    let useAuthentication isActive =
        let (authKey, setAuthKey) = React.useState(None)
        React.useEffect (
            fun () ->
                if isActive then
                    let mutable key = ""
                    let mutable timeoutId = 0.
                    let rec finishAuthKey () =
                        setAuthKey (Some (AuthKey key))
                        key <- ""
                    and listener (e: Browser.Types.Event) =
                        window.clearTimeout timeoutId
                        key <- key + (e :?> Browser.Types.KeyboardEvent).key
                        timeoutId <- window.setTimeout (finishAuthKey, 500)
                    window.addEventListener("keydown", listener)
                    React.createDisposable (fun () -> window.removeEventListener("keydown", listener))
                else
                    setAuthKey None
                    React.createDisposable id
            ,
            [| box isActive |]
        )
        authKey

let loadOrderSummary authKey : Async<OrderSummary> = async {
    let url = sprintf "/api/order/summary?authKey=%s" (AuthKey.toString authKey |> JS.encodeURIComponent)
    return! Fetch.``get``(url, caseStrategy = CamelCase) |> Async.AwaitPromise
}

let orderForm = React.functionComponent ("OrderForm", fun (props: {| Children: ReactElement list |}) ->
    let loadData = async {
        let coders =
            Extra.empty
            |> Extra.withCustom ProductId.encode ProductId.decoder
        let! (data: ProductGroup list) = Fetch.``get``("/api/grouped-products", caseStrategy = CamelCase, extra = coders) |> Async.AwaitPromise
        return data
    }

    let (data, setData) = React.useState(Deferred.HasNotStartedYet)
    let startLoadingData = React.useDeferredCallback((fun () -> loadData), setData)
    React.useEffectOnce(startLoadingData)

    let (orders, setOrders) = React.useState(Map.empty)

    let changeAmount productId delta =
        let newAmount =
            match Map.tryFind productId orders with
            | Some amount -> amount + delta
            | None -> delta
        let newOrders =
            if newAmount <= 0 then Map.remove productId orders
            else Map.add productId newAmount orders
        setOrders newOrders

    let resetOrders () = setOrders Map.empty

    let (isFinishing, setFinishing) = React.useState(false)
    let authKey = React.useAuthentication isFinishing
    let startAuthenticate () = setFinishing true

    let (orderState, setOrderState) = React.useState(Deferred.HasNotStartedYet)

    let hideFinishingForm () =
        setFinishing false
        if Deferred.resolved orderState then resetOrders ()
        setOrderState Deferred.HasNotStartedYet

    let sendOrder authKey = async {
        let body =
            {
                AuthKey = authKey
                Entries =
                    orders
                    |> Map.toList
                    |> List.choose (fun (productId, amount) ->
                        PositiveInteger.tryCreate amount
                        |> Option.map (fun amount -> { ProductId = productId; Amount = amount })
                    )
            }
        let coders =
            Extra.empty
            |> Extra.withCustom ProductId.encode ProductId.decoder
            |> Extra.withCustom AuthKey.encode AuthKey.decoder
            |> Extra.withCustom PositiveInteger.encode PositiveInteger.decoder
        do! Fetch.post("/api/order", body, caseStrategy = CamelCase, extra = coders) |> Async.AwaitPromise

        return! loadOrderSummary authKey
    }
    let startSendOrder = React.useDeferredCallback(sendOrder, setOrderState)

    React.useEffect(
        fun () ->
            match authKey, orderState with
            | Some authKey, Deferred.HasNotStartedYet
            | Some authKey, Deferred.Failed -> startSendOrder authKey
            | _ -> ()
        ,
        [| box authKey |]
    )

    let productView product =
        let amount = Map.tryFind product.Id orders
        Bulma.level [
            prop.className "product"
            prop.children [
                Bulma.levelLeft [
                    prop.style [
                        style.flexShrink 1
                    ]
                    prop.children [
                        Bulma.levelItem [
                            prop.className "product-name"
                            prop.style [
                                style.flexShrink 1
                            ]
                            prop.children [
                                Bulma.title.p [
                                    title.is5
                                    prop.text product.Name
                                ]
                            ]
                        ]
                    ]
                ]
                Bulma.levelRight [
                    prop.children [
                        Bulma.levelItem [
                            match amount with
                            | Some amount ->
                                Bulma.title.p [
                                    title.is5
                                    color.hasTextSuccess
                                    prop.textf "%.2f€" (float amount * product.Price)
                                ]
                            | None ->
                                Bulma.title.p [
                                    title.is5
                                    prop.textf "%.2f€" (float product.Price)
                                ]
                        ]

                        Bulma.levelItem [
                            Bulma.button.button [
                                color.isDanger
                                prop.disabled (Option.defaultValue 0 amount <= 0)
                                prop.onClick (fun _e -> changeAmount product.Id -1)
                                prop.children [
                                    Bulma.icon [
                                        Fa.i [ Fa.Solid.Minus; Fa.Size Fa.Fa2x ] []
                                    ]
                                ]
                            ]
                        ]

                        Bulma.levelItem [
                            Bulma.title.p [
                                title.is5
                                prop.text (amount |> Option.map string |> Option.defaultValue "")
                            ]
                        ]

                        Bulma.levelItem [
                            Bulma.button.button [
                                color.isSuccess
                                prop.onClick (fun _e -> changeAmount product.Id 1)
                                prop.children [
                                    Bulma.icon [
                                        Fa.i [ Fa.Solid.Plus; Fa.Size Fa.Fa2x ] []
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]

    let productGroupView group =
        Bulma.box [
            yield Bulma.title.h3 [ prop.text group.Name ]
            for product in group.Products -> productView product
        ]

    let controlButtons = Bulma.buttons [
        prop.className "controls"
        prop.children [
            Bulma.button.button [
                color.isDanger
                prop.disabled (Map.isEmpty orders)
                prop.onClick (ignore >> resetOrders)
                prop.children [
                    Bulma.icon [ Fa.i [ Fa.Solid.UndoAlt ] [] ]
                    Html.span [ prop.text "Bestellung löschen" ]
                ]
            ]
            Bulma.button.button [
                color.isSuccess
                prop.disabled (Map.isEmpty orders)
                prop.onClick (ignore >> startAuthenticate)
                prop.children [
                    Bulma.icon [ Fa.i [ Fa.Solid.EuroSign ] [] ]
                    Html.span [ prop.text "Bestellung speichern" ]
                ]
            ]
        ]
    ]

    let authForm =
        match authKey with
        | None -> authForm "Bestellung speichern" hideFinishingForm
        | Some ->
            let children =
                match orderState with
                | Deferred.HasNotStartedYet -> []
                | Deferred.InProgress ->
                    [
                        Html.div [
                            color.hasTextPrimary
                            prop.children [
                                Fa.i [ Fa.Solid.Spinner; Fa.Pulse; Fa.Size Fa.Fa8x ] []
                            ]
                        ]
                    ]
                | Deferred.Failed ->
                    [
                        Bulma.container [
                            color.hasTextDanger
                            spacing.px2
                            prop.children [
                                Fa.i [ Fa.Solid.Key; Fa.Size Fa.Fa8x ] []
                                Bulma.title.p [
                                    color.hasTextDanger
                                    prop.children [
                                        Html.text "Fehler beim Bestellen."
                                        Html.br []
                                        Html.text "Versuche es nochmal mit deinem Musischlüssel."
                                    ]
                                ]
                            ]
                        ]
                    ]
                | Deferred.Resolved orderSummary ->
                    [
                        Bulma.container [
                            spacing.px2
                            prop.children [
                                Html.div [
                                    color.hasTextSuccess
                                    prop.children [
                                        Fa.i [ Fa.Solid.Check; Fa.Size Fa.Fa8x ] []
                                    ]
                                ]
                                Bulma.title.p [
                                    color.hasTextSuccess
                                    prop.children [
                                        Html.text "Bestellung erfolgreich gespeichert. Prost Mahlzeit, "
                                        Html.span [
                                            color.hasTextPrimary
                                            prop.text orderSummary.ClientFullName
                                        ]
                                        Html.text "!"
                                    ]
                                ]
                                Html.br []
                                yield! orderSummaryView orderSummary
                            ]
                        ]
                    ]

            modal "Bestellung speichern" hideFinishingForm children

    [
        match data with
        | Deferred.HasNotStartedYet ->
            ()
        | Deferred.InProgress ->
            yield Bulma.section [ Bulma.progress [ color.isPrimary ] ]
        | Deferred.Failed error ->
            yield Bulma.section [ errorNotificationWithRetry error.Message startLoadingData ]
        | Deferred.Resolved [] ->
            yield Bulma.section [ errorNotificationWithRetry "No products available." startLoadingData ]
        | Deferred.Resolved data ->
            yield Bulma.section [
                prop.className "products"
                prop.children [
                    Bulma.container [
                        for group in data -> productGroupView group
                    ]
                ]
            ]
            yield Bulma.section [
                Bulma.container [
                    Bulma.level [
                        Bulma.levelLeft [
                            Bulma.levelItem [
                                controlButtons
                            ]
                        ]
                        Bulma.levelRight [
                            Bulma.levelItem props.Children
                        ]
                    ]
                ]
            ]

        if isFinishing then yield authForm
    ]
)

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
                            Fa.i [ Fa.Solid.Beer; Fa.Size Fa.Fa2x ] []
                        ]
                    ]
                ]
            ]
        ]
    ]

let showOrderSummary = React.functionComponent (fun () ->
    let (isVisible, setVisible) = React.useState(false)
    let authKey = React.useAuthentication isVisible
    let startAuthenticate () = setVisible true

    let (orderSummaryState, setOrderSummaryState) = React.useState(Deferred.HasNotStartedYet)

    let hideOrderSummary () =
        setVisible false
        setOrderSummaryState Deferred.HasNotStartedYet

    let startLoadOrderSummary = React.useDeferredCallback(loadOrderSummary, setOrderSummaryState)

    React.useEffect(
        fun () ->
            match authKey, orderSummaryState with
            | Some authKey, Deferred.HasNotStartedYet
            | Some authKey, Deferred.Failed -> startLoadOrderSummary authKey
            | _ -> ()
        ,
        [| box authKey |]
    )

    let authForm =
        match authKey with
        | None -> authForm "Bestellungen anzeigen" hideOrderSummary
        | Some ->
            match orderSummaryState with
            | Deferred.HasNotStartedYet -> Html.none
            | Deferred.InProgress ->
                modal "Bestellungen anzeigen" hideOrderSummary [
                    Html.div [
                        color.hasTextPrimary
                        prop.children [
                            Fa.i [ Fa.Solid.Spinner; Fa.Pulse; Fa.Size Fa.Fa8x ] []
                        ]
                    ]
                ]
            | Deferred.Failed ->
                modal "Bestellungen anzeigen" hideOrderSummary [
                    Bulma.container [
                        color.hasTextDanger
                        spacing.px2
                        prop.children [
                            Fa.i [ Fa.Solid.Key; Fa.Size Fa.Fa8x ] []
                            Bulma.title.p [
                                color.hasTextDanger
                                prop.children [
                                    Html.text "Fehler beim Anzeigen der Bestellungen."
                                    Html.br []
                                    Html.text "Versuche es nochmal mit deinem Musischlüssel."
                                ]
                            ]
                        ]
                    ]
                ]
            | Deferred.Resolved orderSummary ->
                modal (sprintf "Bestellungen von %s" orderSummary.ClientFullName) hideOrderSummary [
                    Bulma.container (orderSummaryView orderSummary)
                ]

    [
        Bulma.button.button [
            color.isInfo
            prop.onClick (ignore >> startAuthenticate)
            prop.children [
                Bulma.icon [ Fa.i [ Fa.Solid.FileAlt ] [] ]
                Html.span [ prop.text "Bestellungen anzeigen" ]
            ]
        ]
        if isVisible then authForm
    ]
)

let main =
    Html.div [
        prop.className "main"
        prop.children [
            nav
            orderForm
                {| Children =
                    [
                        Bulma.buttons [
                            showOrderSummary ()
                        ]
                    ]
                |}
        ]
    ]

ReactDOM.render(main, document.getElementById "app")
