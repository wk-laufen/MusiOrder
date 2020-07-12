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

let loadIconBig =
    Html.div [
        color.hasTextPrimary
        prop.children [
            Fa.i [ Fa.Solid.Spinner; Fa.Pulse; Fa.Size Fa.Fa8x ] []
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
    ]

let balanceColor balance =
    if balance >= 5. then color.isSuccess
    elif balance >= 0. then color.isWarning
    else color.isDanger

let orderSummaryView (orderSummary: OrderSummary) =
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
                        let newKey = (e :?> Browser.Types.KeyboardEvent).key
                        if newKey = "Enter" then finishAuthKey ()
                        else
                            key <- key + newKey
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

let orderForm = React.functionComponent ("OrderForm", fun (props: {| UserButtons: ReactElement list; AdminButtons: ReactElement list |}) ->
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

    let userButtons = Bulma.buttons [
        prop.className "controls"
        prop.children [
            Bulma.button.button [
                color.isDanger
                prop.disabled (Map.isEmpty orders)
                prop.onClick (ignore >> resetOrders)
                prop.children [
                    Bulma.icon [ Fa.i [ Fa.Solid.UndoAlt ] [] ]
                    Html.span [ prop.text "Zurücksetzen" ]
                ]
            ]
            Bulma.button.button [
                color.isSuccess
                prop.disabled (Map.isEmpty orders)
                prop.onClick (ignore >> startAuthenticate)
                prop.children [
                    Bulma.icon [ Fa.i [ Fa.Solid.EuroSign ] [] ]
                    Html.span [ prop.text "Bestellen" ]
                ]
            ]
            yield! props.UserButtons
        ]
    ]

    let authForm =
        match orderState with
        | Deferred.HasNotStartedYet -> authForm "Bestellung speichern" hideFinishingForm
        | Deferred.InProgress -> modal "Bestellung speichern" hideFinishingForm [ loadIconBig ]
        | Deferred.Failed ->
            modal "Bestellung speichern" hideFinishingForm [
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
            modal "Bestellung speichern" hideFinishingForm [
                Bulma.container [
                    spacing.px2
                    color.hasTextSuccess
                    prop.children [
                        Html.div [
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
                    ]
                ]
                Bulma.container [
                    spacing.mt2
                    prop.children (orderSummaryView orderSummary)
                ]
            ]

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
                                userButtons
                            ]
                        ]
                        Bulma.levelRight [
                            Bulma.levelItem [
                                Bulma.buttons props.AdminButtons
                            ]
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
                modal "Bestellungen anzeigen" hideOrderSummary [ loadIconBig ]
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
                Html.span [ prop.text "Meine Bestellungen" ]
            ]
        ]
        if isVisible then authForm
    ]
)

let showAdministration = React.functionComponent (fun () ->
    let (isVisible, setVisible) = React.useState(false)
    let authKey = React.useAuthentication isVisible
    let startAuthenticate () = setVisible true

    let authorize authKey : Async<Result<UserInfo list, unit>> = async {
        let url = sprintf "/api/users?authKey=%s" (AuthKey.toString authKey |> JS.encodeURIComponent)
        match! Fetch.``tryGet``(url, caseStrategy = CamelCase) |> Async.AwaitPromise with
        | Ok result -> return Ok result
        | Error (FetchFailed response) when response.Status = 403 -> return Error ()
        | Error e -> return failwith (Helper.message e)
    }

    let (authorizationState, setAuthorizationState) = React.useState(Deferred.HasNotStartedYet)

    let startAuthorization = React.useDeferredCallback(authorize, setAuthorizationState)

    let hideOrderSummary () =
        setVisible false
        setAuthorizationState Deferred.HasNotStartedYet

    React.useEffect(
        fun () ->
            match authKey, authorizationState with
            | Some authKey, Deferred.HasNotStartedYet -> startAuthorization authKey
            | Some authKey, Deferred.Failed -> startAuthorization authKey
            | Some authKey, Deferred.Resolved Error -> startAuthorization authKey
            | _ -> ()
        ,
        [| box authKey |]
    )

    let authForm =
        match authorizationState with
        | Deferred.HasNotStartedYet -> authForm "Administration" hideOrderSummary
        | Deferred.InProgress -> modal "Administration" hideOrderSummary [ loadIconBig ]
        | Deferred.Failed ->
            modal "Administration" hideOrderSummary [
                Bulma.container [
                    color.hasTextDanger
                    spacing.px2
                    prop.children [
                        Fa.i [ Fa.Solid.Key; Fa.Size Fa.Fa8x ] []
                        Bulma.title.p [
                            color.hasTextDanger
                            prop.children [
                                Html.text "Fehler bei der Authorisierung."
                                Html.br []
                                Html.text "Versuche es nochmal mit deinem Musischlüssel."
                            ]
                        ]
                    ]
                ]
            ]
        | Deferred.Resolved Error ->
            modal "Administration" hideOrderSummary [
                Bulma.container [
                    color.hasTextDanger
                    spacing.px2
                    prop.children [
                        Fa.i [ Fa.Solid.Ban; Fa.Size Fa.Fa8x ] []
                        Bulma.title.p [
                            color.hasTextDanger
                            prop.children [
                                Html.text "Schlüssel ist nicht authorisiert."
                                Html.br []
                                Html.text "Versuche es nochmal mit einem Administrator-Schlüssel."
                            ]
                        ]
                    ]
                ]
            ]
        | Deferred.Resolved (Ok users) ->
            modal "Administration" hideOrderSummary [
                Bulma.table [
                    Html.thead [
                        Html.tr [
                            Html.th [ prop.text "Nachname" ]
                            Html.th [ prop.text "Vorname" ]
                            Html.th [ prop.text "Letzte Bestellung" ]
                            Html.th [ prop.text "Aktuelles Guthaben" ]
                            Html.th [ prop.text "Guthaben aufladen" ]
                        ]
                    ]
                    Html.tbody [
                        for user in users ->
                            let (latestOrderColor, latestOrderTime) =
                                user.LatestOrderTimestamp
                                |> Option.map (fun v ->
                                    let m = moment(v)
                                    let daysSinceLatestOrder = moment(System.DateTimeOffset.Now)?diff(m, "days")
                                    let color =
                                        if daysSinceLatestOrder < 10. then Some color.isSuccess
                                        elif daysSinceLatestOrder < 30. then Some color.isWarning
                                        else Some color.isDanger
                                    color, moment(v)?fromNow()
                                )
                                |> Option.defaultValue (None, "-")
                            Html.tr [
                                Html.td [
                                    text.hasTextLeft
                                    prop.style [ style.textTransform.uppercase ]
                                    prop.text user.LastName
                                ]
                                Html.td [
                                    text.hasTextLeft
                                    prop.text user.FirstName
                                ]
                                Html.td [
                                    match latestOrderColor with
                                    | Some color -> color
                                    | None -> ()
                                    prop.text latestOrderTime
                                ]
                                Html.td [
                                    balanceColor user.Balance
                                    prop.textf "%.2f€" user.Balance
                                ]
                                Html.td [
                                    Bulma.buttons [
                                        Bulma.button.button [
                                            prop.text "+1€"
                                        ]
                                        Bulma.button.button [
                                            prop.text "+2€"
                                        ]
                                        Bulma.button.button [
                                            prop.text "+5€"
                                        ]
                                        Bulma.button.button [
                                            prop.text "+10€"
                                        ]
                                        Bulma.button.button [
                                            prop.text "+20€"
                                        ]
                                    ]
                                ]
                            ]
                    ]
                ]
            ]

    [
        Bulma.button.button [
            prop.onClick (ignore >> startAuthenticate)
            prop.children [
                Bulma.icon [ Fa.i [ Fa.Solid.Cogs ] [] ]
                Html.span [ prop.text "Administration" ]
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
                {|
                    UserButtons = [ showOrderSummary () ]
                    AdminButtons = [ showAdministration () ]
                |}
        ]
    ]

ReactDOM.render(main, document.getElementById "app")
