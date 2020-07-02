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

let products = React.functionComponent (fun () ->
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
    let (authKey, setAuthKey) = React.useState(None)
    React.useEffect (
        fun () ->
            if isFinishing then
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
                React.createDisposable id
        ,
        [| box isFinishing |]
    )
    let startAuthenticate () = setFinishing true

    let (orderState, setOrderState) = React.useState(Deferred.HasNotStartedYet)

    let hideFinishingForm () =
        setFinishing false
        setAuthKey None
        if Deferred.resolved orderState then resetOrders ()
        setOrderState Deferred.HasNotStartedYet

    let sendOrder authKey = async {
        let body =
            {
                AuthKey = authKey
                Entries =
                    orders
                    |> Map.toList
                    |> List.map (fun (productId, amount) -> { ProductId = productId; Amount = amount })
            }
        let coders =
            Extra.empty
            |> Extra.withCustom ProductId.encode ProductId.decoder
            |> Extra.withCustom AuthKey.encode AuthKey.decoder
        return! Fetch.``post``("/api/order", body, caseStrategy = CamelCase, extra = coders, decoder = Decode.unit) |> Async.AwaitPromise
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
                    Bulma.levelItem [
                        prop.className "product-name"
                        prop.children [
                            Bulma.title.p [
                                title.is5
                                prop.text product.Name
                            ]
                        ]
                    ]

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
                    Html.span [ prop.text "Reset" ]
                ]
            ]
            Bulma.button.button [
                color.isSuccess
                prop.disabled (Map.isEmpty orders)
                prop.onClick (ignore >> startAuthenticate)
                prop.children [
                    Bulma.icon [ Fa.i [ Fa.Solid.EuroSign ] [] ]
                    Html.span [ prop.text "Order" ]
                ]
            ]
        ]
    ]

    let authForm = Bulma.modal [
        if isFinishing then helpers.isActive
        prop.children [
            Bulma.modalBackground [
                prop.onClick (ignore >> hideFinishingForm)
            ]
            Bulma.modalCard [
                Bulma.modalCardHead [
                    Bulma.modalCardTitle [
                        prop.text "Authenticate using your hardware key"
                    ]
                    Bulma.delete [
                        prop.onClick (ignore >> hideFinishingForm)
                    ]
                ]
                Bulma.modalCardBody [
                    text.hasTextCentered
                    prop.children [
                        match authKey, orderState with
                        | None, _ ->
                            Html.div [
                                color.hasTextPrimary
                                prop.style [ style.padding 10 ]
                                prop.children [
                                    Fa.i [ Fa.Solid.Key; Fa.Size Fa.Fa8x ] []
                                ]
                            ]
                        | Some, Deferred.HasNotStartedYet -> ()
                        | Some, Deferred.InProgress ->
                            Html.div [
                                color.hasTextPrimary
                                prop.children [
                                    Fa.i [ Fa.Solid.Spinner; Fa.Pulse; Fa.Size Fa.Fa8x ] []
                                ]
                            ]
                        | Some, Deferred.Failed e ->
                            Bulma.container [
                                color.hasTextDanger
                                prop.style [ style.padding 10 ]
                                prop.children [
                                    Fa.i [ Fa.Solid.Key; Fa.Size Fa.Fa8x ] []
                                    Bulma.title.p [
                                        color.hasTextDanger
                                        prop.children [
                                            Html.text "Error while placing order."
                                            Html.br []
                                            Html.text "Try again using your hardware key."
                                        ]
                                    ]
                                ]
                            ]
                        | Some, Deferred.Resolved ->
                            Bulma.container [
                                color.hasTextSuccess
                                prop.style [ style.padding 10 ]
                                prop.children [
                                    Fa.i [ Fa.Solid.Check; Fa.Size Fa.Fa8x ] []
                                    Bulma.title.p [
                                        color.hasTextSuccess
                                        prop.text "Your order has been placed successfully. Enjoy!"
                                    ]
                                ]
                            ]
                    ]
                ]
            ]
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
        | Deferred.Resolved data ->
            yield Bulma.section [
                prop.className "products"
                prop.children [
                    for group in data -> productGroupView group
                ]
            ]
            yield Bulma.section [ controlButtons ]

        yield authForm
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

let main =
    Html.div [
        prop.className "main"
        prop.children [
            nav
            products ()
        ]
    ]

ReactDOM.render(main, document.getElementById "app")
