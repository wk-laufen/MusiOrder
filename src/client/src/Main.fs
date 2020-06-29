module Main

open Browser.Dom
open Fable.Core
open Fable.Core.JsInterop
open Fable.FontAwesome
open Feliz
open Feliz.Bulma
open Feliz.UseDeferred
open Thoth.Fetch
open Thoth.Json

importAll "../styles/main.scss"

type ProductId = ProductId of string

module ProductId =
    let decoder = Decode.string |> Decode.map ProductId

type Product = {
    Id: ProductId
    Name: string
    Price: float
}

type ProductGroup = {
    Name: string
    Products: Product list
}

module ProductGroup =
    let decoder =
        let decoders =
            Extra.empty
            |> Extra.withCustom (fun _ -> failwith "Not implemeneted") ProductId.decoder
        Decode.Auto.generateDecoderCached<ProductGroup>(caseStrategy = CamelCase, extra = decoders)

type OrderState =
    | Drafting
    | Authenticating

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
                        Bulma.button.button [
                            color.isSuccess
                            prop.onClick (ignore >> onRetry)
                            prop.children [
                                Bulma.icon [ Fa.i [ Fa.Solid.SyncAlt ] [] ]
                                Html.span [ prop.text "Retry" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let products = React.functionComponent (fun () ->
    let loadData = async {
        return! Fetch.``get``("/api/grouped-products", decoder = Decode.list ProductGroup.decoder) |> Async.AwaitPromise
    }

    let (data, setData) = React.useState(Deferred.HasNotStartedYet)
    let startLoadingData = React.useDeferredCallback((fun () -> loadData), setData)
    React.useEffectOnce(startLoadingData)

    let (orders, setOrders) = React.useState(Map.empty)
    let (orderState, setOrderState) = React.useState(Drafting)

    let changeAmount productId delta =
        let newAmount =
            match Map.tryFind productId orders with
            | Some amount -> amount + delta
            | None -> delta
        let newOrders =
            if newAmount <= 0 then Map.remove productId orders
            else Map.add productId newAmount orders
        setOrders newOrders

    let resetOrders () =
        setOrders Map.empty

    let showAuthForm () =
        setOrderState Authenticating

    let hideAuthForm () =
        setOrderState Drafting

    Bulma.section [
        match data with
        | Deferred.HasNotStartedYet ->
            ()
        | Deferred.InProgress ->
            yield Bulma.progress [ color.isPrimary ]
        | Deferred.Failed error ->
            yield errorNotificationWithRetry error.Message startLoadingData
        | Deferred.Resolved data ->
            for group in data ->
                Bulma.box [
                    yield Bulma.title.h3 [ prop.text group.Name ]
                    for product in group.Products ->
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
                ]
            yield Bulma.buttons [
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
                    prop.onClick (ignore >> showAuthForm)
                    prop.children [
                        Bulma.icon [ Fa.i [ Fa.Solid.EuroSign ] [] ]
                        Html.span [ prop.text "Order" ]
                    ]
                ]
            ]

            yield Bulma.modal [
                if orderState = Authenticating then helpers.isActive
                prop.children [
                    Bulma.modalBackground [
                        prop.onClick (ignore >> hideAuthForm)
                    ]
                    Bulma.modalContent [
                        Bulma.box [
                            Bulma.title.h3 [
                                prop.text "Authenticate using your hardware key"
                            ]
                            Bulma.container [
                                text.hasTextCentered
                                prop.children [
                                    Fa.i [ Fa.Solid.Key; Fa.Size Fa.Fa8x ] []
                                ]
                            ]
                        ]
                    ]
                    Bulma.modalClose [
                        prop.onClick (ignore >> hideAuthForm)
                    ]
                ]
            ]
    ]
)

let main =
    Html.div [
        prop.style [
            style.display.flex
            style.flexDirection.column
            style.height (length.vh 100)
        ]
        prop.children [
            Bulma.navbar [
                color.hasBackgroundPrimary
                prop.style [
                    style.flexShrink 0
                ]
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
            Html.div [
                prop.style [
                    style.flexGrow 1
                    style.overflowY.scroll
                ]
                prop.children [
                    products ()
                ]
            ]
        ]
    ]

ReactDOM.render(main, document.getElementById "app")
