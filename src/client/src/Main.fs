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

type Product = {
    Name: string
    Price: float
}

type ProductGroup = {
    Name: string
    Products: Product list
}

module ProductGroup =
    let decoder = Decode.Auto.generateDecoderCached<ProductGroup>(caseStrategy = CamelCase)

let errorNotificationWithRetry (message: string) onRetry =
    Bulma.notification [
        color.hasBackgroundDanger
        color.hasTextWhite
        prop.children [
            Bulma.level [
                Bulma.levelLeft [
                    Bulma.levelItem [
                        Fa.i [ Fa.Solid.ExclamationTriangle; Fa.Size Fa.Fa2x ] []
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
                                Html.span [ prop.text "Retry" ]
                                Bulma.icon [ Fa.i [ Fa.Solid.SyncAlt ] [] ]
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
                Html.section [
                    yield Bulma.title.h3 [ prop.text group.Name ]
                    for product in group.Products ->
                        Html.div [
                            prop.text product.Name
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
                        Bulma.navbarItem.div [
                            Bulma.title.h1 [
                                prop.text "MusiOrder 🥂"
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
