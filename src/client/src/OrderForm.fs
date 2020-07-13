module OrderForm

open Elmish
open Fable.Core
open Fable.FontAwesome
open Feliz
open Feliz.Bulma
open Feliz.UseDeferred
open Feliz.UseElmish
open MusiOrder.Models
open Thoth.Fetch
open Thoth.Json

type OrderState =
    | Drafting of Map<ProductId, int>
    | Authenticating of Map<ProductId, int>
    | Sending of Map<ProductId, int> * AuthKey
    | SendError of Map<ProductId, int> * AuthKey
    | Sent of AuthKey * Deferred<OrderSummary>

type Model =
    {
        Products: Deferred<ProductGroup list>
        Order: OrderState
    }

type Msg =
    | LoadData
    | LoadDataResult of Result<ProductGroup list, exn>
    | ChangeOrderAmount of ProductId * delta: int
    | ResetOrder
    | Authenticate
    | SendOrder of AuthKey
    | SendOrderResult of Result<unit, exn>
    | LoadOrderSummary
    | LoadOrderSummaryResult of Result<OrderSummary, exn>
    | CloseSendOrder

let init =
    let state =
        {
            Products = Deferred.HasNotStartedYet
            Order = Drafting Map.empty
        }
    state, Cmd.ofMsg LoadData

let loadData = async {
    let coders =
        Extra.empty
        |> Extra.withCustom ProductId.encode ProductId.decoder
    let! (data: ProductGroup list) = Fetch.``get``("/api/grouped-products", caseStrategy = CamelCase, extra = coders) |> Async.AwaitPromise
    return data
}

let sendOrder authKey order = async {
    let body =
        {
            AuthKey = authKey
            Entries =
                order
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
}

let update msg (state: Model) =
    match msg with
    | LoadData -> { state with Products = Deferred.InProgress }, Cmd.OfAsync.either (fun () -> loadData) () (Ok >> LoadDataResult) (Error >> LoadDataResult)
    | LoadDataResult (Ok v) -> { state with Products = Deferred.Resolved v }, Cmd.none
    | LoadDataResult (Error e) -> { state with Products = Deferred.Failed e }, Cmd.none
    | ChangeOrderAmount (productId, delta) ->
        match state.Order with
        | Drafting order ->
            let newAmount =
                match Map.tryFind productId order with
                | Some amount -> amount + delta
                | None -> delta
            let newOrder =
                if newAmount <= 0 then Map.remove productId order
                else Map.add productId newAmount order
            { state with Order = Drafting newOrder }, Cmd.none
        | _ -> state, Cmd.none
    | ResetOrder ->
        match state.Order with
        | Drafting -> { state with Order = Drafting Map.empty }, Cmd.none
        | _ -> state, Cmd.none
    | Authenticate ->
        match state.Order with
        | Drafting order -> { state with Order = Authenticating order }, Cmd.none
        | _ -> state, Cmd.none
    | SendOrder authKey ->
        match state.Order with
        | Drafting order
        | Authenticating order
        | SendError (order, _) -> { state with Order = Sending (order, authKey) }, Cmd.OfAsync.either (uncurry sendOrder) (authKey, order) (Ok >> SendOrderResult) (Error >> SendOrderResult)
        | _ -> state, Cmd.none
    | SendOrderResult Ok ->
        match state.Order with
        | Sending (_, authKey) -> { state with Order = Sent (authKey, Deferred.HasNotStartedYet) }, Cmd.ofMsg LoadOrderSummary
        | _ -> state, Cmd.none
    | SendOrderResult Error ->
        match state.Order with
        | Sending (order, authKey) -> { state with Order = SendError (order, authKey) }, Cmd.none
        | _ -> state, Cmd.none
    | LoadOrderSummary ->
        match state.Order with
        | Sent (authKey, Deferred.HasNotStartedYet)
        | Sent (authKey, Deferred.Failed) -> { state with Order = Sent (authKey, Deferred.InProgress) }, Cmd.OfAsync.either Api.loadOrderSummary authKey (Ok >> LoadOrderSummaryResult) (Error >> LoadOrderSummaryResult)
        | _ -> state, Cmd.none
    | LoadOrderSummaryResult (Ok v) ->
        match state.Order with
        | Sent (authKey, Deferred.InProgress) -> { state with Order = Sent (authKey, Deferred.Resolved v) }, Cmd.none
        | _ -> state, Cmd.none
    | LoadOrderSummaryResult (Error e) ->
        match state.Order with
        | Sent (authKey, Deferred.InProgress) -> { state with Order = Sent (authKey, Deferred.Failed e) }, Cmd.none
        | _ -> state, Cmd.none
    | CloseSendOrder ->
        match state.Order with
        | Drafting -> state, Cmd.none
        | Authenticating order -> { state with Order = Drafting order }, Cmd.none
        | Sending -> state, Cmd.none
        | SendError (order, _) -> { state with Order = Drafting order }, Cmd.none
        | Sent -> { state with Order = Drafting Map.empty}, Cmd.none

let view = React.functionComponent ("OrderForm", fun (props: {| UserButtons: ReactElement list; AdminButtons: ReactElement list |}) ->
    let (state, dispatch) = React.useElmish(init, update, [||])

    let acceptsAuthKey =
        match state.Order with
        | Authenticating
        | SendError -> true
        | _ -> false
    React.useAuthentication acceptsAuthKey (SendOrder >> dispatch)

    let productView (product: Product) =
        let order =
            match state.Order with
            | Drafting order
            | Authenticating order
            | Sending (order, _)
            | SendError (order, _) -> Some order
            | Sent -> None
        let amount = order |> Option.bind (Map.tryFind product.Id)
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
                                prop.onClick (fun _ -> dispatch (ChangeOrderAmount(product.Id, -1)))
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
                                prop.onClick (fun _ -> dispatch (ChangeOrderAmount(product.Id, 1)))
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
                match state.Order with
                | Drafting order ->
                    prop.disabled (Map.isEmpty order)
                    prop.onClick (fun _ -> dispatch ResetOrder)
                | _ -> ()
                prop.children [
                    Bulma.icon [ Fa.i [ Fa.Solid.UndoAlt ] [] ]
                    Html.span [ prop.text "Zurücksetzen" ]
                ]
            ]
            Bulma.button.button [
                color.isSuccess
                match state.Order with
                | Drafting order ->
                    prop.disabled (Map.isEmpty order)
                    prop.onClick (fun _ -> dispatch Authenticate)
                | _ -> ()
                prop.children [
                    Bulma.icon [ Fa.i [ Fa.Solid.EuroSign ] [] ]
                    Html.span [ prop.text "Bestellen" ]
                ]
            ]
            yield! props.UserButtons
        ]
    ]

    let authForm =
        match state.Order with
        | Drafting -> Html.none
        | Authenticating -> View.authForm "Bestellung speichern" (fun () -> dispatch CloseSendOrder)
        | Sending -> View.modal "Bestellung speichern" (fun () -> dispatch CloseSendOrder) [ View.loadIconBig ] []
        | SendError ->
            View.modal "Bestellung speichern" (fun () -> dispatch CloseSendOrder) [
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
            ] []
        | Sent (_, loadSummaryState) ->
            View.modal "Bestellung speichern" (fun () -> dispatch CloseSendOrder) [
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
                                Html.text "Bestellung erfolgreich gespeichert. Prost Mahlzeit"
                                match loadSummaryState with
                                | Deferred.Resolved orderSummary ->
                                    Html.text ", "
                                    Html.span [
                                        color.hasTextPrimary
                                        prop.text orderSummary.ClientFullName
                                    ]
                                | _ -> ()
                                Html.text "!"
                            ]
                        ]
                    ]
                ]
                Bulma.container [
                    spacing.mt2
                    prop.children [
                        match loadSummaryState with
                        | Deferred.HasNotStartedYet
                        | Deferred.InProgress -> View.loadIconBig
                        | Deferred.Failed error ->
                            View.errorNotificationWithRetry error.Message (fun () -> dispatch LoadOrderSummary)
                        | Deferred.Resolved orderSummary ->
                            yield! View.orderSummary orderSummary
                    ]
                ]
            ] []

    [
        match state.Products with
        | Deferred.HasNotStartedYet ->
            ()
        | Deferred.InProgress ->
            Bulma.section [ Bulma.progress [ color.isPrimary ] ]
        | Deferred.Failed error ->
            Bulma.section [ View.errorNotificationWithRetry error.Message (fun () -> dispatch LoadData) ]
        | Deferred.Resolved [] ->
            Bulma.section [ View.errorNotificationWithRetry "No products available." (fun () -> dispatch LoadData) ]
        | Deferred.Resolved data ->
            Bulma.section [
                prop.className "products"
                prop.children [
                    Bulma.container [
                        for group in data -> productGroupView group
                    ]
                ]
            ]
            Bulma.section [
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

        authForm
    ]
)
