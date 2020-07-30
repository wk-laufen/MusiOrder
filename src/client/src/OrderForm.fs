module OrderForm

open Api
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
    | LoadingUsers of Map<ProductId, int> * AuthKey
    | LoadUsersError of Map<ProductId, int> * AuthKey
    | LoadedUsers of Map<ProductId, int> * AuthKey * UserInfo list
    | Sending of Map<ProductId, int> * AuthKey
    | SendError of Map<ProductId, int> * AuthKey
    | Sent of AuthKey * Deferred<OrderSummary>

type Model =
    {
        Products: Deferred<ProductGroup list>
        Order: OrderState
    }

type Msg =
    | LoadProducts
    | LoadProductsResult of Result<ProductGroup list, exn>
    | ChangeOrderAmount of ProductId * delta: int
    | ResetOrder
    | Authenticate
    | LoadUsers of AuthKey
    | LoadUsersResult of Result<UserInfo list, LoadUsersError>
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
    state, Cmd.ofMsg LoadProducts

let loadProducts = async {
    let coders =
        Extra.empty
        |> Extra.withCustom ProductId.encode ProductId.decoder
    let! (products: ProductGroup list) = Fetch.``get``("/api/grouped-products", caseStrategy = CamelCase, extra = coders) |> Async.AwaitPromise
    return products
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
    | LoadProducts -> { state with Products = Deferred.InProgress }, Cmd.OfAsync.either (fun () -> loadProducts) () (Ok >> LoadProductsResult) (Error >> LoadProductsResult)
    | LoadProductsResult (Ok v) -> { state with Products = Deferred.Resolved v }, Cmd.none
    | LoadProductsResult (Error e) -> { state with Products = Deferred.Failed e }, Cmd.none
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
    | LoadUsers authKey ->
        match state.Order with
        | Authenticating order -> { state with Order = LoadingUsers (order, authKey) }, Cmd.OfAsync.perform loadUsers authKey LoadUsersResult
        | _ -> state, Cmd.none
    | LoadUsersResult (Ok users) ->
        match state.Order with
        | LoadingUsers (order, authKey) -> { state with Order = LoadedUsers (order, authKey, users) }, Cmd.none
        | _ -> state, Cmd.none
    | LoadUsersResult (Error Forbidden) ->
        match state.Order with
        | LoadingUsers (order, authKey) -> state, Cmd.ofMsg (SendOrder authKey)
        | _ -> state, Cmd.none
    | LoadUsersResult (Error Other) ->
        match state.Order with
        | LoadingUsers (order, authKey) -> { state with Order = LoadUsersError (order, authKey) }, Cmd.none
        | _ -> state, Cmd.none
    | SendOrder authKey ->
        match state.Order with
        | LoadingUsers (order, _)
        | LoadedUsers (order, _, _)
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
        | Sent (authKey, Deferred.Failed) -> { state with Order = Sent (authKey, Deferred.InProgress) }, Cmd.OfAsync.either loadOrderSummary authKey (Ok >> LoadOrderSummaryResult) (Error >> LoadOrderSummaryResult)
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
        | LoadingUsers (order, _) -> { state with Order = Drafting order }, Cmd.none
        | LoadUsersError (order, _) -> { state with Order = Drafting order }, Cmd.none
        | LoadedUsers (order, _, _) -> { state with Order = Drafting order }, Cmd.none
        | Sending -> state, Cmd.none
        | SendError (order, _) -> { state with Order = Drafting order }, Cmd.none
        | Sent -> { state with Order = Drafting Map.empty}, Cmd.none

let view = React.functionComponent ("OrderForm", fun (props: {| UserButtons: ReactElement list; AdminButtons: ReactElement list |}) ->
    let (state, dispatch) = React.useElmish(init, update, [||])

    let acceptsAuthKey =
        match state.Order with
        | Drafting -> false
        | Authenticating order -> true
        | LoadingUsers (order, _) -> false
        | LoadUsersError (order, _) -> true
        | LoadedUsers (order, _, _) -> false
        | Sending -> false
        | SendError (order, _) -> true
        | Sent -> false
    React.useAuthentication acceptsAuthKey (LoadUsers >> dispatch)

    let productView (product: Product) =
        let order =
            match state.Order with
            | Drafting order
            | Authenticating order
            | LoadingUsers (order, _)
            | LoadUsersError (order, _)
            | LoadedUsers (order, _, _)
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

    let errorView =
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

    let authForm =
        match state.Order with
        | Drafting -> Html.none
        | Authenticating -> View.modalAuthForm "Bestellung speichern" (fun () -> dispatch CloseSendOrder)
        | LoadingUsers -> View.modal "Bestellung speichern" (fun () -> dispatch CloseSendOrder) [ View.loadIconBig ] []
        | LoadUsersError (order, _) -> errorView
        | LoadedUsers (order, _, users) ->
            View.modal "Bestellung speichern" (fun () -> dispatch CloseSendOrder) [
                Bulma.table [
                    Html.thead [
                        Html.tr [
                            Html.th [ prop.text "Nachname" ]
                            Html.th [ prop.text "Vorname" ]
                            Html.th [ prop.text "Aktuelles Guthaben" ]
                        ]
                    ]
                    Html.tbody [
                        for user in users ->
                            Html.tr [
                                prop.onClick (fun _ -> dispatch (SendOrder user.AuthKey))
                                prop.children [
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
                                        View.balanceColor user.Balance
                                        prop.textf "%.2f€" user.Balance
                                    ]
                                ]
                            ]
                    ]
                ]
            ] []
        | Sending -> View.modal "Bestellung speichern" (fun () -> dispatch CloseSendOrder) [ View.loadIconBig ] []
        | SendError -> errorView
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
            Bulma.section [ View.errorNotificationWithRetry error.Message (fun () -> dispatch LoadProducts) ]
        | Deferred.Resolved [] ->
            Bulma.section [ View.errorNotificationWithRetry "No products available." (fun () -> dispatch LoadProducts) ]
        | Deferred.Resolved products ->
            Bulma.section [
                prop.className "content"
                prop.children [
                    Bulma.container [
                        for group in products -> productGroupView group
                    ]
                ]
            ]
            Bulma.section [
                prop.className "controls"
                prop.children [
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
            ]

        authForm
    ]
)
