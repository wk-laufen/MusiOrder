module OrderForm

open Api
open Api.Order
open Elmish
open Fable.FontAwesome
open Feliz
open Feliz.UseDeferred
open Feliz.UseElmish
open MusiOrder.Models
open MusiOrder.Models.Order

type OrderState =
    | Drafting of Map<ProductId, int>
    | Authenticating of Map<ProductId, int>
    | AuthenticationError of Map<ProductId, int> * React.AuthenticationError
    | LoadingUsers of Map<ProductId, int> * AuthKey option
    | LoadUsersError of Map<ProductId, int> * AuthKey option
    | LoadedUsers of Map<ProductId, int> * AuthKey option * UserInfo list
    | Sending of Map<ProductId, int> * AuthKey option * UserId option
    | SendError of Map<ProductId, int> * AuthKey option
    | Sent of AuthKey option * UserId option * Deferred<OrderSummary>

type Model =
    {
        Products: Deferred<ProductGroup list>
        Order: OrderState
    }

type Msg =
    | LoadProducts
    | LoadProductsResult of Result<ProductGroup list, string>
    | ChangeOrderAmount of ProductId * delta: int
    | ResetOrder
    | Authenticate
    | SetAuthKey of Result<AuthKey, React.AuthenticationError>
    | LoadUsers of AuthKey option
    | LoadUsersResult of Result<UserInfo list, ApiError<LoadUsersError>>
    | SendOrder of AuthKey option * UserId option
    | SendOrderResult of Result<unit, ApiError<AddOrderError list>>
    | LoadOrderSummary
    | LoadOrderSummaryResult of Result<OrderSummary, ApiError<LoadOrderSummaryError>>
    | CloseSendOrder

let init =
    let state =
        {
            Products = Deferred.HasNotStartedYet
            Order = Drafting Map.empty
        }
    state, Cmd.ofMsg LoadProducts

let update msg (state: Model) =
    match msg with
    | LoadProducts ->
        { state with Products = Deferred.InProgress },
        Cmd.OfAsync.perform (fun () -> loadProducts) () LoadProductsResult
    | LoadProductsResult (Ok v) ->
        { state with Products = Deferred.Resolved v },
        Cmd.none
    | LoadProductsResult (Error e) ->
        { state with Products = Deferred.Failed (exn "Fehler beim Laden der Artikel.") },
        Cmd.none
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
        | Drafting _ -> { state with Order = Drafting Map.empty }, Cmd.none
        | _ -> state, Cmd.none
    | Authenticate ->
        match state.Order with
        | Drafting order
        | AuthenticationError (order, _)
        | Sending (order, _, _) -> { state with Order = Authenticating order }, Cmd.none
        | _ -> state, Cmd.none
    | SetAuthKey (Ok authKey) ->
        state, Cmd.ofMsg (LoadUsers (Some authKey))
    | SetAuthKey (Error error) ->
        match state.Order with
        | Authenticating order
        | LoadUsersError (order, _) ->
            { state with Order = AuthenticationError (order, error) },
            Cmd.none
        | _ -> state, Cmd.none
    | LoadUsers authKey ->
        match state.Order with
        | Drafting order
        | Authenticating order
        | LoadUsersError (order, _)
        | Sending (order, _, _) ->
            { state with Order = LoadingUsers (order, authKey) },
            Cmd.OfAsync.perform loadUsers authKey LoadUsersResult
        | _ -> state, Cmd.none
    | LoadUsersResult (Ok users) ->
        match state.Order with
        | LoadingUsers (order, authKey) ->
            { state with Order = LoadedUsers (order, authKey, users) },
            Cmd.none
        | _ -> state, Cmd.none
    | LoadUsersResult (Error (ExpectedError LoadUsersError.NotAuthorized)) ->
        match state.Order with
        | LoadingUsers (order, authKey) -> state, Cmd.ofMsg (SendOrder (authKey, None))
        | _ -> state, Cmd.none
    | LoadUsersResult (Error (ExpectedError LoadUsersError.InvalidAuthKey))
    | LoadUsersResult (Error (UnexpectedError _)) ->
        match state.Order with
        | LoadingUsers (order, authKey) ->
            { state with Order = LoadUsersError (order, authKey) },
            Cmd.none
        | _ -> state, Cmd.none
    | SendOrder (authKey, userId) ->
        match state.Order with
        | Drafting order
        | LoadingUsers (order, _)
        | LoadedUsers (order, _, _)
        | SendError (order, _) ->
            { state with Order = Sending (order, authKey, userId) },
            Cmd.OfAsync.perform (sendOrder authKey userId) order SendOrderResult
        | _ -> state, Cmd.none
    | SendOrderResult (Ok _) ->
        match state.Order with
        | Sending (_, authKey, userId) -> { state with Order = Sent (authKey, userId, Deferred.HasNotStartedYet) }, Cmd.ofMsg LoadOrderSummary
        | _ -> state, Cmd.none
    | SendOrderResult (Error (ExpectedError [AddOrderError.NotAuthorized])) ->
        state, Cmd.ofMsg Authenticate
    | SendOrderResult (Error (ExpectedError [AddOrderError.NoOrderUser])) ->
        let authKey =
            match state.Order with
            | Sending (order, authKey, userId) -> authKey
            | _ -> None
        state, Cmd.ofMsg (LoadUsers authKey)
    | SendOrderResult (Error _) ->
        match state.Order with
        | Sending (order, authKey, userId) -> { state with Order = SendError (order, authKey) }, Cmd.none
        | _ -> state, Cmd.none
    | LoadOrderSummary ->
        match state.Order with
        | Sent (authKey, userId, Deferred.HasNotStartedYet)
        | Sent (authKey, userId, Deferred.Failed _) ->
            { state with Order = Sent (authKey, userId, Deferred.InProgress) },
            Cmd.OfAsync.perform (loadOrderSummary authKey) userId LoadOrderSummaryResult
        | _ -> state, Cmd.none
    | LoadOrderSummaryResult (Ok v) ->
        match state.Order with
        | Sent (authKey, userId, Deferred.InProgress) ->
            { state with Order = Sent (authKey, userId, Deferred.Resolved v) },
            Cmd.none
        | _ -> state, Cmd.none
    | LoadOrderSummaryResult (Error _) ->
        match state.Order with
        | Sent (authKey, userId, Deferred.InProgress) ->
            { state with Order = Sent (authKey, userId, Deferred.Failed (exn "Fehler beim Laden der Bestellübersicht")) },
            Cmd.none
        | _ -> state, Cmd.none
    | CloseSendOrder ->
        match state.Order with
        | Drafting _ -> state, Cmd.none
        | Authenticating order -> { state with Order = Drafting order }, Cmd.none
        | AuthenticationError (order, _) -> { state with Order = Drafting order }, Cmd.none
        | LoadingUsers (order, _) -> { state with Order = Drafting order }, Cmd.none
        | LoadUsersError (order, _) -> { state with Order = Drafting order }, Cmd.none
        | LoadedUsers (order, _, _) -> { state with Order = Drafting order }, Cmd.none
        | Sending _ -> state, Cmd.none
        | SendError (order, _) -> { state with Order = Drafting order }, Cmd.none
        | Sent _ -> { state with Order = Drafting Map.empty}, Cmd.none

[<ReactComponent>]
let OrderForm (userButtons: ReactElement list) (adminButtons: ReactElement list) =
    let (state, dispatch) = React.useElmish(init, update, [||])

    let acceptsAuthKey =
        match state.Order with
        | Drafting _ -> false
        | Authenticating _ -> true
        | AuthenticationError _ -> false
        | LoadingUsers _ -> false
        | LoadUsersError _ -> true
        | LoadedUsers _ -> false
        | Sending _ -> false
        | SendError _ -> true
        | Sent _ -> false
    React.useAuthentication acceptsAuthKey (SetAuthKey >> dispatch)

    let productView (product: Product) =
        let order =
            match state.Order with
            | Drafting order
            | Authenticating order
            | AuthenticationError (order, _)
            | LoadingUsers (order, _)
            | LoadUsersError (order, _)
            | LoadedUsers (order, _, _)
            | Sending (order, _, _)
            | SendError (order, _) -> Some order
            | Sent _ -> None
        let amount = order |> Option.bind (Map.tryFind product.Id)
        Html.div [
            prop.className "flex items-center gap-2"
            prop.children [
                Html.span [
                    prop.className "text-3xl"
                    prop.text product.Name
                ]
                Html.hr [
                    prop.className "grow h-px self-center bg-gray-300"
                ]
                Html.span [
                    prop.classes [
                        "text-2xl"
                        if Option.isSome amount then "text-musi-green font-semibold"
                    ]
                    let price =
                        match amount with
                        | Some amount -> decimal amount * product.Price
                        | None -> product.Price 
                    prop.text $"%.2f{price}€"
                ]

                Html.button [
                    prop.className "btn btn-solid btn-red text-3xl"
                    prop.disabled (Option.defaultValue 0 amount <= 0)
                    prop.onClick (fun _ -> dispatch (ChangeOrderAmount(product.Id, -1)))
                    prop.children [
                        Fa.i [ Fa.Solid.Minus ] []
                    ]
                ]

                match amount with
                | Some amount ->
                    Html.span [
                        prop.className "text-2xl"
                        prop.text $"%d{amount}"
                    ]
                | None -> ()

                Html.button [
                    prop.className "btn btn-solid btn-green text-3xl"
                    prop.onClick (fun _ -> dispatch (ChangeOrderAmount(product.Id, 1)))
                    prop.children [
                        Fa.i [ Fa.Solid.Plus ] []
                    ]
                ]
            ]
        ]

    let productGroupView group =
        Html.div [
            prop.className "p-4 shadow-lg rounded-lg"
            prop.children [
                Html.h3 [
                    prop.className "text-4xl font-semibold my-4"
                    prop.text group.Name
                ]
                match group.Products with
                | [] -> View.infoNotification "Keine Artikel vorhanden"
                | products -> Html.div [
                    prop.className "flex flex-col gap-2"
                    prop.children (List.map productView products)
                ]
            ]
        ]

    let userButtons = [
        yield! userButtons
        Html.button [
            prop.className "btn btn-solid btn-red text-xl py-4"
            match state.Order with
            | Drafting order ->
                prop.disabled (Map.isEmpty order)
                prop.onClick (fun _ -> dispatch ResetOrder)
            | _ -> ()
            prop.children [
                Html.span [
                    prop.className "inline-flex gap-2 items-center"
                    prop.children [
                        Fa.i [ Fa.Solid.UndoAlt ] []
                        Html.span [ prop.text "Zurücksetzen" ]
                    ]
                ]
            ]
        ]
        Html.button [
            prop.className "btn btn-solid btn-green text-xl py-4"
            match state.Order with
            | Drafting order ->
                prop.disabled (Map.isEmpty order)
                prop.onClick (fun _ -> dispatch (SendOrder (None, None)))
            | _ -> ()
            prop.children [
                Html.span [
                    prop.className "inline-flex gap-2 items-center"
                    prop.children [
                        Fa.i [ Fa.Solid.EuroSign ] []
                        Html.span [ prop.text "Bestellen" ]
                    ]
                ]
            ]
        ]
    ]

    let errorView =
        View.modal "Bestellung speichern" (fun () -> dispatch CloseSendOrder) [
            Html.div [
                prop.className "flex flex-col items-center gap-2 text-musi-red"
                prop.children [
                    Fa.i [ Fa.Solid.Key; Fa.Size Fa.Fa8x ] []
                    Html.span [
                        prop.className "text-center text-3xl"
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
        | Drafting _ -> Html.none
        | Authenticating _ -> View.modalAuthForm "Bestellung speichern" (fun () -> dispatch CloseSendOrder)
        | AuthenticationError (_, error) -> View.modalAuthError "Bestellungen anzeigen" error (fun () -> dispatch Authenticate) (fun () -> dispatch CloseSendOrder)
        | LoadingUsers _ -> View.modal "Bestellung speichern" (fun () -> dispatch CloseSendOrder) [ View.loadIconBig ] []
        | LoadUsersError _ -> errorView
        | LoadedUsers (_, authKey, users) ->
            View.modal "Bestellung speichern" (fun () -> dispatch CloseSendOrder) [
                View.Order.userCards users (fun user -> dispatch (SendOrder (authKey, Some user.Id)))
            ] []
        | Sending _ -> View.modal "Bestellung speichern" (fun () -> dispatch CloseSendOrder) [ View.loadIconBig ] []
        | SendError _ -> errorView
        | Sent (_, _, loadSummaryState) ->
            View.modal "Bestellung speichern" (fun () -> dispatch CloseSendOrder) [
                Html.div [
                    prop.className "flex flex-col gap-2"
                    prop.children [
                        Html.div [
                            prop.className "flex flex-col items-center gap-2 text-musi-green"
                            prop.children [
                                Html.div [
                                    Fa.i [ Fa.Solid.Check; Fa.Size Fa.Fa8x ] []
                                ]
                                Html.span [
                                    prop.className "text-center text-3xl"
                                    prop.children [
                                        Html.text "Bestellung erfolgreich gespeichert."
                                        Html.br []
                                        Html.text "Prost Mahlzeit"
                                        match loadSummaryState with
                                        | Deferred.Resolved orderSummary ->
                                            Html.text ", "
                                            Html.span [
                                                prop.className "text-musi-green"
                                                prop.text orderSummary.ClientFullName
                                            ]
                                        | _ -> ()
                                        Html.text "!"
                                    ]
                                ]
                            ]
                        ]
                        match loadSummaryState with
                        | Deferred.HasNotStartedYet
                        | Deferred.InProgress -> View.loadIconBig
                        | Deferred.Failed error ->
                            View.errorNotificationWithRetry error.Message (fun () -> dispatch LoadOrderSummary)
                        | Deferred.Resolved orderSummary ->
                            View.Order.orderSummary orderSummary
                    ]
                ]
            ] []

    React.fragment [
        match state.Products with
        | Deferred.HasNotStartedYet ->
            ()
        | Deferred.InProgress ->
            Html.div [
                prop.className "p-8"
                prop.children [ View.loadIconBig ]
            ]
        | Deferred.Failed error ->
            Html.div [
                prop.className "p-8"
                prop.children [ View.errorNotificationWithRetry error.Message (fun () -> dispatch LoadProducts) ]
            ]
        | Deferred.Resolved [] ->
            Html.div [
                prop.className "p-8"
                prop.children [ View.errorNotificationWithRetry "No products available." (fun () -> dispatch LoadProducts) ]
            ]
        | Deferred.Resolved productGroups ->
            Html.div [
                prop.className "overflow-y-auto grow p-8"
                prop.children [
                    Html.div [
                        prop.className "container flex flex-col gap-2"
                        prop.children [
                            match productGroups with
                            | [] -> View.infoNotification "Keine Artikel vorhanden"
                            | productGroups -> yield! List.map productGroupView productGroups
                        ]
                    ]
                ]
            ]
            Html.div [
                prop.className "grow-0 p-8"
                prop.children [
                    Html.div [
                        prop.className "container"
                        prop.children [
                            Html.div [
                                prop.className "flex justify-between"
                                prop.children [
                                    Html.div [
                                        prop.className "grid grid-cols-3 items-end gap-2"
                                        prop.children userButtons
                                    ]
                                    Html.div [
                                        prop.className "flex items-end gap-2"
                                        prop.children adminButtons
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]

        authForm
    ]
