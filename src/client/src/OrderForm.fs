module OrderForm

open Api
open Api.Order
open Elmish
open Fable.FontAwesome
open Feliz
open Feliz.Bulma
open Feliz.Bulma.Operators
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
                                    prop.textf "%.2f€" (decimal amount * product.Price)
                                ]
                            | None ->
                                Bulma.title.p [
                                    title.is5
                                    prop.textf "%.2f€" (decimal product.Price)
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
            Bulma.title.h3 [ prop.text group.Name ]
            match group.Products with
            | [] -> View.infoNotification "Keine Artikel vorhanden"
            | products -> yield! List.map productView products
        ]

    let userButtons = Html.div [
        prop.className "flex gap-2"
        prop.children [
            Html.button [
                prop.className "btn btn-solid btn-red text-2xl py-4"
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
                prop.className "btn btn-solid btn-green text-2xl py-4"
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
            yield! userButtons
        ]
    ]

    let errorView =
        View.modal "Bestellung speichern" (fun () -> dispatch CloseSendOrder) [
            Bulma.container [
                text.hasTextCentered
                ++ color.hasTextDanger
                ++ spacing.px2
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
        | Drafting _ -> Html.none
        | Authenticating _ -> View.modalAuthForm "Bestellung speichern" (fun () -> dispatch CloseSendOrder)
        | AuthenticationError (_, error) -> View.modalAuthError "Bestellungen anzeigen" error (fun () -> dispatch Authenticate) (fun () -> dispatch CloseSendOrder)
        | LoadingUsers _ -> View.modal "Bestellung speichern" (fun () -> dispatch CloseSendOrder) [ View.loadIconBig ] []
        | LoadUsersError _ -> errorView
        | LoadedUsers (_, authKey, users) ->
            View.modal "Bestellung speichern" (fun () -> dispatch CloseSendOrder) [
                Bulma.table [
                    table.isFullWidth
                    prop.children [
                        Html.thead [
                            Html.tr [
                                Html.th [ prop.text "Nachname" ]
                                Html.th [ prop.text "Vorname" ]
                                Html.th [ prop.text "Aktuelles Guthaben" ]
                                Html.th []
                            ]
                        ]
                        Html.tbody [
                            for user in users ->
                                Html.tr [
                                    prop.onClick (fun _ -> dispatch (SendOrder (authKey, Some user.Id)))

                                    prop.children [
                                        Html.td [
                                            text.hasTextLeft
                                            ++ text.isUppercase
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
                ]
            ] []
        | Sending _ -> View.modal "Bestellung speichern" (fun () -> dispatch CloseSendOrder) [ View.loadIconBig ] []
        | SendError _ -> errorView
        | Sent (_, _, loadSummaryState) ->
            View.modal "Bestellung speichern" (fun () -> dispatch CloseSendOrder) [
                Bulma.container [
                    text.hasTextCentered
                    ++ spacing.px2
                    ++ color.hasTextSuccess
                    prop.children [
                        Html.div [
                            Fa.i [ Fa.Solid.Check; Fa.Size Fa.Fa8x ] []
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
                    text.hasTextCentered
                    prop.children [
                        match loadSummaryState with
                        | Deferred.HasNotStartedYet
                        | Deferred.InProgress -> View.loadIconBig
                        | Deferred.Failed error ->
                            View.errorNotificationWithRetry error.Message (fun () -> dispatch LoadOrderSummary)
                        | Deferred.Resolved orderSummary ->
                            yield! View.Order.orderSummary orderSummary
                    ]
                ]
            ] []

    React.fragment [
        match state.Products with
        | Deferred.HasNotStartedYet ->
            ()
        | Deferred.InProgress ->
            Bulma.section [ Bulma.progress [ color.isPrimary ] ]
        | Deferred.Failed error ->
            Bulma.section [ View.errorNotificationWithRetry error.Message (fun () -> dispatch LoadProducts) ]
        | Deferred.Resolved [] ->
            Bulma.section [ View.errorNotificationWithRetry "No products available." (fun () -> dispatch LoadProducts) ]
        | Deferred.Resolved productGroups ->
            Bulma.section [
                prop.className "main-content"
                prop.children [
                    Bulma.container [
                        match productGroups with
                        | [] -> View.infoNotification "Keine Artikel vorhanden"
                        | productGroups -> yield! List.map productGroupView productGroups
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
                                    Bulma.buttons adminButtons
                                ]
                            ]
                        ]
                    ]
                ]
            ]

        authForm
    ]
