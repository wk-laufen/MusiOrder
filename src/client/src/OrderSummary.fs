module OrderSummary

open Api
open Api.Order
open Elmish
open Fable.FontAwesome
open Feliz
open Feliz.Bulma
open Feliz.Bulma.Operators
open Feliz.UseElmish
open MusiOrder.Models
open MusiOrder.Models.Order

type Model =
    | Hidden
    | Authenticating
    | AuthenticationError of React.AuthenticationError
    | LoadingUsers of AuthKey option
    | LoadUsersError of AuthKey option
    | LoadedUsers of AuthKey option * UserInfo list
    | LoadingOrderSummary of AuthKey option
    | LoadOrderSummaryError of ApiError<LoadOrderSummaryError>
    | LoadedOrderSummary of OrderSummary

type Msg =
    | Show
    | SetAuthKey of Result<AuthKey, React.AuthenticationError>
    | LoadUsers of AuthKey option
    | LoadUsersResult of Result<UserInfo list, ApiError<LoadUsersError>>
    | LoadOrderSummary of AuthKey option * UserId option
    | LoadOrderSummaryResult of Result<OrderSummary, ApiError<LoadOrderSummaryError>>
    | Close

let init = Hidden, Cmd.none

let update msg state =
    match msg with
    | Show -> state, Cmd.ofMsg (LoadOrderSummary (None, None))
    | SetAuthKey (Ok authKey) -> state, Cmd.ofMsg (LoadOrderSummary (Some authKey, None))
    | SetAuthKey (Error error) -> AuthenticationError error, Cmd.none
    | LoadUsers authKey -> LoadingUsers authKey, Cmd.OfAsync.perform loadUsers authKey LoadUsersResult
    | LoadUsersResult (Ok users) ->
        match state with
        | LoadingUsers authKey -> LoadedUsers (authKey, users), Cmd.none
        | _ -> state, Cmd.none
    | LoadUsersResult (Error (ExpectedError LoadUsersError.NotAuthorized)) ->
        match state with
        | LoadingUsers authKey -> state, Cmd.ofMsg (LoadOrderSummary (authKey, None))
        | _ -> state, Cmd.none
    | LoadUsersResult (Error (ExpectedError LoadUsersError.InvalidAuthKey))
    | LoadUsersResult (Error (UnexpectedError _)) ->
        match state with
        | LoadingUsers authKey -> LoadUsersError authKey, Cmd.none
        | _ -> state, Cmd.none
    | LoadOrderSummary (authKey, userId) ->
        LoadingOrderSummary authKey, Cmd.OfAsync.perform (loadOrderSummary authKey) userId LoadOrderSummaryResult
    | LoadOrderSummaryResult (Ok orderSummary) -> LoadedOrderSummary orderSummary, Cmd.none
    | LoadOrderSummaryResult (Error (ExpectedError LoadOrderSummaryError.NotAuthorized)) -> Authenticating, Cmd.none
    | LoadOrderSummaryResult (Error (ExpectedError LoadOrderSummaryError.NoOrderSummaryUser)) ->
        let authKey =
            match state with
            | LoadingOrderSummary authKey -> authKey
            | _ -> None
        state, Cmd.ofMsg (LoadUsers authKey)
    | LoadOrderSummaryResult (Error e) -> LoadOrderSummaryError e, Cmd.none
    | Close -> Hidden, Cmd.none

[<ReactComponent>]
let OrderSummary () =
    let (state, dispatch) = React.useElmish(init, update, [||])
    let acceptsAuthKey =
        match state with
        | Authenticating
        | LoadOrderSummaryError _ -> true
        | Hidden
        | AuthenticationError _
        | LoadingUsers _
        | LoadedUsers _
        | LoadUsersError _
        | LoadedOrderSummary _
        | LoadingOrderSummary _ -> false
    React.useAuthentication acceptsAuthKey (SetAuthKey >> dispatch)

    let authForm =
        match state with
        | Hidden -> Html.none
        | Authenticating -> View.modalAuthForm "Bestellungen anzeigen" (fun () -> dispatch Close)
        | AuthenticationError error -> View.modalAuthError "Bestellungen anzeigen" error (fun () -> dispatch Show) (fun () -> dispatch Close)
        | LoadingUsers _
        | LoadingOrderSummary _ -> View.modal "Bestellungen anzeigen" (fun () -> dispatch Close) [ View.loadIconBig ] []
        | LoadedUsers (authKey, users) ->
            View.modal "Bestellungen anzeigen" (fun () -> dispatch Close) [
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
                                    prop.onClick (fun _ -> dispatch (LoadOrderSummary (authKey, Some user.Id)))

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
                                            View.bulmaBalanceColor user.Balance
                                            prop.textf "%.2f€" user.Balance
                                        ]
                                    ]
                                ]
                        ]
                    ]
                ]
            ] []
        | LoadUsersError _
        | LoadOrderSummaryError _ ->
            View.modal "Bestellungen anzeigen" (fun () -> dispatch Close) [
                Bulma.container [
                    text.hasTextCentered
                    ++ color.hasTextDanger
                    ++ spacing.px2
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
            ] []
        | LoadedOrderSummary orderSummary ->
            View.modal (sprintf "Bestellungen von %s" orderSummary.ClientFullName) (fun () -> dispatch Close) [
                View.Order.orderSummary orderSummary
            ] []

    React.fragment [
        Html.button [
            prop.className "btn btn-solid btn-blue text-2xl py-4"
            prop.onClick (fun _ -> dispatch Show)
            prop.children [
                Html.span [
                    prop.className "inline-flex gap-2 items-center"
                    prop.children [
                        Fa.i [ Fa.Solid.FileAlt ] []
                        Html.span [ prop.text "Meine Bestellungen" ]
                    ]
                ]
            ]
        ]
        authForm
    ]
