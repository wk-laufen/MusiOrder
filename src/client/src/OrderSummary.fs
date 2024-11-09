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
    | Loading of AuthKey
    | LoadError of ApiError<LoadOrderSummaryError>
    | Loaded of OrderSummary

type Msg =
    | Show
    | SetAuthKey of Result<AuthKey, React.AuthenticationError>
    | LoadResult of Result<OrderSummary, ApiError<LoadOrderSummaryError>>
    | Close

let init = Hidden, Cmd.none

let update msg state =
    match msg with
    | Show -> Authenticating, Cmd.none
    | SetAuthKey (Ok authKey) -> Loading authKey, Cmd.OfAsync.perform (loadOrderSummary authKey) None LoadResult
    | SetAuthKey (Error error) -> AuthenticationError error, Cmd.none
    | LoadResult (Ok orderSummary) -> Loaded orderSummary, Cmd.none
    | LoadResult (Error e) -> LoadError e, Cmd.none
    | Close -> Hidden, Cmd.none

[<ReactComponent>]
let OrderSummary () =
    let (state, dispatch) = React.useElmish(init, update, [||])
    let acceptsAuthKey =
        match state with
        | Authenticating
        | LoadError _ -> true
        | Hidden
        | AuthenticationError _
        | Loaded _
        | Loading _ -> false
    React.useAuthentication acceptsAuthKey (SetAuthKey >> dispatch)

    let authForm =
        match state with
        | Hidden -> Html.none
        | Authenticating -> View.modalAuthForm "Bestellungen anzeigen" (fun () -> dispatch Close)
        | AuthenticationError error -> View.modalAuthError "Bestellungen anzeigen" error (fun () -> dispatch Show) (fun () -> dispatch Close)
        | Loading _ -> View.modal "Bestellungen anzeigen" (fun () -> dispatch Close) [ View.loadIconBig ] []
        | LoadError _ ->
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
        | Loaded orderSummary ->
            View.modal (sprintf "Bestellungen von %s" orderSummary.ClientFullName) (fun () -> dispatch Close) [
                Bulma.container [
                    text.hasTextCentered
                    prop.children (View.Order.orderSummary orderSummary)
                ]
            ] []

    React.fragment [
        Bulma.button.button [
            color.isInfo
            prop.onClick (fun _ -> dispatch Show)
            prop.children [
                Bulma.icon [ Fa.i [ Fa.Solid.FileAlt ] [] ]
                Html.span [ prop.text "Meine Bestellungen" ]
            ]
        ]
        authForm
    ]
