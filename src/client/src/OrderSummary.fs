module OrderSummary

open Api
open Elmish
open Fable.FontAwesome
open Feliz
open Feliz.Bulma
open Feliz.UseElmish
open MusiOrder.Models

type Model =
    | Hidden
    | Authenticating
    | Loading of AuthKey
    | LoadError of exn
    | Loaded of OrderSummary

type Msg =
    | Show
    | Load of AuthKey
    | LoadResult of Result<OrderSummary, exn>
    | Close

let init = Hidden, Cmd.none

let update msg state =
    match msg with
    | Show -> Authenticating, Cmd.none
    | Load authKey -> Loading authKey, Cmd.OfAsync.either loadOrderSummary authKey (Ok >> LoadResult) (Error >> LoadResult)
    | LoadResult (Ok orderSummary) -> Loaded orderSummary, Cmd.none
    | LoadResult (Error e) -> LoadError e, Cmd.none
    | Close -> Hidden, Cmd.none

let view = React.functionComponent (fun () ->
    let (state, dispatch) = React.useElmish(init, update, [||])
    let acceptsAuthKey =
        match state with
        | Authenticating
        | LoadError -> true
        | _ -> false
    React.useAuthentication acceptsAuthKey (Load >> dispatch)

    let authForm =
        match state with
        | Hidden -> Html.none
        | Authenticating -> View.modalAuthForm "Bestellungen anzeigen" (fun () -> dispatch Close)
        | Loading -> View.modal "Bestellungen anzeigen" (fun () -> dispatch Close) [ View.loadIconBig ] []
        | LoadError ->
            View.modal "Bestellungen anzeigen" (fun () -> dispatch Close) [
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
            ] []
        | Loaded orderSummary ->
            View.modal (sprintf "Bestellungen von %s" orderSummary.ClientFullName) (fun () -> dispatch Close) [
                Bulma.container (View.orderSummary orderSummary)
            ] []

    [
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
)
