module OrderAdministration

open Api
open Elmish
open Fable.Core.JsInterop
open Fable.FontAwesome
open Feliz
open Feliz.Bulma
open Feliz.UseDeferred
open Feliz.UseElmish
open global.JS
open MusiOrder.Models

type LoadedModel = {
    Orders: OrderInfo list
    DeleteOrderState: Map<string, Deferred<unit>>
}

type Model =
    | NotLoaded
    | Loading of AuthKey
    | LoadError of AuthKey * ApiError<LoadOrderInfoError>
    | Loaded of AuthKey * LoadedModel

type Msg =
    | Load of AuthKey
    | LoadResult of Result<OrderInfo list, ApiError<LoadOrderInfoError>>
    | DeleteOrder of orderId: string
    | DeleteOrderResult of orderId: string * Result<unit, ApiError<DeleteOrderError>>

let init authKey =
    match authKey with
    | Some authKey ->
        NotLoaded, Cmd.ofMsg (Load authKey)
    | None ->
        NotLoaded, Cmd.none

let update msg state =
    match msg with
    | Load authKey ->
        Loading authKey, Cmd.OfAsync.perform loadOrderInfo authKey LoadResult
    | LoadResult (Ok orders) ->
        match state with
        | Loading authKey ->
            Loaded (authKey, { Orders = orders; DeleteOrderState = Map.empty }),
            Cmd.none
        | _ -> state, Cmd.none
    | LoadResult (Error e) ->
        match state with
        | Loading authKey -> LoadError (authKey, e), Cmd.none
        | _ -> state, Cmd.none
    | DeleteOrder orderId ->
        match state with
        | Loaded (authKey, state) ->
            Loaded (authKey, { state with DeleteOrderState = Map.add orderId Deferred.InProgress state.DeleteOrderState }),
            Cmd.OfAsync.perform (deleteOrder authKey) orderId (fun v -> DeleteOrderResult (orderId, v))
        | _ -> state, Cmd.none
    | DeleteOrderResult (orderId, Ok ()) ->
        match state with
        | Loaded (authKey, state) ->
            Loaded (authKey, { state with DeleteOrderState = Map.add orderId (Deferred.Resolved ()) state.DeleteOrderState }),
            Cmd.none
        | _ -> state, Cmd.none
    | DeleteOrderResult (orderId, Error e) ->
        match state with
        | Loaded (authKey, state) ->
            let errorMessage =
                match e with
                | ExpectedError DeleteOrderError.InvalidAuthKey
                | ExpectedError DeleteOrderError.NotAuthorized
                | UnexpectedError _ -> "Fehler beim Löschen der Bestellung"
            Loaded (authKey, { state with DeleteOrderState = Map.add orderId (Deferred.Failed (exn errorMessage)) state.DeleteOrderState }),
            Cmd.none
        | _ -> state, Cmd.none

[<ReactComponent>]
let OrderAdministration authKey setAuthKeyInvalid (setMenuItems: ReactElement list -> ReactElement) =
    let (state, dispatch) = React.useElmish(init authKey, update, [| authKey :> obj |])

    match state with
    | NotLoaded -> Html.none // Handled by parent component
    | Loading _ -> View.loadIconBig
    | LoadError (_, ExpectedError LoadOrderInfoError.InvalidAuthKey)
    | LoadError (_, ExpectedError LoadOrderInfoError.NotAuthorized) ->
        setAuthKeyInvalid ()
        Html.none // Handled by parent component
    | LoadError (authKey, UnexpectedError _) ->
        View.errorNotificationWithRetry "Fehler beim Laden der Daten." (fun () -> dispatch (Load authKey))
    | Loaded (_, { Orders = [] }) ->
        View.infoNotification "Keine Bestellungen vorhanden"
    | Loaded (_, state) ->
        Bulma.container [
            Bulma.table [
                table.isFullWidth
                prop.children [
                    Html.thead [
                        Html.tr [
                            Html.th [ prop.text "Nachname" ]
                            Html.th [ prop.text "Vorname" ]
                            Html.th [ prop.text "Bestellung" ]
                            Html.th [ prop.text "Zeitpunkt" ]
                            Html.th [ prop.style [ style.width (length.px 150) ] ]
                        ]
                    ]
                    Html.tbody [
                        for order in state.Orders ->
                            let deleteOrderState = Map.tryFind order.Id state.DeleteOrderState
                            Html.tr [
                                if deleteOrderState = Some (Deferred.Resolved ()) then
                                    color.hasTextGreyLight
                                prop.children [
                                    Html.td [
                                        text.isUppercase
                                        prop.text order.LastName
                                    ]
                                    Html.td [
                                        prop.text order.FirstName
                                    ]
                                    Html.td [
                                        prop.textf "%d x %s à %.2f€" order.Amount order.ArticleName order.PricePerUnit
                                    ]
                                    Html.td [
                                        let relativeTime: string = moment(order.Timestamp)?fromNow()
                                        let timestamp: string = moment(order.Timestamp)?format("LLLL")
                                        prop.text (sprintf "%s (%s)" timestamp relativeTime)
                                    ]
                                    Html.td [
                                        Bulma.level [
                                            Bulma.levelLeft [
                                                Bulma.levelItem [
                                                    Bulma.button.a [
                                                        color.isDanger
                                                        prop.onClick (fun _ -> dispatch (DeleteOrder order.Id))
                                                        
                                                        match deleteOrderState with
                                                        | Some Deferred.InProgress
                                                        | Some (Deferred.Resolved ()) -> prop.disabled true
                                                        | _ -> ()

                                                        prop.children [
                                                            Bulma.icon [ Fa.i [ Fa.Solid.TrashAlt ] [] ]
                                                        ]
                                                        if deleteOrderState = Some Deferred.InProgress then
                                                            button.isLoading
                                                    ]
                                                ]
                                                let icon iconProps faProps =
                                                    Bulma.levelItem [
                                                        Bulma.icon [
                                                            control.isMedium
                                                            yield! iconProps
                                                            prop.children [
                                                                Fa.i [
                                                                    Fa.Size Fa.FaLarge
                                                                    yield! faProps
                                                                ] []
                                                            ]
                                                        ]
                                                    ]
                                                match deleteOrderState with
                                                | Some Deferred.HasNotStartedYet -> ()
                                                | Some Deferred.InProgress -> icon [ color.hasTextPrimary ] [ Fa.Solid.Spinner; Fa.Pulse ]
                                                | Some (Deferred.Failed e) -> icon [ color.hasTextDanger; prop.title e.Message ] [ Fa.Solid.Times ]
                                                | Some (Deferred.Resolved _) -> icon [ color.hasTextSuccess ] [ Fa.Solid.Check ]
                                                | None -> ()
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                    ]
                ]
            ]
        ]
