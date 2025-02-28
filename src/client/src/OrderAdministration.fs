module OrderAdministration

open Api
open Api.OrderAdministration
open Elmish
open Fable.Core.JsInterop
open Feliz
open Feliz.UseDeferred
open Feliz.UseElmish
open global.JS
open MusiOrder.Models
open MusiOrder.Models.OrderAdministration

type LoadedModel = {
    Orders: OrderInfo list
    DeleteOrderState: Map<OrderId, Deferred<unit>>
}

type Model =
    | NotLoaded
    | Loading of AuthKey option
    | LoadError of AuthKey option * ApiError<LoadOrderInfoError>
    | Loaded of AuthKey option * LoadedModel

type Msg =
    | Load of AuthKey option
    | LoadResult of Result<OrderInfo list, ApiError<LoadOrderInfoError>>
    | DeleteOrder of OrderId
    | DeleteOrderResult of OrderId * Result<unit, ApiError<DeleteOrderError>>

let init authKey =
    NotLoaded, Cmd.ofMsg (Load authKey)

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

    React.useEffect(fun () ->
        match state with
        | LoadError (_, ExpectedError LoadOrderInfoError.InvalidAuthKey)
        | LoadError (_, ExpectedError LoadOrderInfoError.NotAuthorized) ->
            setAuthKeyInvalid ()
        | _ -> ()
    )

    match state with
    | NotLoaded -> Html.none // Handled by parent component
    | Loading _ -> View.loadIconBig
    | LoadError (_, ExpectedError LoadOrderInfoError.InvalidAuthKey)
    | LoadError (_, ExpectedError LoadOrderInfoError.NotAuthorized) -> Html.none // Handled by parent component
    | LoadError (authKey, UnexpectedError _) ->
        View.errorNotificationWithRetry "Fehler beim Laden der Daten." (fun () -> dispatch (Load authKey))
    | Loaded (_, { Orders = [] }) ->
        View.infoNotification "Keine Bestellungen vorhanden." []
    | Loaded (_, state) ->
        Html.div [
            prop.className "container"
            prop.children [
                Html.table [
                    prop.className "w-full"
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
                                    prop.classes [
                                        if deleteOrderState = Some (Deferred.Resolved ()) then "opacity-50"
                                    ]
                                    prop.children [
                                        Html.td [
                                            prop.className "uppercase"
                                            prop.text order.LastName
                                        ]
                                        Html.td [
                                            prop.text order.FirstName
                                        ]
                                        Html.td [
                                            prop.text $"%d{order.Amount} x %s{order.ProductName} à %s{View.formatPrice order.PricePerUnit}"
                                        ]
                                        Html.td [
                                            let relativeTime: string = moment(order.Timestamp)?fromNow()
                                            let timestamp: string = moment(order.Timestamp)?format("LLLL")
                                            prop.text $"%s{timestamp} (%s{relativeTime})"
                                        ]
                                        Html.td [
                                            Html.div [
                                                prop.className "flex items-center gap-2"
                                                prop.children [
                                                    Html.button [
                                                        prop.className "btn btn-solid btn-red"
                                                        prop.onClick (fun _ -> dispatch (DeleteOrder order.Id))
                                                        
                                                        match deleteOrderState with
                                                        | Some Deferred.InProgress
                                                        | Some (Deferred.Resolved ()) -> prop.disabled true
                                                        | _ -> ()

                                                        prop.children [
                                                            Html.i [ prop.className "fas fa-trash-alt" ]
                                                        ]
                                                    ]
                                                    match deleteOrderState with
                                                    | Some Deferred.HasNotStartedYet -> ()
                                                    | Some Deferred.InProgress -> Html.i [ prop.className "fa-lg fas fa-spinner fa-pulse text-musi-gold" ]
                                                    | Some (Deferred.Failed e) -> Html.i [ prop.className "fa-lg fas fa-times text-musi-red"; prop.title e.Message ]
                                                    | Some (Deferred.Resolved _) -> Html.i [ prop.className "fa-lg fas fa-check text-musi-green" ]
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
        ]
