module OrderStatistics

open Api
open Api.OrderStatistics
open Elmish
open Feliz
open Feliz.UseElmish
open global.JS
open MusiOrder.Models
open MusiOrder.Models.OrderStatistics

type LoadableData =
    | NotLoaded
    | Loading of AuthKey option
    | LoadError of AuthKey option * ApiError<LoadOrderInfoError>
    | Loaded of AuthKey option * OrderInfo list

type Model = {
    Period: System.DateTime * System.DateTime
    Data: LoadableData
}

type Msg =
    | Load of AuthKey option
    | LoadResult of Result<OrderInfo list, ApiError<LoadOrderInfoError>>
    | SetPeriod of System.DateTime option * System.DateTime option 

let init authKey =
    let model = {
        Period =
            let d = System.DateTime.Today
            System.DateTime(d.Year, d.Month, 1),
            d.AddDays 1.
        Data = NotLoaded
    }
    model, Cmd.ofMsg (Load authKey)

let update msg state =
    match msg with
    | Load authKey ->
        { state with Data = Loading authKey }, Cmd.OfAsync.perform (loadOrderInfo authKey) state.Period LoadResult
    | LoadResult (Ok orders) ->
        match state.Data with
        | Loading authKey ->
            { state with Data = Loaded (authKey, orders) },
            Cmd.none
        | _ -> state, Cmd.none
    | LoadResult (Error e) ->
        match state.Data with
        | Loading authKey -> { state with Data = LoadError (authKey, e) }, Cmd.none
        | _ -> state, Cmd.none
    | SetPeriod (startTime, endTime) ->
        match state.Data with
        | Loaded (authKey, _) ->
            let period = 
                startTime |> Option.defaultValue (fst state.Period),
                endTime |> Option.defaultValue (snd state.Period)
            { state with Period = period }, Cmd.ofMsg (Load authKey)
        | _ -> state, Cmd.none

[<ReactComponent>]
let OrderStatistics authKey setAuthKeyInvalid (setMenuItems: ReactElement list -> ReactElement) =
    let (state, dispatch) = React.useElmish(init authKey, update, [| authKey :> obj |])

    React.useEffect(fun () ->
        match state.Data with
        | LoadError (_, ExpectedError LoadOrderInfoError.InvalidAuthKey)
        | LoadError (_, ExpectedError LoadOrderInfoError.NotAuthorized) ->
            setAuthKeyInvalid ()
        | _ -> ()
    )

    let timeRangeSelection = Html.div [
            prop.className "container"
            prop.children [
                Html.div [
                    prop.className "flex items-center gap-2"
                    prop.children [
                        Html.span "Zeitraum:"
                        Html.input [
                            prop.type' "date"
                            prop.value (fst state.Period)
                            prop.onTextChange (fun v ->
                                let d = System.DateTime.Parse v
                                SetPeriod (Some d, None) |> dispatch
                            )
                        ]
                        Html.span "bis"
                        Html.input [
                            prop.type' "date"
                            prop.value (snd state.Period)
                            prop.onTextChange (fun v ->
                                let d = System.DateTime.Parse v
                                SetPeriod (None, Some d) |> dispatch
                            )
                        ]
                        Html.a [
                            prop.className "btn btn-blue" 
                            prop.text "Aktuelles Monat"
                            prop.onClick (fun _ ->
                                let d = System.DateTime.Today
                                SetPeriod (Some (System.DateTime(d.Year, d.Month, 1)), Some (d.AddDays 1.)) |> dispatch
                            )
                        ]
                        Html.a [
                            prop.className "btn btn-blue" 
                            prop.text "Letztes Monat"
                            prop.onClick (fun _ ->
                                let d = System.DateTime.Today
                                let firstOfMonth = System.DateTime(d.Year, d.Month, 1)
                                let lastOfLastMonth = firstOfMonth.AddDays -1.
                                let firstOfLastMonth = System.DateTime(lastOfLastMonth.Year, lastOfLastMonth.Month, 1)
                                SetPeriod (Some firstOfLastMonth, Some firstOfMonth) |> dispatch
                            )
                        ]
                    ]
                ]
            ]
        ]

    Html.div [
        prop.className "flex flex-col gap-2"
        prop.children [
            timeRangeSelection
            match state.Data with
            | NotLoaded -> Html.none // Handled by parent component
            | Loading _ -> View.loadIconBig
            | LoadError (_, ExpectedError LoadOrderInfoError.InvalidAuthKey)
            | LoadError (_, ExpectedError LoadOrderInfoError.NotAuthorized) -> Html.none // Handled by parent component
            | LoadError (_, ExpectedError LoadOrderInfoError.MissingTimeRange) ->
                View.infoNotification "Der ausgewählte Zeitbereich ist ungültig." []
            | LoadError (authKey, UnexpectedError _) ->
                View.errorNotificationWithRetry "Fehler beim Laden der Daten." (fun () -> dispatch (Load authKey))
            | Loaded (_, []) ->
                View.infoNotification "Keine Bestellungen vorhanden." []
            | Loaded (_, data) ->
                Html.div [
                    prop.className "container"
                    prop.children [
                        Html.table [
                            prop.className "min-w-[640px]"
                            prop.children [
                                Html.thead [
                                    Html.tr [
                                        Html.th [
                                            prop.className "text-right"
                                            prop.text "Anzahl"
                                        ]
                                        Html.th "Name"
                                        Html.th [
                                            prop.className "text-right"
                                            prop.text "Umsatz"
                                        ]
                                    ]
                                ]
                                let orderGroups =
                                    data
                                    |> List.groupBy (fun v -> v.ProductName)
                                    |> List.map (fun (name, orders) -> {|
                                        Name = name
                                        Amount = orders |> List.sumBy (fun v -> v.Amount)
                                        Revenue = orders |> List.sumBy (fun v -> decimal v.Amount * v.PricePerUnit)
                                    |})
                                    |> List.sortByDescending (fun v -> v.Amount)
                                Html.tbody [
                                    for group in orderGroups do
                                        Html.tr [
                                            Html.td [
                                                prop.className "text-right"
                                                prop.text group.Amount
                                            ]
                                            Html.td group.Name
                                            Html.td [
                                                prop.className "text-right"
                                                prop.text (group.Revenue |> View.formatPrice)
                                            ]
                                        ]
                                ]
                                Html.tfoot [
                                    Html.tr [
                                        Html.td [
                                            prop.className "text-right"
                                            prop.text (orderGroups |> List.sumBy (fun v -> v.Amount))
                                        ]
                                        Html.td []
                                        Html.td [
                                            prop.className "text-right"
                                            prop.text (orderGroups |> List.sumBy (fun v -> v.Revenue) |> View.formatPrice)
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
        ]
    ]
