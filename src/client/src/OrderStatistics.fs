module OrderStatistics

open Api
open Api.OrderStatistics
open Elmish
open Fable.Core
open Fable.Core.JS
open Feliz
open Feliz.UseElmish
open global.JS
open MusiOrder.Models
open MusiOrder.Models.OrderStatistics

type OrderData = {
    Orders: OrderInfo list
    UserSelectionVisible: bool
    SelectedUsers: Collections.Set<string * string>
}

type LoadableData =
    | NotLoaded
    | Loading of AuthKey option
    | LoadError of AuthKey option * ApiError<LoadOrderInfoError>
    | Loaded of AuthKey option * OrderData

type Model = {
    Period: System.DateTime * System.DateTime
    Data: LoadableData
}

type Msg =
    | Load of AuthKey option
    | LoadResult of Result<OrderInfo list, ApiError<LoadOrderInfoError>>
    | SetPeriod of System.DateTime option * System.DateTime option
    | ShowUserSelection
    | SetUserSelection of Collections.Set<string * string>
    | HideUserSelection
    | CreateExcelReport
    | CreateExcelReportFinished

type GroupedData = {
    Name: string
    Amount: int
    Revenue: decimal
}

let filterByUsersAndGroupByProduct data users =
    data
    |> List.filter (fun v -> Collections.Set.isEmpty users || Collections.Set.contains (v.LastName, v.FirstName) users)
    |> List.groupBy (fun v -> v.ProductName)
    |> List.map (fun (name, orders) -> {
        Name = name
        Amount = orders |> List.sumBy (fun v -> v.Amount)
        Revenue = orders |> List.sumBy (fun v -> decimal v.Amount * v.PricePerUnit)
    })

[<Import("createAndDownloadReport", "./order-report.js")>]
let createAndDownloadReport (orders: GroupedData array, fileName: string) : Promise<unit> = jsNative

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
            { state with Data = Loaded (authKey, { Orders = orders; UserSelectionVisible = false; SelectedUsers = Collections.Set.empty }) },
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
    | ShowUserSelection ->
        match state.Data with
        | Loaded (authKey, data) ->
            { state with Data = Loaded (authKey, { data with UserSelectionVisible = true }) }, Cmd.none
        | _ -> state, Cmd.none
    | SetUserSelection users ->
        match state.Data with
        | Loaded (authKey, data) ->
            { state with Data = Loaded (authKey, { data with SelectedUsers = users }) }, Cmd.none
        | _ -> state, Cmd.none
    | HideUserSelection ->
        match state.Data with
        | Loaded (authKey, data) ->
            { state with Data = Loaded (authKey, { data with UserSelectionVisible = false }) }, Cmd.none
        | _ -> state, Cmd.none
    | CreateExcelReport ->
        match state.Data with
        | Loaded (_, data) ->
            let orderGroups =
                filterByUsersAndGroupByProduct data.Orders data.SelectedUsers
                |> List.sortBy (fun v -> v.Name)
                |> List.toArray
            let fileName =
                let startDate = (fst state.Period).ToString "yyyy-MM-dd"
                let endDate = (snd state.Period).ToString "yyyy-MM-dd"
                $"Bestellbericht %s{startDate} - %s{endDate}.xlsx"
            state, Cmd.OfPromise.perform createAndDownloadReport (orderGroups, fileName) (fun () -> CreateExcelReportFinished)
        | _ -> state, Cmd.none
    | CreateExcelReportFinished -> state, Cmd.none

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
            | Loaded (_, { Orders = [] }) ->
                View.infoNotification "Keine Bestellungen vorhanden." []
            | Loaded (_, data) ->
                Html.div [
                    prop.className "container flex flex-col items-start gap-2"
                    prop.children [
                        Html.div [
                            prop.className "container"
                            prop.children [
                                Html.a [
                                    prop.className "btn btn-blue"
                                    let text =
                                        if data.SelectedUsers = Collections.Set.empty then "Alle Personen"
                                        else
                                            let name (lastName: string, firstName: string) =
                                                [
                                                    lastName
                                                    yield! firstName |> Seq.tryHead |> Option.map (sprintf "%c.") |> Option.toList
                                                ]
                                                |> String.concat " "
                                            let names = data.SelectedUsers |> Seq.truncate 3 |> Seq.map name |> String.concat ", "
                                            if data.SelectedUsers.Count <= 3 then names
                                            else
                                                let more = View.pluralize (data.SelectedUsers.Count - 3) "weitere Person" "weitere Personen"
                                                $"%s{names} und %s{more}"
                                    prop.text text
                                    prop.onClick (fun _e -> dispatch ShowUserSelection)
                                ]
                                if data.UserSelectionVisible then
                                    let users =
                                        data.Orders
                                        |> List.groupBy (fun v -> v.LastName, v.FirstName)
                                        |> List.map (fun (name, list) ->
                                            let revenue = list |> List.sumBy (fun v -> decimal v.Amount * v.PricePerUnit)
                                            name, revenue
                                        )
                                        |> List.sortBy fst
                                    View.modal "Personen auswählen" (fun () -> dispatch HideUserSelection) [
                                        UserCards.UserCards users (fst >> fst) (fun ((lastName, firstName), revenue) ->
                                            let isSelected = Collections.Set.contains (lastName, firstName) data.SelectedUsers
                                            Html.div [
                                                prop.classes [
                                                    "flex flex-col shadow rounded p-2 cursor-pointer border"
                                                    if isSelected then "border-musi-blue"
                                                ]
                                                prop.onClick (fun _ ->
                                                    let selectedUsers =
                                                        if isSelected then Collections.Set.remove (lastName, firstName) data.SelectedUsers
                                                        else Collections.Set.add (lastName, firstName) data.SelectedUsers
                                                    dispatch (SetUserSelection selectedUsers)
                                                )
                                                prop.children [
                                                    Html.span [
                                                        prop.className "text-center"
                                                        prop.text $"%s{lastName.ToUpper()} %s{firstName}"
                                                    ]
                                                    Html.span [
                                                        prop.className "text-center text-sm"
                                                        prop.text (View.formatPrice revenue)
                                                    ]
                                                ]
                                            ]
                                        )
                                    ] [
                                        Html.a [
                                            prop.className "btn btn-blue"
                                            prop.text "Alle auswählen"
                                            prop.onClick (fun _e -> SetUserSelection (users |> List.map fst |> Collections.Set.ofList) |> dispatch)
                                        ]
                                        Html.a [
                                            prop.className "btn btn-blue"
                                            prop.text "Keine auswählen"
                                            prop.onClick (fun _e -> SetUserSelection Collections.Set.empty |> dispatch)
                                        ]
                                    ]
                            ]
                        ]
                        Html.div [
                            Html.a [
                                prop.className "btn btn-green"
                                prop.text "Bericht herunterladen"
                                prop.onClick (fun _e -> dispatch CreateExcelReport)
                            ]
                        ]
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
                                    filterByUsersAndGroupByProduct data.Orders data.SelectedUsers
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
