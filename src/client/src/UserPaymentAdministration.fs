module UserPaymentAdministration

open Api
open Api.UserPaymentAdministration
open Elmish
open Fable.Core.JsInterop
open Feliz
open Feliz.UseDeferred
open Feliz.UseElmish
open global.JS
open MusiOrder.Models
open MusiOrder.Models.UserPaymentAdministration

type LoadedModel = {
    Users: UserInfo list
    SelectedUser: UserId option
    AddPaymentState: Deferred<unit>
}

type Model =
    | NotLoaded
    | Loading of AuthKey option
    | LoadError of AuthKey option * ApiError<LoadUsersError>
    | Loaded of AuthKey option * LoadedModel

type Msg =
    | Load of AuthKey option
    | LoadResult of Result<UserInfo list, ApiError<LoadUsersError>>
    | SelectUser of UserId
    | AddPayment of UserId * amount: decimal
    | AddPaymentResult of Result<(UserId * decimal), ApiError<AddPaymentError>>

let init authKey =
    NotLoaded, Cmd.ofMsg (Load authKey)

let update msg state =
    match msg with
    | Load authKey -> Loading authKey, Cmd.OfAsync.perform loadUsers authKey LoadResult
    | LoadResult (Ok users) ->
        match state with
        | Loading authKey -> Loaded (authKey, { Users = users; SelectedUser = None; AddPaymentState = Deferred.HasNotStartedYet }), Cmd.none
        | _ -> state, Cmd.none
    | LoadResult (Error e) ->
        match state with
        | Loading authKey -> LoadError (authKey, e), Cmd.none
        | _ -> state, Cmd.none
    | SelectUser userId ->
        match state with
        | Loaded (authKey, state) -> Loaded (authKey, { state with SelectedUser = Some userId }), Cmd.none
        | _ -> state, Cmd.none
    | AddPayment (userId, amount) ->
        match state with
        | Loaded (authKey, state) ->
            let payment = { Amount = amount }
            Loaded (authKey, { state with AddPaymentState = Deferred.InProgress }), Cmd.OfAsync.perform (addPayment authKey userId) payment (Result.map (fun v -> (userId, v)) >> AddPaymentResult)
        | _ -> state, Cmd.none
    | AddPaymentResult (Ok (userId, totalAmount)) ->
        match state with
        | Loaded (authKey, state) ->
            let users =
                state.Users
                |> List.map (fun user ->
                    if user.Id = userId then { user with Balance = totalAmount }
                    else user
                )
            Loaded (authKey, { state with Users = users; AddPaymentState = Deferred.Resolved () }), Cmd.none
        | _ -> state, Cmd.none
    | AddPaymentResult (Error e) ->
        match state with
        | Loaded (authKey, userList) -> Loaded (authKey, { userList with AddPaymentState = Deferred.Failed (exn "Fehler beim Speichern der Zahlung") }), Cmd.none
        | _ -> state, Cmd.none

[<ReactComponent>]
let UserPaymentAdministration authKey setAuthKeyInvalid (setMenuItems: ReactElement list -> ReactElement) =
    let (state, dispatch) = React.useElmish(init authKey, update, [| authKey :> obj |])

    React.useEffect(fun () ->
        match state with
        | LoadError (_, ExpectedError LoadUsersError.InvalidAuthKey)
        | LoadError (_, ExpectedError LoadUsersError.NotAuthorized) ->
            setAuthKeyInvalid ()
        | _ -> ()
    )

    match state with
    | NotLoaded -> Html.none // Handled by parent component
    | Loading _ -> View.loadIconBig
    | LoadError (_, ExpectedError LoadUsersError.InvalidAuthKey)
    | LoadError (_, ExpectedError LoadUsersError.NotAuthorized) -> Html.none // Handled by parent component
    | LoadError (authKey, UnexpectedError _) ->
        View.errorNotificationWithRetry "Fehler beim Laden der Daten." (fun () -> dispatch (Load authKey))
    | Loaded (_, { Users = [] }) ->
        View.infoNotification "Keine Benutzer vorhanden." []
    | Loaded (_, state) ->
        React.fragment [
            match state.SelectedUser with
            | Some selectedUserId ->
                setMenuItems [
                    Html.span [
                        prop.text "Guthaben ändern:"
                    ]
                    let amounts = [
                        yield!
                            state.Users
                            |> List.tryFind (fun v -> v.Id = selectedUserId)
                            |> Option.map (fun v -> v.SuggestedBalanceChanges)
                            |> Option.defaultValue []
                    ]
                    for amount in amounts do
                        Html.button [
                            prop.className "btn"
                            match state.AddPaymentState with
                            | Deferred.InProgress ->
                                prop.disabled true
                            | _ -> ()
                            prop.text (View.formatBalance amount)
                            prop.onClick (fun _ -> dispatch (AddPayment (selectedUserId, amount)))
                        ]
                    match state.AddPaymentState with
                    | Deferred.HasNotStartedYet -> ()
                    | Deferred.InProgress -> Html.i [ prop.className "fa-lg fas fa-spinner fa-pulse text-musi-gold" ]
                    | Deferred.Failed e -> Html.i [ prop.className "fa-lg fas fa-times text-musi-red"; prop.title e.Message ]
                    | Deferred.Resolved _ -> Html.i [ prop.className "fa-lg fas fa-check text-musi-green" ]
                ]
            | None -> ()

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
                                    Html.th [ prop.text "Letzte Bestellung" ]
                                    Html.th [ prop.text "Aktuelles Guthaben" ]
                                ]
                            ]
                            Html.tbody [
                                for user in state.Users ->
                                    let (latestOrderColor, latestOrderTime) =
                                        user.LatestOrderTimestamp
                                        |> Option.map (fun v ->
                                            let m = moment(v)
                                            let daysSinceLatestOrder = moment(System.DateTimeOffset.Now)?diff(m, "days")
                                            let color =
                                                if daysSinceLatestOrder < 10. then Some "text-musi-green"
                                                elif daysSinceLatestOrder < 30. then Some "text-musi-blue"
                                                else Some "text-musi-red"
                                            color, moment(v)?fromNow()
                                        )
                                        |> Option.defaultValue (None, "-")
                                    Html.tr [
                                        prop.classes [
                                            if state.SelectedUser = Some user.Id then "selected"
                                        ]
                                        prop.onClick (fun _ -> dispatch (SelectUser user.Id))
                                        prop.children [
                                            Html.td [
                                                prop.className "uppercase"
                                                prop.text user.LastName
                                            ]
                                            Html.td [
                                                prop.text user.FirstName
                                            ]
                                            Html.td [
                                                prop.classes [
                                                    yield! Option.toList latestOrderColor
                                                ]
                                                prop.text latestOrderTime
                                            ]
                                            Html.td [
                                                prop.className $"%s{View.balanceColor user.Balance}"
                                                prop.text (View.formatBalance user.Balance)
                                            ]
                                        ]
                                    ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
