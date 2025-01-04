module UserPaymentAdministration

open Api
open Api.UserPaymentAdministration
open Elmish
open Fable.Core.JsInterop
open Fable.FontAwesome
open Feliz
open Feliz.Bulma
open Feliz.Bulma.Operators
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
                    Bulma.levelItem [
                        prop.text "Guthaben ändern:"
                    ]
                    Bulma.levelItem [
                        Bulma.buttons [
                            let amounts = [
                                yield!
                                    state.SelectedUser
                                    |> Option.bind (fun userId ->
                                        state.Users
                                        |> List.tryFind (fun v -> v.Id = userId)
                                    )
                                    |> Option.filter (fun v -> v.Balance <> 0.m)
                                    |> Option.map (fun v -> -v.Balance)
                                    |> Option.toList

                                yield! [ 0.1m; 0.2m; 0.5m; 1.m; 2.m; 5.m; 10.m; 20.m ]
                            ]
                            for amount in amounts do
                                Bulma.button.button [
                                    match state.AddPaymentState with
                                    | Deferred.InProgress ->
                                        prop.disabled true
                                    | _ -> ()
                                    prop.textf "%s%.2f€" (if amount >= 0.m then "+" else "") amount
                                    prop.onClick (fun _ -> dispatch (AddPayment (selectedUserId, amount)))
                                ]
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
                    match state.AddPaymentState with
                    | Deferred.HasNotStartedYet -> ()
                    | Deferred.InProgress -> icon [ color.hasTextPrimary ] [ Fa.Solid.Spinner; Fa.Pulse ]
                    | Deferred.Failed e -> icon [ color.hasTextDanger; prop.title e.Message ] [ Fa.Solid.Times ]
                    | Deferred.Resolved _ -> icon [ color.hasTextSuccess ] [ Fa.Solid.Check ]
                ]
            | None -> ()

            Bulma.container [
                Bulma.table [
                    table.isFullWidth
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
                                            if daysSinceLatestOrder < 10. then Some color.isSuccess
                                            elif daysSinceLatestOrder < 30. then Some color.isWarning
                                            else Some color.isDanger
                                        color, moment(v)?fromNow()
                                    )
                                    |> Option.defaultValue (None, "-")
                                Html.tr [
                                    prop.onClick (fun _ -> dispatch (SelectUser user.Id))
                                    prop.children [
                                        Html.td [
                                            if state.SelectedUser = Some user.Id then tr.isSelected
                                            prop.style [ style.textTransform.uppercase ]
                                            prop.text user.LastName
                                        ]
                                        Html.td [
                                            if state.SelectedUser = Some user.Id then tr.isSelected
                                            prop.text user.FirstName
                                        ]
                                        Html.td [
                                            match latestOrderColor with
                                            | Some color -> color
                                            | None -> ()
                                            prop.text latestOrderTime
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
            ]
        ]
