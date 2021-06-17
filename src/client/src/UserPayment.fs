module UserPayment

open Api
open Elmish
open Fable.Core
open Fable.Core.JsInterop
open Fable.FontAwesome
open Fable.React
open Feliz
open Feliz.Bulma
open Feliz.Bulma.Operators
open Feliz.UseDeferred
open Feliz.UseElmish
open global.JS
open MusiOrder.Models
open Thoth.Json
open Thoth.Fetch

type UserList = {
    Users: UserInfo list
    SelectedUser: string option
    AddPaymentState: Deferred<unit>
}

type Model =
    | NotLoaded
    | Loading of AuthKey
    | LoadError of AuthKey * LoadUsersError
    | Loaded of AuthKey * UserList

module UserList =
    let init users = {
        Users = users
        SelectedUser = None
        AddPaymentState = Deferred.HasNotStartedYet
    }

type Msg =
    | Load of AuthKey
    | LoadResult of Result<UserInfo list, LoadUsersError>
    | SelectUser of userId: string
    | AddPayment of userId: string * amount: decimal
    | AddPaymentResult of Result<(string * decimal), exn>

let init authKey =
    match authKey with
    | Some authKey ->
        NotLoaded, Cmd.ofMsg (Load authKey)
    | None ->
        NotLoaded, Cmd.none

let addPayment (payment: Payment) = async {
    let coders =
        Extra.empty
        |> Extra.withCustom AuthKey.encode AuthKey.decoder
        |> Extra.withDecimal
    let! (totalAmount: decimal) = Fetch.post("/api/payment", payment, caseStrategy = CamelCase, extra = coders) |> Async.AwaitPromise
    return (payment.UserId, totalAmount)
}

let update msg state =
    match msg with
    | Load authKey -> Loading authKey, Cmd.OfAsync.perform loadUsers authKey LoadResult
    | LoadResult (Ok users) ->
        match state with
        | Loading authKey -> Loaded (authKey, UserList.init users), Cmd.none
        | _ -> state, Cmd.none
    | LoadResult (Error e) ->
        match state with
        | Loading authKey -> LoadError (authKey, e), Cmd.none
        | _ -> state, Cmd.none
    | SelectUser userId ->
        match state with
        | Loaded (authKey, userList) -> Loaded (authKey, { userList with SelectedUser = Some userId }), Cmd.none
        | _ -> state, Cmd.none
    | AddPayment (userId, amount) ->
        match state with
        | Loaded (authKey, userList) ->
            let payment = { AuthKey = authKey; UserId = userId; Amount = amount }
            Loaded (authKey, { userList with AddPaymentState = Deferred.InProgress }), Cmd.OfAsync.either addPayment payment (Ok >> AddPaymentResult) (Error >> AddPaymentResult)
        | _ -> state, Cmd.none
    | AddPaymentResult (Ok (userId, totalAmount)) ->
        match state with
        | Loaded (authKey, userList) ->
            let users =
                userList.Users
                |> List.map (fun user ->
                    if user.Id = userId then { user with Balance = totalAmount }
                    else user
                )
            Loaded (authKey, { userList with Users = users; AddPaymentState = Deferred.Resolved () }), Cmd.none
        | _ -> state, Cmd.none
    | AddPaymentResult (Error e) ->
        match state with
        | Loaded (authKey, userList) -> Loaded (authKey, { userList with AddPaymentState = Deferred.Failed e }), Cmd.none
        | _ -> state, Cmd.none

[<ReactComponent>]
let UserPayment authKey setAuthKeyInvalid (setMenuItems: ReactElement list -> ReactElement) =
    let (state, dispatch) = React.useElmish(init authKey, update, [| authKey :> obj |])

    match state with
    | NotLoaded -> Html.none // Handled by parent component
    | Loading _ ->
        Html.div [
            text.hasTextCentered
            prop.children [ View.loadIconBig ]
        ]
    | LoadError (_, Forbidden) ->
        setAuthKeyInvalid ()
        Html.none // Handled by parent component
    | LoadError (authKey, Other _) ->
        View.errorNotificationWithRetry "Fehler beim Laden der Daten." (fun () -> dispatch (Load authKey))
    | Loaded (_, userList) ->
        Html.div [
            match userList.SelectedUser with
            | Some selectedUserId ->
                setMenuItems [
                    Bulma.levelItem [
                        prop.text "Guthaben aufladen:"
                    ]
                    Bulma.levelItem [
                        Bulma.buttons [
                            let amounts = [
                                yield!
                                    userList.SelectedUser
                                    |> Option.bind (fun userId ->
                                        userList.Users
                                        |> List.tryFind (fun v -> v.Id = userId)
                                    )
                                    |> Option.filter (fun v -> v.Balance <> 0.m)
                                    |> Option.map (fun v -> -v.Balance)
                                    |> Option.toList

                                yield! [ 0.1m; 0.2m; 0.5m; 1.m; 2.m; 5.m; 10.m; 20.m ]
                            ]
                            for amount in amounts do
                                Bulma.button.button [
                                    match userList.AddPaymentState with
                                    | Deferred.InProgress ->
                                        prop.disabled true
                                    | _ -> ()
                                    prop.textf "%s%g€" (if amount >= 0.m then "+" else "") amount
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
                    match userList.AddPaymentState with
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
                            for user in userList.Users ->
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
                                            if userList.SelectedUser = Some user.Id then tr.isSelected ++ text.hasTextLeft
                                            else text.hasTextLeft
                                            prop.style [ style.textTransform.uppercase ]
                                            prop.text user.LastName
                                        ]
                                        Html.td [
                                            if userList.SelectedUser = Some user.Id then tr.isSelected ++ text.hasTextLeft
                                            else text.hasTextLeft
                                            prop.text user.FirstName
                                        ]
                                        Html.td [
                                            match latestOrderColor with
                                            | Some color -> color
                                            | None -> ()
                                            prop.text latestOrderTime
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
            ]
        ]
