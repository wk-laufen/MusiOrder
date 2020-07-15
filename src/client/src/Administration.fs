module Administration

open Api
open Elmish
open Fable.Core
open Fable.Core.JsInterop
open Fable.FontAwesome
open Feliz
open Feliz.Bulma
open Feliz.UseDeferred
open Feliz.UseElmish
open MusiOrder.Models
open Thoth.Fetch
open Thoth.Json

type UserList = {
    AuthKey: AuthKey
    Users: UserInfo list
    SelectedUser: string option
    AddPaymentState: Deferred<unit>
}
module UserList =
    let init authKey users = {
        AuthKey = authKey
        Users = users
        SelectedUser = None
        AddPaymentState = Deferred.HasNotStartedYet
    }

type Model =
    | Hidden
    | Authenticating
    | Loading of AuthKey
    | LoadError of LoadUsersError
    | Loaded of UserList

type Msg =
    | Show
    | Load of AuthKey
    | LoadResult of Result<UserInfo list, LoadUsersError>
    | SelectUser of userId: string
    | AddPayment of userId: string * amount: PositiveInteger
    | AddPaymentResult of Result<(string * float), exn>
    | Close

let init = Hidden, Cmd.none

let addPayment (payment: Payment) = async {
    let coders =
        Extra.empty
        |> Extra.withCustom AuthKey.encode AuthKey.decoder
        |> Extra.withCustom PositiveInteger.encode PositiveInteger.decoder
    let! (totalAmount: float) = Fetch.post("/api/payment", payment, caseStrategy = CamelCase, extra = coders) |> Async.AwaitPromise
    return (payment.UserId, totalAmount)
}

let update msg state =
    match msg with
    | Show -> Authenticating, Cmd.none
    | Load authKey -> Loading authKey, Cmd.OfAsync.perform loadUsers authKey LoadResult
    | LoadResult (Ok users) ->
        match state with
        | Loading authKey -> Loaded (UserList.init authKey users), Cmd.none
        | _ -> state, Cmd.none
    | LoadResult (Error e) -> LoadError e, Cmd.none
    | SelectUser userId ->
        match state with
        | Loaded userList -> Loaded { userList with SelectedUser = Some userId }, Cmd.none
        | _ -> state, Cmd.none
    | AddPayment (userId, amount) ->
        match state with
        | Loaded userList ->
            let payment = { AuthKey = userList.AuthKey; UserId = userId; Amount = amount }
            Loaded { userList with AddPaymentState = Deferred.InProgress }, Cmd.OfAsync.either addPayment payment (Ok >> AddPaymentResult) (Error >> AddPaymentResult)
        | _ -> state, Cmd.none
    | AddPaymentResult (Ok (userId, totalAmount)) ->
        match state with
        | Loaded userList ->
            let users =
                userList.Users
                |> List.map (fun user ->
                    if user.Id = userId then { user with Balance = totalAmount }
                    else user
                )
            Loaded { userList with Users = users; AddPaymentState = Deferred.Resolved () }, Cmd.none
        | _ -> state, Cmd.none
    | AddPaymentResult (Error e) ->
        match state with
        | Loaded userList -> Loaded { userList with AddPaymentState = Deferred.Failed e }, Cmd.none
        | _ -> state, Cmd.none
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
        | Authenticating -> View.authForm "Administration" (fun () -> dispatch Close)
        | Loading -> View.modal "Administration" (fun () -> dispatch Close) [ View.loadIconBig ] []
        | LoadError Forbidden ->
            View.modal "Administration" (fun () -> dispatch Close) [
                Bulma.container [
                    color.hasTextDanger
                    spacing.px2
                    prop.children [
                        Fa.i [ Fa.Solid.Ban; Fa.Size Fa.Fa8x ] []
                        Bulma.title.p [
                            color.hasTextDanger
                            prop.children [
                                Html.text "Schlüssel ist nicht authorisiert."
                                Html.br []
                                Html.text "Versuche es nochmal mit einem Administrator-Schlüssel."
                            ]
                        ]
                    ]
                ]
            ] []
        | LoadError Other ->
            View.modal "Administration" (fun () -> dispatch Close) [
                Bulma.container [
                    color.hasTextDanger
                    spacing.px2
                    prop.children [
                        Fa.i [ Fa.Solid.Key; Fa.Size Fa.Fa8x ] []
                        Bulma.title.p [
                            color.hasTextDanger
                            prop.children [
                                Html.text "Fehler bei der Authorisierung."
                                Html.br []
                                Html.text "Versuche es nochmal mit deinem Musischlüssel."
                            ]
                        ]
                    ]
                ]
            ] []
        | Loaded userList ->
            View.modal "Administration" (fun () -> dispatch Close) [
                Bulma.table [
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
                                    let m = JS.moment(v)
                                    let daysSinceLatestOrder = JS.moment(System.DateTimeOffset.Now)?diff(m, "days")
                                    let color =
                                        if daysSinceLatestOrder < 10. then Some color.isSuccess
                                        elif daysSinceLatestOrder < 30. then Some color.isWarning
                                        else Some color.isDanger
                                    color, JS.moment(v)?fromNow()
                                )
                                |> Option.defaultValue (None, "-")
                            Html.tr [
                                prop.onClick (fun _ -> dispatch (SelectUser user.Id))
                                if userList.SelectedUser = Some user.Id then tr.isSelected
                                prop.children [
                                    Html.td [
                                        text.hasTextLeft
                                        prop.style [ style.textTransform.uppercase ]
                                        prop.text user.LastName
                                    ]
                                    Html.td [
                                        text.hasTextLeft
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
            ] [
                match userList.SelectedUser with
                | Some selectedUserId ->
                    Html.span [
                        spacing.mr2
                        prop.text "Guthaben aufladen:"
                    ]
                    Html.div [
                        Bulma.buttons [
                            for i in List.choose PositiveInteger.tryCreate [1; 2; 5; 10; 20] ->
                                Bulma.button.button [
                                    match userList.AddPaymentState with
                                    | Deferred.InProgress ->
                                        prop.disabled true
                                    | _ -> ()
                                    prop.textf "+%d€" (PositiveInteger.value i)
                                    prop.onClick (fun _ -> dispatch (AddPayment (selectedUserId, i)))
                                ]
                        ]
                    ]
                    let icon iconProps faProps =
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
                    match userList.AddPaymentState with
                    | Deferred.HasNotStartedYet -> ()
                    | Deferred.InProgress -> icon [ color.hasTextPrimary ] [ Fa.Solid.Spinner; Fa.Pulse ]
                    | Deferred.Failed e -> icon [ color.hasTextDanger; prop.title e.Message ] [ Fa.Solid.Times ]
                    | Deferred.Resolved -> icon [ color.hasTextSuccess ] [ Fa.Solid.Check ]
                | None -> ()
            ]

    [
        Bulma.button.button [
            prop.onClick (fun _ -> dispatch Show)
            prop.children [
                Bulma.icon [ Fa.i [ Fa.Solid.Cogs ] [] ]
                Html.span [ prop.text "Administration" ]
            ]
        ]
        authForm
    ]
)
