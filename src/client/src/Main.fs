module Main

open Browser.Dom
open Fable.Core
open Fable.Core.JsInterop
open Fable.FontAwesome
open Feliz
open Feliz.Bulma
open Feliz.UseDeferred
open Feliz.UseElmish
open global.JS
open MusiOrder.Models
open Thoth.Fetch
open Thoth.Json

importAll "../styles/main.scss"

module React =
    let useAuthentication isActive =
        let (authKey, setAuthKey) = React.useState(None)
        React.useEffect (
            fun () ->
                if isActive then
                    let mutable key = ""
                    let mutable timeoutId = 0.
                    let rec finishAuthKey () =
                        setAuthKey (Some (AuthKey key))
                        key <- ""
                    and listener (e: Browser.Types.Event) =
                        window.clearTimeout timeoutId
                        let newKey = (e :?> Browser.Types.KeyboardEvent).key
                        if newKey = "Enter" then finishAuthKey ()
                        else
                            key <- key + newKey
                            timeoutId <- window.setTimeout (finishAuthKey, 500)
                    window.addEventListener("keydown", listener)
                    React.createDisposable (fun () -> window.removeEventListener("keydown", listener))
                else
                    setAuthKey None
                    React.createDisposable id
            ,
            [| box isActive |]
        )
        authKey

let nav =
    Bulma.navbar [
        color.hasBackgroundPrimary
        prop.className "navigation"
        prop.children [
            Bulma.navbarBrand.div [
                prop.style [
                    style.alignItems.baseline
                ]
                prop.children [
                    Bulma.navbarItem.div [
                        Bulma.title.h1 [
                            prop.text "MusiOrder"
                        ]
                    ]
                    Bulma.navbarItem.div [
                        Bulma.icon [
                            Fa.i [ Fa.Solid.Beer; Fa.Size Fa.Fa2x ] []
                        ]
                    ]
                ]
            ]
        ]
    ]

let showAdministration = React.functionComponent (fun () ->
    let (isVisible, setVisible) = React.useState(false)
    let authKey = React.useAuthentication isVisible
    let startAuthenticate () = setVisible true

    let authorize authKey : Async<Result<AuthKey * UserInfo list, unit>> = async {
        let url = sprintf "/api/users?authKey=%s" (AuthKey.toString authKey |> JS.encodeURIComponent)
        match! Fetch.``tryGet``(url, caseStrategy = CamelCase) |> Async.AwaitPromise with
        | Ok result -> return Ok (authKey, result)
        | Error (FetchFailed response) when response.Status = 403 -> return Error ()
        | Error e -> return failwith (Helper.message e)
    }

    let (authorizationState, setAuthorizationState) = React.useState(Deferred.HasNotStartedYet)

    let startAuthorization = React.useDeferredCallback(authorize, setAuthorizationState)

    React.useEffect(
        fun () ->
            match authKey, authorizationState with
            | Some authKey, Deferred.HasNotStartedYet -> startAuthorization authKey
            | Some authKey, Deferred.Failed -> startAuthorization authKey
            | Some authKey, Deferred.Resolved Error -> startAuthorization authKey
            | _ -> ()
        ,
        [| box authKey |]
    )

    let (selectedUser, setSelectedUser) = React.useState(None)
    let selectUser = Some >> setSelectedUser

    let addPayment (payment: Payment) = async {
        let coders =
            Extra.empty
            |> Extra.withCustom AuthKey.encode AuthKey.decoder
            |> Extra.withCustom PositiveInteger.encode PositiveInteger.decoder
        let! (totalAmount: float) = Fetch.post("/api/payment", payment, caseStrategy = CamelCase, extra = coders) |> Async.AwaitPromise
        return (payment.UserId, totalAmount)
    }

    let (paymentState, setPaymentState) = React.useState(Deferred.HasNotStartedYet)
    let startPayment = React.useDeferredCallback(addPayment, setPaymentState)

    React.useEffect (
        fun () ->
            paymentState
            |> Deferred.iter (fun (userId, totalAmount) ->
                authorizationState
                |> Deferred.map (fun result ->
                    result
                    |> Result.map (fun (authKey, users) ->
                        let users' =
                            users
                            |> List.map (fun user ->
                                if user.Id = userId then { user with Balance = totalAmount }
                                else user
                            )
                        (authKey, users')
                    )
                )
                |> setAuthorizationState
            )
        , [| box paymentState |]
    )

    let hideOrderSummary () =
        setVisible false
        setAuthorizationState Deferred.HasNotStartedYet
        setSelectedUser None
        setPaymentState Deferred.HasNotStartedYet

    let authForm =
        match authorizationState with
        | Deferred.HasNotStartedYet -> View.authForm "Administration" hideOrderSummary
        | Deferred.InProgress -> View.modal "Administration" hideOrderSummary [ View.loadIconBig ] []
        | Deferred.Failed ->
            View.modal "Administration" hideOrderSummary [
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
        | Deferred.Resolved Error ->
            View.modal "Administration" hideOrderSummary [
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
        | Deferred.Resolved (Ok (authKey, users)) ->
            View.modal "Administration" hideOrderSummary [
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
                        for user in users ->
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
                                prop.onClick (fun _ -> selectUser user.Id)
                                if selectedUser = Some user.Id then tr.isSelected
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
                match selectedUser with
                | Some selectedUserId ->
                    Html.span [
                        spacing.mr2
                        prop.text "Guthaben aufladen:"
                    ]
                    Html.div [
                        Bulma.buttons [
                            for i in List.choose PositiveInteger.tryCreate [1; 2; 5; 10; 20] ->
                                Bulma.button.button [
                                    match paymentState with
                                    | Deferred.InProgress ->
                                        prop.disabled true
                                    | _ -> ()
                                    prop.textf "+%d€" (PositiveInteger.value i)
                                    prop.onClick (fun _ -> startPayment { AuthKey = authKey; UserId = selectedUserId; Amount = i })
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
                    match paymentState with
                    | Deferred.HasNotStartedYet -> ()
                    | Deferred.InProgress -> icon [ color.hasTextPrimary ] [ Fa.Solid.Spinner; Fa.Pulse ]
                    | Deferred.Failed e -> icon [ color.hasTextDanger; prop.title e.Message ] [ Fa.Solid.Times ]
                    | Deferred.Resolved -> icon [ color.hasTextSuccess ] [ Fa.Solid.Check ]
                | None -> ()
            ]

    [
        Bulma.button.button [
            prop.onClick (ignore >> startAuthenticate)
            prop.children [
                Bulma.icon [ Fa.i [ Fa.Solid.Cogs ] [] ]
                Html.span [ prop.text "Administration" ]
            ]
        ]
        if isVisible then authForm
    ]
)

let main =
    Html.div [
        prop.className "main"
        prop.children [
            nav
            OrderForm.view
                {|
                    UserButtons = [ OrderSummary.view () ]
                    AdminButtons = [ showAdministration () ]
                |}
        ]
    ]

ReactDOM.render(main, document.getElementById "app")
