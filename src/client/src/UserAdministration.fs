module UserAdministration

open Api
open Elmish
open Fable.FontAwesome
open Feliz
open Feliz.Bulma
open Feliz.UseElmish
open global.JS
open MusiOrder.Models

type LoadedModel = {
    Users: UserData list
    VisibleKeyCodeUserIds: Set<string>
}

type Model =
    | NotLoaded
    | Loading of AuthKey
    | LoadError of AuthKey * FetchError
    | Loaded of AuthKey * LoadedModel

type Msg =
    | Load of AuthKey
    | LoadResult of Result<UserData list, FetchError>
    | ShowAuthKey of userId: string

let init authKey =
    match authKey with
    | Some authKey ->
        NotLoaded, Cmd.ofMsg (Load authKey)
    | None ->
        NotLoaded, Cmd.none

let update msg state =
    match msg with
    | Load authKey ->
        Loading authKey, Cmd.OfAsync.perform loadUserData authKey LoadResult
    | LoadResult (Ok users) ->
        match state with
        | Loading authKey ->
            Loaded (authKey, { Users = users; VisibleKeyCodeUserIds = Set.empty }),
            Cmd.none
        | _ -> state, Cmd.none
    | LoadResult (Error e) ->
        match state with
        | Loading authKey -> LoadError (authKey, e), Cmd.none
        | _ -> state, Cmd.none
    | ShowAuthKey userId ->
        match state with
        | Loaded (authKey, state) -> Loaded (authKey, { state with VisibleKeyCodeUserIds = Set.add userId state.VisibleKeyCodeUserIds }), Cmd.none
        | _ -> state, Cmd.none

[<ReactComponent>]
let UserAdministration authKey setAuthKeyInvalid (setMenuItems: ReactElement list -> ReactElement) =
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
    | Loaded (_, { Users = [] }) ->
        View.infoNotification "Keine Benutzer vorhanden"
    | Loaded (_, state) ->
        Bulma.container [
            Bulma.table [
                prop.children [
                    Html.thead [
                        Html.tr [
                            Html.th [ prop.text "Nachname" ]
                            Html.th [ prop.text "Vorname" ]
                            Html.th [ prop.text "Schlüsselnummer" ]
                            Html.th [ prop.text "Rolle" ]
                            Html.th [ prop.style [ style.width (length.px 150) ] ]
                        ]
                    ]
                    Html.tbody [
                        for user in state.Users ->
                            Html.tr [
                                prop.children [
                                    Html.td [
                                        text.isUppercase
                                        prop.text user.LastName
                                    ]
                                    Html.td [
                                        prop.text user.FirstName
                                    ]
                                    Html.td [
                                        match user.AuthKey with
                                        | Some authKey ->
                                            if Set.contains user.Id state.VisibleKeyCodeUserIds then
                                                prop.text (AuthKey.toString authKey)
                                            else
                                                prop.children [
                                                    Bulma.level [
                                                        Bulma.levelLeft [
                                                            Bulma.levelItem [
                                                                prop.text "**********"
                                                            ]
                                                            Bulma.levelItem [
                                                                Bulma.button.a [
                                                                    prop.onClick (fun _ -> dispatch (ShowAuthKey user.Id))
                                                                    
                                                                    prop.children [
                                                                        Bulma.icon [ Fa.i [ Fa.Solid.Eye ] [] ]
                                                                    ]
                                                                ]
                                                            ]
                                                        ]
                                                    ]
                                                ]
                                        | None -> prop.text "-"
                                    ]
                                    Html.td [
                                        prop.text (UserRole.toString user.Role)
                                    ]
                                    Html.td [
                                        // Bulma.level [
                                        //     Bulma.levelLeft [
                                        //         Bulma.levelItem [
                                        //             Bulma.button.a [
                                        //                 color.isDanger
                                        //                 prop.onClick (fun _ -> dispatch (DeleteOrder order.Id))
                                                        
                                        //                 match deleteOrderState with
                                        //                 | Some Deferred.InProgress
                                        //                 | Some (Deferred.Resolved ()) -> prop.disabled true
                                        //                 | _ -> ()

                                        //                 prop.children [
                                        //                     Bulma.icon [ Fa.i [ Fa.Solid.TrashAlt ] [] ]
                                        //                 ]
                                        //                 if deleteOrderState = Some Deferred.InProgress then
                                        //                     button.isLoading
                                        //             ]
                                        //         ]
                                        //         let icon iconProps faProps =
                                        //             Bulma.levelItem [
                                        //                 Bulma.icon [
                                        //                     control.isMedium
                                        //                     yield! iconProps
                                        //                     prop.children [
                                        //                         Fa.i [
                                        //                             Fa.Size Fa.FaLarge
                                        //                             yield! faProps
                                        //                         ] []
                                        //                     ]
                                        //                 ]
                                        //             ]
                                        //         match deleteOrderState with
                                        //         | Some Deferred.HasNotStartedYet -> ()
                                        //         | Some Deferred.InProgress -> icon [ color.hasTextPrimary ] [ Fa.Solid.Spinner; Fa.Pulse ]
                                        //         | Some (Deferred.Failed e) -> icon [ color.hasTextDanger; prop.title e.Message ] [ Fa.Solid.Times ]
                                        //         | Some (Deferred.Resolved _) -> icon [ color.hasTextSuccess ] [ Fa.Solid.Check ]
                                        //         | None -> ()
                                        //     ]
                                        // ]
                                    ]
                                ]
                            ]
                    ]
                ]
            ]
        ]
