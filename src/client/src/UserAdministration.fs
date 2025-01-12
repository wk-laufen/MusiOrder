module UserAdministration

open Api
open Api.UserAdministration
open Elmish
open Fable.FontAwesome
open Fable.Form.Simple
open Feliz
open Feliz.UseElmish
open global.JS
open MusiOrder.Models
open MusiOrder.Models.UserAdministration

type EditingUserAuthKeyState =
    | WaitingForUserAuthKey
    | GettingUserAuthKeyError of React.AuthenticationError
    | SavingUserAuthKey of AuthKey option
    | SaveUserAuthKeyResult of Result<AuthKey option, ApiError<SaveUserError>>

type UserFormData = {
    FirstName: string
    LastName: string
    AuthKey: string
    Role: string
}
module UserFormData =
    let fromUserData (v: ExistingUserData) =
        {
            FirstName = v.FirstName.Value
            LastName = v.LastName.Value
            AuthKey = v.AuthKey |> Option.map AuthKey.toString |> Option.defaultValue ""
            Role = UserRole.toString v.Role
        }
    let empty =
        {
            FirstName = ""
            LastName = ""
            AuthKey = ""
            Role = UserRole.toString User
        }

type EditingUser = {
    Id: UserId option
    Data: Form.View.Model<UserFormData>
}

type DeleteUserState =
    | LoadingWarnings
    | LoadedWarnings of Result<DeleteUserWarning list, ApiError<DeleteUserError>>
    | Deleting
    | Deleted of Result<unit, ApiError<ForceDeleteUserError>>

type LoadedModel = {
    Users: ExistingUser list
    VisibleKeyCodeUserIds: Set<UserId>
    EditingUserAuthKey: (ExistingUser * EditingUserAuthKeyState) option
    EditingUser: EditingUser option
    DeleteUserStates: Map<UserId, DeleteUserState>
}

type Model =
    | NotLoaded
    | Loading of AuthKey option
    | LoadError of AuthKey option * ApiError<LoadExistingUsersError>
    | Loaded of AuthKey option * LoadedModel

type Msg =
    | Load of AuthKey option
    | LoadResult of Result<ExistingUser list, ApiError<LoadExistingUsersError>>
    | ShowAuthKey of UserId
    | EditAuthKey of ExistingUser
    | SetAuthKey of Result<AuthKey option, React.AuthenticationError>
    | SetAuthKeyResult of Result<AuthKey option, ApiError<SaveUserError>>
    | CancelEditAuthKey
    | EditUser of ExistingUser
    | DeleteUser of UserId
    | DeleteUserResult of UserId * Result<DeleteUserWarning list, ApiError<DeleteUserError>>
    | ForceDeleteUser of UserId
    | ForceDeleteUserResult of UserId * Result<unit, ApiError<ForceDeleteUserError>>
    | EditNewUser
    | FormChanged of Form.View.Model<UserFormData>
    | SaveUser of ExistingUserData
    | SaveUserResult of Result<UserId, ApiError<SaveUserError>>
    | CancelEditUser

let init authKey =
    NotLoaded, Cmd.ofMsg (Load authKey)

let update msg state =
    match msg with
    | Load authKey ->
        Loading authKey, Cmd.OfAsync.perform loadUserData authKey LoadResult
    | LoadResult (Ok users) ->
        match state with
        | Loading authKey ->
            Loaded (authKey, { Users = users; VisibleKeyCodeUserIds = Set.empty; EditingUserAuthKey = None; EditingUser = None; DeleteUserStates = Map.empty }),
            Cmd.none
        | _ -> state, Cmd.none
    | LoadResult (Error e) ->
        match state with
        | Loading authKey -> LoadError (authKey, e), Cmd.none
        | _ -> state, Cmd.none
    | ShowAuthKey userId ->
        match state with
        | Loaded (authKey, state) ->
            Loaded (authKey, { state with VisibleKeyCodeUserIds = Set.add userId state.VisibleKeyCodeUserIds }),
            Cmd.none
        | _ -> state, Cmd.none
    | EditAuthKey user ->
        match state with
        | Loaded (authKey, state) ->
            Loaded (authKey, { state with EditingUserAuthKey = Some (user, WaitingForUserAuthKey) }),
            Cmd.none
        | _ -> state, Cmd.none
    | SetAuthKey (Ok userAuthKey) ->
        match state with
        | Loaded (authKey, ({ EditingUserAuthKey = Some (user, _) } as state)) ->
            Loaded (authKey, { state with EditingUserAuthKey = Some (user, SavingUserAuthKey userAuthKey) }),
            Cmd.OfAsync.perform (updateUser authKey user.Id) { PatchUserData.empty with AuthKey = userAuthKey; SetAuthKey = true } (Result.map (fun () -> userAuthKey) >> SetAuthKeyResult)
        | _ -> state, Cmd.none
    | SetAuthKey (Error error) ->
        match state with
        | Loaded (authKey, ({ EditingUserAuthKey = Some (user, _) } as state)) ->
            Loaded (authKey, { state with EditingUserAuthKey = Some (user, GettingUserAuthKeyError error) }),
            Cmd.none
        | _ -> state, Cmd.none
    | SetAuthKeyResult result ->
        match state with
        | Loaded (authKey, ({ EditingUserAuthKey = Some (user, _) } as state)) ->
            Loaded (authKey, { state with EditingUserAuthKey = Some (user, SaveUserAuthKeyResult result) }),
            Cmd.none
        | _ -> state, Cmd.none
    | CancelEditAuthKey ->
        match state with
        | Loaded (authKey, state) ->
            Loaded (authKey, { state with EditingUserAuthKey = None }),
            Cmd.ofMsg (Load authKey)
        | _ -> state, Cmd.none
    | EditUser user ->
        match state with
        | Loaded (authKey, state) ->
            Loaded (authKey, { state with EditingUser = Some { Id = Some user.Id; Data = Form.View.idle (UserFormData.fromUserData user.Data) } }),
            Cmd.none
        | _ -> state, Cmd.none
    | DeleteUser userId ->
        match state with
        | Loaded (authKey, state) ->
            Loaded (authKey, { state with DeleteUserStates = Map.add userId LoadingWarnings state.DeleteUserStates }),
            Cmd.OfAsync.perform (deleteUser authKey) userId (fun result -> DeleteUserResult (userId, result))
        | _ -> state, Cmd.none
    | DeleteUserResult (userId, result) ->
        match state with
        | Loaded (authKey, state) ->
            Loaded (authKey, { state with DeleteUserStates = Map.add userId (LoadedWarnings result) state.DeleteUserStates }),
            Cmd.none
        | _ -> state, Cmd.none
    | ForceDeleteUser userId ->
        match state with
        | Loaded (authKey, state) ->
            Loaded (authKey, { state with DeleteUserStates = Map.add userId Deleting state.DeleteUserStates }),
            Cmd.OfAsync.perform (forceDeleteUser authKey) userId (fun result -> ForceDeleteUserResult (userId, result))
        | _ -> state, Cmd.none
    | ForceDeleteUserResult (userId, result) ->
        match state with
        | Loaded (authKey, state) ->
            Loaded (authKey, { state with DeleteUserStates = Map.add userId (Deleted result) state.DeleteUserStates }),
            Cmd.none
        | _ -> state, Cmd.none
    | EditNewUser ->
        match state with
        | Loaded (authKey, state) ->
            Loaded (authKey, { state with EditingUser = Some { Id = None; Data = Form.View.idle UserFormData.empty } }),
            Cmd.none
        | _ -> state, Cmd.none
    | FormChanged formData ->
        match state with
        | Loaded (authKey, ({ EditingUser = Some editingUser } as state)) ->
            Loaded (authKey, { state with EditingUser = Some { editingUser with Data = formData } }),
            Cmd.none
        | _ -> state, Cmd.none
    | SaveUser user ->
        match state with
        | Loaded (authKey, ({ EditingUser = Some editingUser } as state)) ->
            let state = Loaded (authKey, { state with EditingUser = Some { editingUser with Data = { editingUser.Data with State = Form.View.State.Loading } } })
            let cmd =
                match editingUser.Id with
                | Some userId ->
                    let patchData = { PatchUserData.empty with FirstName = Some user.FirstName; LastName = Some user.LastName; Role = Some user.Role }
                    Cmd.OfAsync.perform (updateUser authKey userId) patchData (Result.map (fun () -> userId) >> SaveUserResult)
                | None -> Cmd.OfAsync.perform (createUser authKey) user SaveUserResult
            state, cmd
        | _ -> state, Cmd.none
    | SaveUserResult (Ok userId) ->
        match state with
        | Loaded (authKey, ({ EditingUser = Some editingUser } as state)) ->
            Loaded (authKey, { state with EditingUser = Some { editingUser with Id = Some userId; Data = { editingUser.Data with State = Form.View.State.Success "Benutzer erfolgreich gespeichert." } } }),
            Cmd.none
        | _ -> state, Cmd.none
    | SaveUserResult (Error e) ->
        match state with
        | Loaded (authKey, ({ EditingUser = Some editingUser } as state)) ->
            let errorMessage =
                match e with
                | ExpectedError DowngradeSelfNotAllowed -> "Fehler beim Speichern des Benutzers: Die eigene Rolle darf nicht gewechselt werden."
                | ExpectedError RemoveKeyCodeNotAllowed -> "Fehler beim Speichern des Benutzers: Die eigene Schlüsselnummer darf nicht entfernt werden."
                | ExpectedError (KeyCodeTaken None) -> "Fehler beim Speichern des Benutzers: Schlüsselnummer ist bereits vergeben."
                | ExpectedError (KeyCodeTaken (Some userName)) -> sprintf "Fehler beim Speichern des Benutzers: Schlüsselnummer ist bereits an %s vergeben." userName
                | ExpectedError SaveUserError.InvalidAuthKey
                | ExpectedError SaveUserError.NotAuthorized
                | UnexpectedError _ -> "Fehler beim Speichern des Benutzers."
            Loaded (authKey, { state with EditingUser = Some { editingUser with Data = { editingUser.Data with State = Form.View.State.Error errorMessage } } }),
            Cmd.none
        | _ -> state, Cmd.none
    | CancelEditUser ->
        match state with
        | Loaded (authKey, state) -> Loaded (authKey, { state with EditingUser = None }), Cmd.ofMsg (Load authKey)
        | _ -> state, Cmd.none

[<ReactComponent>]
let UserAdministration authKey setAuthKeyInvalid (setMenuItems: ReactElement list -> ReactElement) =
    let (state, dispatch) = React.useElmish(init authKey, update, [| authKey :> obj |])

    React.useEffect(fun () ->
        match state with
        | LoadError (_, ExpectedError LoadExistingUsersError.InvalidAuthKey)
        | LoadError (_, ExpectedError LoadExistingUsersError.NotAuthorized) ->
            setAuthKeyInvalid ()
        | _ -> ()
    )

    let acceptsAuthKey =
        match state with
        | Loaded (_, { EditingUserAuthKey = Some (_, WaitingForUserAuthKey) })
        | Loaded (_, { EditingUserAuthKey = Some (_, GettingUserAuthKeyError _) })
        | Loaded (_, { EditingUserAuthKey = Some (_, SaveUserAuthKeyResult (Error _)) }) -> true
        | _ -> false
    React.useAuthentication acceptsAuthKey (Result.map Some >> SetAuthKey >> dispatch)

    match state with
    | NotLoaded -> Html.none // Handled by parent component
    | Loading _ -> View.loadIconBig
    | LoadError (_, ExpectedError LoadExistingUsersError.InvalidAuthKey)
    | LoadError (_, ExpectedError LoadExistingUsersError.NotAuthorized) -> Html.none // Handled by parent component
    | LoadError (authKey, UnexpectedError _) ->
        View.errorNotificationWithRetry "Fehler beim Laden der Daten." (fun () -> dispatch (Load authKey))
    | Loaded (_, { Users = [] }) ->
        View.infoNotification "Keine Benutzer vorhanden." []
    | Loaded (_, state) ->
        React.fragment [
            setMenuItems [
                Html.a [
                    prop.className "!flex items-center gap-2 btn btn-solid btn-green"
                    prop.onClick (fun _ -> dispatch EditNewUser)
                    
                    prop.children [
                        Fa.i [ Fa.Solid.Plus ] []
                        Html.span [ prop.text "Neuer Benutzer" ]
                    ]
                ]
            ]

            Html.div [
                prop.className "container"
                prop.children [
                    Html.table [
                        prop.className "w-full table-fixed"
                        prop.children [
                            Html.thead [
                                Html.tr [
                                    Html.th "Nachname"
                                    Html.th "Vorname"
                                    Html.th "Schlüsselnummer"
                                    Html.th "Rolle"
                                    Html.th []
                                ]
                            ]
                            Html.tbody [
                                for user in state.Users ->
                                    let deleteUserState = Map.tryFind user.Id state.DeleteUserStates
                                    let isDeleted =
                                        match deleteUserState with
                                        | Some (Deleted (Ok _)) -> true
                                        | _ -> false
                                    Html.tr [
                                        prop.classes [
                                            if isDeleted then "opacity-50"
                                        ]

                                        prop.children [
                                            Html.td [
                                                prop.className "uppercase"
                                                prop.text user.Data.LastName.Value
                                            ]
                                            Html.td [
                                                prop.text user.Data.FirstName.Value
                                            ]
                                            Html.td [
                                                Html.div [
                                                    prop.className "flex items-center gap-2"
                                                    prop.children [
                                                        match user.Data.AuthKey with
                                                        | Some authKey ->
                                                            if Set.contains user.Id state.VisibleKeyCodeUserIds then
                                                                Html.span (AuthKey.toString authKey)
                                                            else
                                                                Html.span [ prop.text "●●●●●●●●●●" ]
                                                                Html.button [
                                                                    prop.className "btn btn-white"
                                                                    prop.onClick (fun _ -> dispatch (ShowAuthKey user.Id))
                                                                    prop.disabled isDeleted
                                                                    
                                                                    prop.children [
                                                                        Fa.i [ Fa.Solid.Eye ] []
                                                                    ]
                                                                ]
                                                        | None -> Html.span "-"

                                                        Html.button [
                                                            prop.className "btn btn-white"
                                                            prop.onClick (fun _ -> dispatch (EditAuthKey user))
                                                            prop.disabled isDeleted
                                                            
                                                            prop.children [
                                                                Fa.i [ Fa.Solid.Edit ] []
                                                            ]
                                                        ]
                                                    ]
                                                ]
                                            ]
                                            Html.td [
                                                prop.text (UserRole.label user.Data.Role)
                                            ]
                                            Html.td [
                                                Html.div [
                                                    prop.className "flex gap-2"
                                                    prop.children [
                                                        Html.button [
                                                            prop.className "btn btn-solid btn-blue"
                                                            prop.disabled isDeleted
                                                            prop.onClick (fun _ -> dispatch (EditUser user))
                                                            
                                                            prop.children [
                                                                Fa.i [ Fa.Solid.Edit ] []
                                                            ]
                                                        ]
                                                        Html.button [
                                                            prop.className "btn btn-solid btn-red"
                                                            prop.disabled isDeleted

                                                            match deleteUserState with
                                                            | None -> prop.onClick (fun _ -> dispatch (DeleteUser user.Id))
                                                            | Some LoadingWarnings -> ()
                                                            | Some (LoadedWarnings (Error _)) -> prop.onClick (fun _ -> dispatch (DeleteUser user.Id))
                                                            | Some (LoadedWarnings (Ok _)) -> prop.onClick (fun _ -> dispatch (ForceDeleteUser user.Id))
                                                            | Some Deleting -> ()
                                                            | Some (Deleted (Error _)) -> prop.onClick (fun _ -> dispatch (ForceDeleteUser user.Id))
                                                            | Some (Deleted (Ok _)) -> ()
                                                            
                                                            prop.children [
                                                                Fa.i [ Fa.Solid.TrashAlt ] []
                                                            ]
                                                        ]
                                                    ]
                                                ]
                                                match deleteUserState with
                                                | Some (LoadedWarnings (Ok [])) ->
                                                    Html.span [
                                                        prop.className "text-sm text-musi-green"
                                                        prop.text "Alles gut. Der Benutzer kann jetzt gelöscht werden."
                                                    ]
                                                | Some (LoadedWarnings (Ok warnings)) ->
                                                    Html.div [
                                                        prop.className "text-sm text-musi-red"
                                                        prop.children [
                                                            Html.span "Beachte folgendes, bevor du den Benutzer löschst:"
                                                            Html.ul [
                                                                prop.className "list-disc ml-4"
                                                                prop.children [
                                                                    for warning in warnings ->
                                                                        let text =
                                                                            match warning with
                                                                            | AuthKeyPresent -> "Der Benutzer hat noch einen Schlüssel zugeordnet."
                                                                            | CurrentBalanceNotZero v -> $"Der Benutzer hat noch ein Guthaben von %s{View.formatBalance v}"
                                                                        Html.li text
                                                                ]
                                                            ]
                                                        ]
                                                    ]
                                                | Some (LoadedWarnings (Error _))
                                                | Some (Deleted (Error _)) ->
                                                    Html.span [
                                                        prop.className "text-sm text-musi-red"
                                                        prop.text "Fehler beim Löschen des Benutzers."
                                                    ]
                                                | Some LoadingWarnings
                                                | Some Deleting
                                                | Some (Deleted (Ok ()))
                                                | None -> ()
                                            ]
                                        ]
                                    ]
                            ]
                        ]
                    ]
                ]
            ]

            match state.EditingUserAuthKey with
            | Some (user, WaitingForUserAuthKey) ->
                View.modal $"Schlüsselnummer von %s{user.Data.LastName.Value} %s{user.Data.FirstName.Value} ändern" (fun () -> dispatch CancelEditAuthKey) [
                    Html.div [
                        prop.className "flex flex-col gap-2 items-center text-musi-gold"
                        prop.children [
                            Fa.i [ Fa.Solid.Key; Fa.Size Fa.Fa8x ] []
                            Html.span [
                                prop.className "text-center text-3xl"
                                prop.children [
                                    Html.text "Halte den Musischlüssel"
                                    Html.br []
                                    Html.text "zum Lesegerät"
                                ]
                            ]
                        ]
                    ]
                ] [
                    Html.button [
                        prop.className "btn btn-solid btn-red"
                        prop.onClick (fun _ -> dispatch (SetAuthKey (Ok None)))
                        prop.text "Schlüsselnummer löschen"
                    ]
                ]
            | Some (user, GettingUserAuthKeyError error) ->
                View.modalAuthError $"Schlüsselnummer von %s{user.Data.LastName.Value} %s{user.Data.FirstName.Value} ändern" error (fun () -> dispatch (EditAuthKey user)) (fun () -> dispatch CancelEditAuthKey)
            | Some (user, SavingUserAuthKey _) ->
                View.modal $"Schlüsselnummer von %s{user.Data.LastName.Value} %s{user.Data.FirstName.Value} ändern" (fun () -> dispatch CancelEditAuthKey) [ View.loadIconBig ] []
            | Some (user, SaveUserAuthKeyResult (Ok authKey)) ->
                View.modal $"Schlüsselnummer von %s{user.Data.LastName.Value} %s{user.Data.FirstName.Value} ändern" (fun () -> dispatch CancelEditAuthKey) [
                    Html.div [
                        prop.className "flex flex-col items-center gap-2 text-musi-green"
                        prop.children [
                            Fa.i [ Fa.Solid.Key; Fa.Size Fa.Fa8x ] []
                            Html.span [
                                prop.className "text-center text-3xl"
                                if Option.isSome authKey then prop.text "Schlüssel wurde erfolgreich gespeichert."
                                else prop.text "Schlüssel wurde erfolgreich gelöscht."
                            ]
                        ]
                    ]
                ] []
            | Some (user, SaveUserAuthKeyResult (Error error)) ->
                View.modal $"Schlüsselnummer von %s{user.Data.LastName.Value} %s{user.Data.FirstName.Value} ändern" (fun () -> dispatch CancelEditAuthKey) [
                    Html.div [
                        prop.className "flex flex-col items-center gap-2 text-musi-red"
                        prop.children [
                            Fa.i [ Fa.Solid.Key; Fa.Size Fa.Fa8x ] []
                            Html.span [
                                prop.className "text-center text-3xl"
                                prop.children [
                                    Html.text "Fehler beim Speichern."
                                    Html.br []
                                    match error with
                                    | ExpectedError DowngradeSelfNotAllowed ->
                                        Html.text "Die eigene Rolle darf nicht gewechselt werden."
                                    | ExpectedError RemoveKeyCodeNotAllowed ->
                                        Html.text "Die eigene Schlüsselnummer darf nicht entfernt werden."
                                    | ExpectedError (KeyCodeTaken None) ->
                                        Html.text "Schlüsselnummer ist bereits vergeben."
                                    | ExpectedError (KeyCodeTaken (Some userName)) ->
                                        Html.text $"Schlüsselnummer ist bereits an %s{userName} vergeben."
                                    | ExpectedError SaveUserError.InvalidAuthKey
                                    | ExpectedError SaveUserError.NotAuthorized
                                    | UnexpectedError _ -> Html.text "Versuche es nochmal."
                                ]
                            ]
                        ]
                    ]
                ] []
            | None -> ()

            match state.EditingUser with
            | Some editingUser ->
                let form : Form.Form<UserFormData, Msg> =
                    let firstNameField =
                        Form.textField
                            {
                                Parser = fun value ->
                                    match NotEmptyString.tryCreate value with
                                    | Some v -> Ok v
                                    | None -> Error "Vorname darf nicht leer sein"
                                Value = fun user -> user.FirstName
                                Update = fun v user -> { user with FirstName = v }
                                Error = fun _ -> None
                                Attributes =
                                    {
                                        Label = "Vorname"
                                        Placeholder = ""
                                    }
                            }

                    let lastNameField =
                        Form.textField
                            {
                                Parser = fun value ->
                                    match NotEmptyString.tryCreate value with
                                    | Some v -> Ok v
                                    | None -> Error "Nachname darf nicht leer sein"
                                Value = fun user -> user.LastName
                                Update = fun v user -> { user with LastName = v }
                                Error = fun _ -> None
                                Attributes =
                                    {
                                        Label = "Nachname"
                                        Placeholder = ""
                                    }
                            }

                    let roleField =
                        Form.radioField
                            {
                                Parser = fun value ->
                                    match UserRole.tryParse value with
                                    | Some role -> Ok role
                                    | None -> Error (sprintf "Ungültige Rolle \"%s\"" value)
                                Value = fun user -> user.Role
                                Update = fun v user -> { user with Role = v }
                                Error = fun _ -> None
                                Attributes =
                                    {
                                        Label = "Rolle"
                                        Options =
                                            [ User; Admin ]
                                            |> List.map (fun role ->
                                                (UserRole.toString role, UserRole.label role)
                                            )
                                    }
                            }

                    let onSubmit = fun firstName lastName role ->
                        SaveUser { FirstName = firstName; LastName = lastName; AuthKey = None; Role = role }

                    Form.succeed onSubmit
                    |> Form.append firstNameField
                    |> Form.append lastNameField
                    |> Form.append roleField

                let title =
                    match editingUser.Id with
                    | Some _ -> "Benutzer bearbeiten"
                    | None -> "Benutzer anlegen"

                View.form title form editingUser.Data dispatch CancelEditUser FormChanged
            | None -> ()
        ]
