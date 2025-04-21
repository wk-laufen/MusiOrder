module UserAdministration

open Api
open Api.UserAdministration
open Elmish
open Fable.Form.Simple
open Feliz
open Feliz.UseElmish
open global.JS
open MusiOrder.Models
open MusiOrder.Models.UserAdministration

type AddingUserAuthKeyState =
    | WaitingForUserAuthKey
    | GettingUserAuthKeyError of React.AuthenticationError
    | SavingAuthKey of AuthKey
    | SavingAuthKeyResult of Result<AuthKey, ApiError<SaveUserError>>

type UserFormData = {
    FirstName: string
    LastName: string
    Role: string
}
module UserFormData =
    let fromUserData (v: ExistingUserData) =
        {
            FirstName = v.FirstName.Value
            LastName = v.LastName.Value
            Role = UserRole.toString v.Role
        }
    let empty =
        {
            FirstName = ""
            LastName = ""
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

type RemovingUserAuthKeyState =
    | RemovingUserAuthKey
    | RemovedUserAuthKey of Result<unit, ApiError<SaveUserError>>

type LoadedModel = {
    Users: ExistingUser list
    VisibleAuthKeyUserIds: Set<UserId>
    AddingUserAuthKey: (ExistingUser * AddingUserAuthKeyState) option
    RemoveAuthKeyStates: Map<UserId * AuthKey, RemovingUserAuthKeyState>
    EditingUser: EditingUser option
    DeleteUserStates: Map<UserId, DeleteUserState>
}
module LoadedModel =
    let create users = {
        Users = users
        VisibleAuthKeyUserIds = Set.empty
        AddingUserAuthKey = None
        RemoveAuthKeyStates = Map.empty
        EditingUser = None
        DeleteUserStates = Map.empty
    }


type Model =
    | NotLoaded
    | Loading of AuthKey option
    | LoadError of AuthKey option * ApiError<LoadExistingUsersError>
    | Loaded of AuthKey option * LoadedModel

type Msg =
    | Load of AuthKey option
    | LoadResult of Result<ExistingUser list, ApiError<LoadExistingUsersError>>
    | ShowAuthKeys of UserId
    | AddAuthKey of ExistingUser
    | SaveNewAuthKey of Result<AuthKey, React.AuthenticationError>
    | SaveNewAuthKeyResult of Result<AuthKey, ApiError<SaveUserError>>
    | CancelAddAuthKey
    | RemoveUserAuthKey of UserId * AuthKey
    | RemoveUserAuthKeyResult of UserId * AuthKey * Result<unit, ApiError<SaveUserError>>
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
            Loaded (authKey, LoadedModel.create users),
            Cmd.none
        | _ -> state, Cmd.none
    | LoadResult (Error e) ->
        match state with
        | Loading authKey -> LoadError (authKey, e), Cmd.none
        | _ -> state, Cmd.none
    | ShowAuthKeys userId ->
        match state with
        | Loaded (authKey, state) ->
            Loaded (authKey, { state with VisibleAuthKeyUserIds = Set.add userId state.VisibleAuthKeyUserIds }),
            Cmd.none
        | _ -> state, Cmd.none
    | AddAuthKey user ->
        match state with
        | Loaded (authKey, state) ->
            Loaded (authKey, { state with AddingUserAuthKey = Some (user, WaitingForUserAuthKey) }),
            Cmd.none
        | _ -> state, Cmd.none
    | SaveNewAuthKey (Ok userAuthKey) ->
        match state with
        | Loaded (authKey, ({ AddingUserAuthKey = Some (user, _) } as state)) ->
            Loaded (authKey, { state with AddingUserAuthKey = Some (user, SavingAuthKey userAuthKey) }),
            Cmd.OfAsync.perform (updateUser authKey user.Id) { PatchUserData.empty with AddAuthKeys = [userAuthKey] } (Result.map (fun () -> userAuthKey) >> SaveNewAuthKeyResult)
        | _ -> state, Cmd.none
    | SaveNewAuthKey (Error error) ->
        match state with
        | Loaded (authKey, ({ AddingUserAuthKey = Some (user, _) } as state)) ->
            Loaded (authKey, { state with AddingUserAuthKey = Some (user, GettingUserAuthKeyError error) }),
            Cmd.none
        | _ -> state, Cmd.none
    | SaveNewAuthKeyResult result ->
        match state with
        | Loaded (authKey, ({ AddingUserAuthKey = Some (user, _) } as state)) ->
            Loaded (authKey, { state with AddingUserAuthKey = Some (user, SavingAuthKeyResult result) }),
            Cmd.none
        | _ -> state, Cmd.none
    | CancelAddAuthKey ->
        match state with
        | Loaded (authKey, state) ->
            Loaded (authKey, { state with AddingUserAuthKey = None }),
            Cmd.ofMsg (Load authKey)
        | _ -> state, Cmd.none
    | RemoveUserAuthKey (userId, authKeyToRemove) ->
        match state with
        | Loaded (authKey, state) ->
            Loaded (authKey, { state with RemoveAuthKeyStates = state.RemoveAuthKeyStates |> Map.add (userId, authKeyToRemove) RemovingUserAuthKey }),
            Cmd.OfAsync.perform (updateUser authKey userId) { PatchUserData.empty with RemoveAuthKeys = [authKeyToRemove] } (fun result -> RemoveUserAuthKeyResult (userId, authKeyToRemove, result))
        | _ -> state, Cmd.none
    | RemoveUserAuthKeyResult (userId, authKeyToRemove, result) ->
        match state with
        | Loaded (authKey, state) ->
            Loaded (authKey, { state with RemoveAuthKeyStates = state.RemoveAuthKeyStates |> Map.add (userId, authKeyToRemove) (RemovedUserAuthKey result) }),
            Cmd.none
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
                | ExpectedError SaveUserError.RemoveActiveAuthKeyNotAllowed -> "Fehler beim Speichern des Benutzers: Die eigene Schlüsselnummer darf nicht entfernt werden."
                | ExpectedError (KeyCodeTaken []) -> "Fehler beim Speichern des Benutzers: Schlüsselnummer ist bereits vergeben."
                | ExpectedError (KeyCodeTaken [userName]) -> $"Fehler beim Speichern des Benutzers: Schlüsselnummer ist bereits an %s{userName} vergeben."
                | ExpectedError (KeyCodeTaken userNames) -> $"""Fehler beim Speichern des Benutzers: Schlüsselnummer ist bereits vergeben an: %s{String.concat ", " userNames}"""
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
        | Loaded (_, { AddingUserAuthKey = Some (_, WaitingForUserAuthKey) })
        | Loaded (_, { AddingUserAuthKey = Some (_, GettingUserAuthKeyError _) })
        | Loaded (_, { AddingUserAuthKey = Some (_, SavingAuthKeyResult (Error _)) }) -> true
        | _ -> false
    React.useAuthentication acceptsAuthKey (SaveNewAuthKey >> dispatch)

    let authKeysView (user: ExistingUser) isDeleted areAuthKeysVisible removeAuthKeyStates =
        Html.div [
            prop.classes [
                "flex gap-2"
                if areAuthKeysVisible then "flex-col items-start"
                else "flex-row items-center"
            ]
            prop.children [
                match user.Data.AuthKeys with
                | [] -> Html.span "Keine"
                | authKeys ->
                    if areAuthKeysVisible then
                        Html.div [
                            prop.className "flex flex-col gap-2"
                            prop.children [
                                yield! authKeys
                                |> List.map (fun authKey ->
                                    let isRemoving =
                                        match removeAuthKeyStates |> Map.tryFind (user.Id, authKey) with
                                        | Some RemovingUserAuthKey -> true
                                        | None
                                        | Some (RemovedUserAuthKey (Ok ()))
                                        | Some (RemovedUserAuthKey (Error _)) -> false
                                    let isRemoved =
                                        match removeAuthKeyStates |> Map.tryFind (user.Id, authKey) with
                                        | Some (RemovedUserAuthKey (Ok ())) -> true
                                        | None
                                        | Some RemovingUserAuthKey
                                        | Some (RemovedUserAuthKey (Error _)) -> false
                                    Html.div [
                                        prop.classes [
                                            "flex items-center gap-2"
                                            if isRemoving then "animate-pulse"
                                            elif isRemoved then "opacity-50"
                                        ]
                                        prop.children [
                                            Html.span [
                                                prop.className "flex items-center gap-2 border rounded-lg py-2 px-4"
                                                prop.children [
                                                    match authKey with
                                                    | NFCAuthKey keyCode ->
                                                        Html.i [ prop.className "fas fa-id-card" ]
                                                        Html.span keyCode
                                                ]
                                            ]
                                            Html.button [
                                                prop.className "btn btn-red"
                                                prop.onClick (fun _ -> dispatch (RemoveUserAuthKey (user.Id, authKey)))
                                                prop.disabled (isDeleted || isRemoving || isRemoved)
                                                prop.children [
                                                    Html.i [ prop.className "fas fa-trash-alt" ]
                                                ]
                                            ]
                                            match removeAuthKeyStates |> Map.tryFind (user.Id, authKey) with
                                            | None
                                            | Some RemovingUserAuthKey
                                            | Some (RemovedUserAuthKey (Ok ())) -> ()
                                            | Some (RemovedUserAuthKey (Error e)) -> Html.span [
                                                prop.className "text-musi-red"
                                                match e with
                                                | ExpectedError SaveUserError.RemoveActiveAuthKeyNotAllowed ->
                                                    prop.text "Die aktuell verwendete Schlüsselnummer darf nicht entfernt werden."
                                                | ExpectedError DowngradeSelfNotAllowed
                                                | ExpectedError (KeyCodeTaken _)
                                                | ExpectedError SaveUserError.InvalidAuthKey
                                                | ExpectedError SaveUserError.NotAuthorized
                                                | UnexpectedError _ ->
                                                    prop.text "Fehler beim Entfernen der Schlüsselnummer."
                                            ]
                                        ]
                                    ]
                                )
                            ]
                        ]
                    else
                        Html.button [
                            prop.className "btn btn-white !flex gap-2 items-center"
                            prop.onClick (fun _ -> dispatch (ShowAuthKeys user.Id))
                            prop.disabled isDeleted
                            prop.children [
                                Html.i [ prop.className "fas fa-eye" ]
                                Html.span $"{authKeys.Length}"
                            ]
                        ]

                Html.button [
                    prop.className "btn btn-white"
                    prop.onClick (fun _ -> dispatch (AddAuthKey user))
                    prop.disabled isDeleted
                    
                    prop.children [
                        Html.i [ prop.className "fas fa-plus" ]
                    ]
                ]
            ]
        ]

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
                        Html.i [ prop.className "fas fa-plus" ]
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
                                    Html.th "Schlüsselnummern"
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
                                                let areAuthKeysVisible = Set.contains user.Id state.VisibleAuthKeyUserIds
                                                authKeysView user isDeleted areAuthKeysVisible state.RemoveAuthKeyStates
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
                                                                Html.i [ prop.className "fas fa-edit" ]
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
                                                                Html.i [ prop.className "fas fa-trash-alt" ]
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

            match state.AddingUserAuthKey with
            | Some (user, WaitingForUserAuthKey) ->
                View.modal $"Neue Schlüsselnummer für %s{user.Data.LastName.Value} %s{user.Data.FirstName.Value} anlegen" (fun () -> dispatch CancelAddAuthKey) [
                    Html.div [
                        prop.className "flex flex-col gap-2 items-center text-musi-gold"
                        prop.children [
                            Html.i [ prop.className "fas fa-key fa-8x" ]
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
                ] []
            | Some (user, GettingUserAuthKeyError error) ->
                View.modalAuthError $"Neue Schlüsselnummer für %s{user.Data.LastName.Value} %s{user.Data.FirstName.Value} anlegen" error (fun () -> dispatch (AddAuthKey user)) (fun () -> dispatch CancelAddAuthKey)
            | Some (user, SavingAuthKey _) ->
                View.modal $"Neue Schlüsselnummer für %s{user.Data.LastName.Value} %s{user.Data.FirstName.Value} anlegen" (fun () -> dispatch CancelAddAuthKey) [ View.loadIconBig ] []
            | Some (user, SavingAuthKeyResult (Ok authKey)) ->
                View.modal $"Neue Schlüsselnummer für %s{user.Data.LastName.Value} %s{user.Data.FirstName.Value} anlegen" (fun () -> dispatch CancelAddAuthKey) [
                    Html.div [
                        prop.className "flex flex-col items-center gap-2 text-musi-green"
                        prop.children [
                            Html.i [ prop.className "fas fa-key fa-8x" ]
                            Html.span [
                                prop.className "text-center text-3xl"
                                prop.text "Schlüsselnummer wurde erfolgreich hinzugefügt."
                            ]
                        ]
                    ]
                ] []
            | Some (user, SavingAuthKeyResult (Error error)) ->
                View.modal $"Neue Schlüsselnummer für %s{user.Data.LastName.Value} %s{user.Data.FirstName.Value} anlegen" (fun () -> dispatch CancelAddAuthKey) [
                    Html.div [
                        prop.className "flex flex-col items-center gap-2 text-musi-red"
                        prop.children [
                            Html.i [ prop.className "fas fa-key fa-8x" ]
                            Html.span [
                                prop.className "text-center text-3xl"
                                prop.children [
                                    Html.text "Fehler beim Speichern."
                                    Html.br []
                                    match error with
                                    | ExpectedError DowngradeSelfNotAllowed ->
                                        Html.text "Die eigene Rolle darf nicht gewechselt werden."
                                    | ExpectedError SaveUserError.RemoveActiveAuthKeyNotAllowed ->
                                        Html.text "Die eigene Schlüsselnummer darf nicht entfernt werden."
                                    | ExpectedError (KeyCodeTaken []) ->
                                        Html.text "Schlüsselnummer ist bereits vergeben."
                                    | ExpectedError (KeyCodeTaken [userName]) ->
                                        Html.text $"Schlüsselnummer ist bereits an %s{userName} vergeben."
                                    | ExpectedError (KeyCodeTaken userNames) ->
                                        Html.text $"""Fehler beim Speichern des Benutzers: Schlüsselnummer ist bereits vergeben an: %s{String.concat ", " userNames}"""
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
                                            [ User; OrderAssistant; Admin ]
                                            |> List.map (fun role ->
                                                (UserRole.toString role, UserRole.label role)
                                            )
                                    }
                            }

                    let onSubmit = fun firstName lastName role ->
                        SaveUser { FirstName = firstName; LastName = lastName; AuthKeys = []; Role = role }

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
