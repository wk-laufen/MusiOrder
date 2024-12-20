module UserAdministration

open Api
open Api.UserAdministration
open Elmish
open Fable.FontAwesome
open Fable.Form.Simple
open Fable.Form.Simple.Bulma
open Feliz
open Feliz.Bulma
open Feliz.Bulma.Operators
open Feliz.UseElmish
open global.JS
open MusiOrder.Models
open MusiOrder.Models.UserAdministration

type UserFormData = {
    FirstName: string
    LastName: string
    AuthKey: string
    Role: string
}
module UserFormData =
    let fromUserData (v: UserData) =
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
    | EditUser of ExistingUser
    | DeleteUser of UserId
    | DeleteUserResult of UserId * Result<DeleteUserWarning list, ApiError<DeleteUserError>>
    | ForceDeleteUser of UserId
    | ForceDeleteUserResult of UserId * Result<unit, ApiError<ForceDeleteUserError>>
    | EditNewUser
    | FormChanged of Form.View.Model<UserFormData>
    | SaveUser of UserData
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
            Loaded (authKey, { Users = users; VisibleKeyCodeUserIds = Set.empty; EditingUser = None; DeleteUserStates = Map.empty }),
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
                | Some userId -> Cmd.OfAsync.perform (updateUser authKey userId) user (Result.map (fun () -> userId) >> SaveUserResult)
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
                | ExpectedError DowngradeSelfNotAllowed -> "Fehler beim Speichern des Benutzers: Rollenwechsel nicht erlaubt."
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

    match state with
    | NotLoaded -> Html.none // Handled by parent component
    | Loading _ -> View.loadIconBig
    | LoadError (_, ExpectedError LoadExistingUsersError.InvalidAuthKey)
    | LoadError (_, ExpectedError LoadExistingUsersError.NotAuthorized) -> Html.none // Handled by parent component
    | LoadError (authKey, UnexpectedError _) ->
        View.errorNotificationWithRetry "Fehler beim Laden der Daten." (fun () -> dispatch (Load authKey))
    | Loaded (_, { Users = [] }) ->
        View.infoNotification "Keine Benutzer vorhanden"
    | Loaded (_, state) ->
        React.fragment [
            setMenuItems [
                Bulma.levelItem [
                    Bulma.button.a [
                        color.isSuccess
                        prop.onClick (fun _ -> dispatch EditNewUser)
                        
                        prop.children [
                            Bulma.icon [ Fa.i [ Fa.Solid.Plus ] [] ]
                            Html.span [ prop.text "Neuer Benutzer" ]
                        ]
                    ]
                ]
            ]

            Bulma.container [
                Bulma.table [
                    table.isFullWidth
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
                                    if isDeleted then color.hasTextGreyLight

                                    prop.children [
                                        Html.td [
                                            text.isUppercase
                                            prop.text user.Data.LastName.Value
                                        ]
                                        Html.td [
                                            prop.text user.Data.FirstName.Value
                                        ]
                                        Html.td [
                                            match user.Data.AuthKey with
                                            | Some authKey ->
                                                if Set.contains user.Id state.VisibleKeyCodeUserIds then
                                                    prop.text (AuthKey.toString authKey)
                                                else
                                                    prop.children [
                                                        Bulma.level [
                                                            Bulma.levelLeft [
                                                                Bulma.levelItem [
                                                                    prop.text "●●●●●●●●●●"
                                                                ]
                                                                Bulma.levelItem [
                                                                    Bulma.button.a [
                                                                        prop.onClick (fun _ -> dispatch (ShowAuthKey user.Id))
                                                                        prop.disabled isDeleted
                                                                        
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
                                            prop.text (UserRole.label user.Data.Role)
                                        ]
                                        Html.td [
                                            Bulma.buttons [
                                                Bulma.button.a [
                                                    color.isWarning
                                                    prop.disabled isDeleted
                                                    prop.onClick (fun _ -> dispatch (EditUser user))
                                                    
                                                    prop.children [
                                                        Bulma.icon [ Fa.i [ Fa.Solid.Edit ] [] ]
                                                    ]
                                                ]
                                                Bulma.button.a [
                                                    color.isDanger
                                                    prop.disabled isDeleted

                                                    match deleteUserState with
                                                    | None -> prop.onClick (fun _ -> dispatch (DeleteUser user.Id))
                                                    | Some LoadingWarnings -> button.isLoading
                                                    | Some (LoadedWarnings (Error _)) -> prop.onClick (fun _ -> dispatch (DeleteUser user.Id))
                                                    | Some (LoadedWarnings (Ok _)) -> prop.onClick (fun _ -> dispatch (ForceDeleteUser user.Id))
                                                    | Some Deleting -> button.isLoading
                                                    | Some (Deleted (Error _)) -> prop.onClick (fun _ -> dispatch (ForceDeleteUser user.Id))
                                                    | Some (Deleted (Ok _)) -> ()
                                                    
                                                    prop.children [
                                                        Bulma.icon [ Fa.i [ Fa.Solid.TrashAlt ] [] ]
                                                    ]
                                                ]
                                            ]
                                            match deleteUserState with
                                            | Some (LoadedWarnings (Ok [])) ->
                                                Bulma.help [
                                                    color.isSuccess
                                                    prop.text "Alles gut. Der Benutzer kann jetzt gelöscht werden."
                                                ]
                                            | Some (LoadedWarnings (Ok warnings)) ->
                                                Bulma.content [
                                                    Html.div [
                                                        prop.className "help"
                                                        ++ color.hasTextDanger
                                                        prop.children [
                                                            Html.span "Beachte folgendes, bevor du den Benutzer löschst:"
                                                            Html.ul [
                                                                for warning in warnings ->
                                                                    Html.li [ prop.text (DeleteUserWarning.label warning) ]
                                                            ]
                                                        ]
                                                    ]
                                                ]
                                            | Some (Deleted (Error _)) ->
                                                Bulma.help [
                                                    color.isDanger
                                                    prop.text "Fehler beim Löschen des Benutzers."
                                                ]
                                            | _ -> ()
                                        ]
                                    ]
                                ]
                        ]
                    ]
                ]
            ]

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

                    // TODO prevent submitting form when entering auth key using physical key (sends `Enter` at the end)
                    let authKeyField =
                        let config: Fable.Form.Base.FieldConfig<Field.TextField.Attributes, string, _, _> =
                            {
                                Parser = fun value ->
                                    if System.String.IsNullOrWhiteSpace value then Ok None
                                    else Ok (Some (AuthKey value))
                                Value = fun user -> user.AuthKey
                                Update = fun v user -> { user with AuthKey = v }
                                Error = fun _ -> None
                                Attributes =
                                    {
                                        Label = "Schlüsselnummer"
                                        Placeholder = "Schlüssel zum Lesegerät halten, Nummer händisch eingeben oder leer lassen"
                                    }
                            }
                        Fable.Form.Base.field (fun _ -> false) (fun x -> Form.Field.Text (Form.TextPassword, x)) config

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

                    let onSubmit = fun firstName lastName authKey role ->
                        SaveUser { FirstName = firstName; LastName = lastName; AuthKey = authKey; Role = role }

                    Form.succeed onSubmit
                    |> Form.append firstNameField
                    |> Form.append lastNameField
                    |> Form.append authKeyField
                    |> Form.append roleField

                let title =
                    match editingUser.Id with
                    | Some _ -> "Benutzer bearbeiten"
                    | None -> "Benutzer anlegen"

                let formView (config: Form.View.FormConfig<Msg>) =
                    Html.form [
                        prop.onSubmit (fun ev ->
                            ev.stopPropagation()
                            ev.preventDefault()

                            config.OnSubmit
                            |> Option.map dispatch
                            |> Option.defaultWith ignore
                        )
                        prop.children [
                            View.modal title (fun () -> dispatch CancelEditUser)
                                [
                                    yield! config.Fields
                                ]
                                [
                                    Bulma.field.div [
                                        prop.classes [ "is-flex-grow-1" ]
                                        field.isGrouped
                                        field.isGroupedRight

                                        prop.children [
                                            match config.State with
                                            | Form.View.Error error ->
                                                Bulma.control.div [
                                                    prop.classes [ "is-align-self-center"; "is-flex-shrink-1" ]
                                                    prop.children [
                                                        Form.View.errorMessage error
                                                    ]
                                                ]
                                            | Form.View.Success success ->
                                                Bulma.control.div [
                                                    prop.classes [ "is-align-self-center"; "is-flex-shrink-1" ]
                                                    text.hasTextCentered
                                                    color.hasTextSuccess
                                                    text.hasTextWeightBold

                                                    prop.text success
                                                ]
                                            | Form.View.Loading
                                            | Form.View.Idle -> ()

                                            Bulma.control.div [
                                                Bulma.button.button [
                                                    color.isPrimary
                                                    prop.text config.Action
                                                    if config.State = Form.View.Loading then
                                                        button.isLoading
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                        ]
                    ]

                let htmlViewConfig = { Form.View.htmlViewConfig with Form = formView }
                let config: Form.View.ViewConfig<_, _> =
                    {
                        Dispatch = dispatch
                        OnChange = FormChanged
                        Action = "Speichern"
                        Validation = Form.View.ValidateOnSubmit
                    }
                Form.View.custom htmlViewConfig config form editingUser.Data
            | None -> ()
        ]
