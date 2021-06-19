module UserAdministration

open Api
open Elmish
open Fable.FontAwesome
open Fable.Form.Simple
open Fable.Form.Simple.Bulma
open Feliz
open Feliz.Bulma
open Feliz.UseElmish
open global.JS
open MusiOrder.Models

type UserFormData = {
    FirstName: string
    LastName: string
    AuthKey: string
    Role: string
}
module UserFormData =
    let fromUserData (v: ExistingUserData) =
        {
            FirstName = v.FirstName
            LastName = v.LastName
            AuthKey = v.AuthKey |> Option.map AuthKey.toString |> Option.defaultValue ""
            Role = UserRole.toString v.Role
        }

type EditingUser = {
    Id: string option
    Data: Form.View.Model<UserFormData>
}

type LoadedModel = {
    Users: ExistingUserData list
    VisibleKeyCodeUserIds: Set<string>
    EditingUser: EditingUser option
}

type Model =
    | NotLoaded
    | Loading of AuthKey
    | LoadError of AuthKey * FetchError
    | Loaded of AuthKey * LoadedModel

type Msg =
    | Load of AuthKey
    | LoadResult of Result<ExistingUserData list, FetchError>
    | ShowAuthKey of userId: string
    | EditUser of ExistingUserData
    | FormChanged of Form.View.Model<UserFormData>
    | SaveUser of NewUserData
    | SaveUserResult of Result<unit, exn>
    | CancelEditUser

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
            Loaded (authKey, { Users = users; VisibleKeyCodeUserIds = Set.empty; EditingUser = None }),
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
            Loaded (authKey, { state with EditingUser = Some { Id = Some user.Id; Data = Form.View.idle (UserFormData.fromUserData user) } }),
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
                | Some userId -> Cmd.OfAsync.either (updateUser authKey userId) user (Ok >> SaveUserResult) (Error >> SaveUserResult)
                | None -> Cmd.OfAsync.either (createUser authKey) user (Ok >> SaveUserResult) (Error >> SaveUserResult)
            state, cmd
        | _ -> state, Cmd.none
    | SaveUserResult (Ok ()) ->
        match state with
        | Loaded (authKey, ({ EditingUser = Some editingUser } as state)) ->
            Loaded (authKey, { state with EditingUser = Some { editingUser with Data = { editingUser.Data with State = Form.View.State.Success "Benutzer erfolgreich gespeichert." } } }),
            Cmd.none
        | _ -> state, Cmd.none
    | SaveUserResult (Error _) ->
        match state with
        | Loaded (authKey, ({ EditingUser = Some editingUser } as state)) ->
            Loaded (authKey, { state with EditingUser = Some { editingUser with Data = { editingUser.Data with State = Form.View.State.Success "Fehler beim Speichern des Benutzers." } } }),
            Cmd.none
        | _ -> state, Cmd.none
    | CancelEditUser ->
        match state with
        | Loaded (authKey, state) -> Loaded (authKey, { state with EditingUser = None }), Cmd.none
        | _ -> state, Cmd.none

[<ReactComponent>]
let UserAdministration authKey setAuthKeyInvalid (setMenuItems: ReactElement list -> ReactElement) =
    let (state, dispatch) = React.useElmish(init authKey, update, [| authKey :> obj |])

    match state with
    | NotLoaded -> Html.none // Handled by parent component
    | Loading _ -> View.loadIconBig
    | LoadError (_, Forbidden) ->
        setAuthKeyInvalid ()
        Html.none // Handled by parent component
    | LoadError (authKey, Other _) ->
        View.errorNotificationWithRetry "Fehler beim Laden der Daten." (fun () -> dispatch (Load authKey))
    | Loaded (_, { Users = [] }) ->
        View.infoNotification "Keine Benutzer vorhanden"
    | Loaded (_, state) ->
        Html.div [
            Bulma.container [
                Bulma.table [
                    table.isFullWidth
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
                                                                    prop.text "●●●●●●●●●●"
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
                                            prop.text (UserRole.label user.Role)
                                        ]
                                        Html.td [
                                            Bulma.level [
                                                Bulma.levelLeft [
                                                    Bulma.levelItem [
                                                        Bulma.button.a [
                                                            color.isWarning
                                                            prop.onClick (fun _ -> dispatch (EditUser user))
                                                            
                                                            prop.children [
                                                                Bulma.icon [ Fa.i [ Fa.Solid.Edit ] [] ]
                                                            ]
                                                        ]
                                                    ]
                                                ]
                                            ]
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
                                    if not <| System.String.IsNullOrWhiteSpace value then
                                        Ok value
                                    else
                                        Error "Vorname darf nicht leer sein"
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
                                    if not <| System.String.IsNullOrWhiteSpace value then
                                        Ok value
                                    else
                                        Error "Nachname darf nicht leer sein"
                                Value = fun user -> user.LastName
                                Update = fun v user -> { user with LastName = v }
                                Error = fun _ -> None
                                Attributes =
                                    {
                                        Label = "Nachname"
                                        Placeholder = ""
                                    }
                            }
                    
                    let authKeyField =
                        Form.textField
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

                View.modal title (fun () -> dispatch CancelEditUser)
                    [
                        Html.div [
                            prop.className "feliz-form"
                            prop.children [
                                Form.View.asHtml
                                    {
                                        Dispatch = dispatch
                                        OnChange = FormChanged
                                        Action = "Speichern"
                                        Validation = Form.View.ValidateOnSubmit
                                    }
                                    form
                                    editingUser.Data
                            ]
                        ]
                    ]
                    [
                        Bulma.level [
                            prop.classes [ "is-flex-grow-1" ]
                            prop.children [
                                Bulma.levelLeft []
                                Bulma.levelRight [
                                    Bulma.levelItem [
                                        Bulma.button.a [
                                            color.isPrimary
                                            if editingUser.Data.State = Form.View.Loading then button.isLoading
                                            prop.children [
                                                Bulma.icon [ Fa.i [ Fa.Solid.Save ] [] ]
                                                Html.span "Speichern"
                                            ]
                                            prop.onClick (fun e ->
                                                // Trigger submit button click
                                                e.currentTarget :?> Browser.Types.HTMLElement
                                                |> fun e -> e.closest(".modal-card")
                                                |> Option.get
                                                |> fun e -> e.querySelector(".field:last-child button") :?> Browser.Types.HTMLButtonElement
                                                |> fun e -> e.click()
                                            )
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
            | None -> ()
        ]
