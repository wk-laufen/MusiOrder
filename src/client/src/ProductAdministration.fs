module ProductAdministration

open Api
open Api.ProductAdministration
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
open MusiOrder.Models.ProductAdministration

type ProductFormData = {
    Name: string
    Price: string
    State: string
}
module ProductFormData =
    let fromProductData (v: ProductData) =
        {
            Name = v.Name.Value
            Price = NonNegativeDecimal.value v.Price |> sprintf "%.2f"
            State = ProductState.toString v.State
        }
    let empty =
        {
            Name = ""
            Price = ""
            State = ProductState.toString Enabled
        }

type EditingProduct = {
    Id: ProductId option
    Data: Form.View.Model<ProductFormData>
}

type MoveProductGroupState =
    | MovingProductGroup
    | MovedProductGroup of Result<unit, ApiError<MoveProductGroupError>>

type MoveProductState =
    | MovingProduct
    | MovedProduct of Result<unit, ApiError<MoveProductError>>

type DeleteProductState =
    | DeletingProduct
    | DeletedProduct of Result<unit, ApiError<DeleteProductError>>

type LoadedModel = {
    Products: ExistingProductGroup list
    MovingUpProductGroup: (ProductGroupId * MoveProductGroupState) option
    MovingDownProductGroup: (ProductGroupId * MoveProductGroupState) option
    EditingProduct: EditingProduct option
    MovingUpProduct: (ProductId * MoveProductState) option
    MovingDownProduct: (ProductId * MoveProductState) option
    ProductStates: Map<ProductId, DeleteProductState>
}
module LoadedModel =
    let init products =
        {
            Products = products
            MovingUpProductGroup = None
            MovingDownProductGroup = None
            EditingProduct = None
            MovingUpProduct = None
            MovingDownProduct = None
            ProductStates = Map.empty
        }

type Model =
    | NotLoaded
    | Loading of AuthKey
    | LoadError of AuthKey * ApiError<LoadExistingProductsError>
    | Loaded of AuthKey * LoadedModel

type Msg =
    | Load of AuthKey
    | LoadResult of Result<ExistingProductGroup list, ApiError<LoadExistingProductsError>>
    | MoveUpProductGroup of ProductGroupId
    | MoveUpProductGroupResult of ProductGroupId * Result<unit, ApiError<MoveProductGroupError>>
    | MoveDownProductGroup of ProductGroupId
    | MoveDownProductGroupResult of ProductGroupId * Result<unit, ApiError<MoveProductGroupError>>
    | EditProduct of ExistingProduct
    | MoveUpProduct of ProductId
    | MoveUpProductResult of ProductId * Result<unit, ApiError<MoveProductError>>
    | MoveDownProduct of ProductId
    | MoveDownProductResult of ProductId * Result<unit, ApiError<MoveProductError>>
    | DeleteProduct of ProductId
    | DeleteProductResult of ProductId * Result<unit, ApiError<DeleteProductError>>
    | EditNewProduct
    | FormChanged of Form.View.Model<ProductFormData>
    | SaveProduct of ProductData
    | SaveProductResult of Result<ProductId, ApiError<SaveProductError>>
    | CancelEditProduct

let init authKey =
    match authKey with
    | Some authKey ->
        NotLoaded, Cmd.ofMsg (Load authKey)
    | None ->
        NotLoaded, Cmd.none

let update msg state =
    match msg with
    | Load authKey ->
        Loading authKey, Cmd.OfAsync.perform loadProductData authKey LoadResult
    | LoadResult (Ok products) ->
        match state with
        | Loading authKey ->
            Loaded (authKey, LoadedModel.init products),
            Cmd.none
        | _ -> state, Cmd.none
    | LoadResult (Error e) ->
        match state with
        | Loading authKey -> LoadError (authKey, e), Cmd.none
        | _ -> state, Cmd.none
    | MoveUpProductGroup groupId ->
        match state with
        | Loaded (authKey, state) when state.MovingUpProductGroup = None ->
            Loaded (authKey, { state with MovingUpProductGroup = Some (groupId, MovingProductGroup) }),
            Cmd.OfAsync.perform (moveUpProductGroup authKey) groupId (fun result -> MoveUpProductGroupResult (groupId, result))
        | _ -> state, Cmd.none
    | MoveUpProductGroupResult (groupId, result) ->
        match state with
        | Loaded (authKey, state) when state.MovingUpProductGroup = Some (groupId, MovingProductGroup) ->
            Loaded (authKey, { state with MovingUpProductGroup = Some (groupId, MovedProductGroup result) }),
            Cmd.ofMsg (Load authKey)
        | _ -> state, Cmd.none
    | MoveDownProductGroup groupId ->
        match state with
        | Loaded (authKey, state) when state.MovingDownProductGroup = None ->
            Loaded (authKey, { state with MovingDownProductGroup = Some (groupId, MovingProductGroup) }),
            Cmd.OfAsync.perform (moveDownProductGroup authKey) groupId (fun result -> MoveDownProductGroupResult (groupId, result))
        | _ -> state, Cmd.none
    | MoveDownProductGroupResult (groupId, result) ->
        match state with
        | Loaded (authKey, state) when state.MovingDownProductGroup = Some (groupId, MovingProductGroup) ->
            Loaded (authKey, { state with MovingDownProductGroup = Some (groupId, MovedProductGroup result) }),
            Cmd.ofMsg (Load authKey)
        | _ -> state, Cmd.none
    | EditProduct product ->
        match state with
        | Loaded (authKey, state) ->
            Loaded (authKey, { state with EditingProduct = Some { Id = Some product.Id; Data = Form.View.idle (ProductFormData.fromProductData product.Data) } }),
            Cmd.none
        | _ -> state, Cmd.none
    | MoveUpProduct productId ->
        match state with
        | Loaded (authKey, state) when state.MovingUpProduct = None ->
            Loaded (authKey, { state with MovingUpProduct = Some (productId, MovingProduct) }),
            Cmd.OfAsync.perform (moveUpProduct authKey) productId (fun result -> MoveUpProductResult (productId, result))
        | _ -> state, Cmd.none
    | MoveUpProductResult (productId, result) ->
        match state with
        | Loaded (authKey, state) when state.MovingUpProduct = Some (productId, MovingProduct) ->
            Loaded (authKey, { state with MovingUpProduct = Some (productId, MovedProduct result) }),
            Cmd.ofMsg (Load authKey)
        | _ -> state, Cmd.none
    | MoveDownProduct productId ->
        match state with
        | Loaded (authKey, state) when state.MovingDownProduct = None ->
            Loaded (authKey, { state with MovingDownProduct = Some (productId, MovingProduct) }),
            Cmd.OfAsync.perform (moveDownProduct authKey) productId (fun result -> MoveDownProductResult (productId, result))
        | _ -> state, Cmd.none
    | MoveDownProductResult (productId, result) ->
        match state with
        | Loaded (authKey, state) when state.MovingDownProduct = Some (productId, MovingProduct) ->
            Loaded (authKey, { state with MovingDownProduct = Some (productId, MovedProduct result) }),
            Cmd.ofMsg (Load authKey)
        | _ -> state, Cmd.none
    | DeleteProduct productId ->
        match state with
        | Loaded (authKey, state) ->
            Loaded (authKey, { state with ProductStates = Map.add productId DeletingProduct state.ProductStates }),
            Cmd.OfAsync.perform (deleteProduct authKey) productId (fun result -> DeleteProductResult (productId, result))
        | _ -> state, Cmd.none
    | DeleteProductResult (productId, result) ->
        match state with
        | Loaded (authKey, state) ->
            Loaded (authKey, { state with ProductStates = Map.add productId (DeletedProduct result) state.ProductStates }),
            Cmd.none
        | _ -> state, Cmd.none
    | EditNewProduct ->
        match state with
        | Loaded (authKey, state) ->
            Loaded (authKey, { state with EditingProduct = Some { Id = None; Data = Form.View.idle ProductFormData.empty } }),
            Cmd.none
        | _ -> state, Cmd.none
    | FormChanged formData ->
        match state with
        | Loaded (authKey, ({ EditingProduct = Some editingProduct } as state)) ->
            Loaded (authKey, { state with EditingProduct = Some { editingProduct with Data = formData } }),
            Cmd.none
        | _ -> state, Cmd.none
    | SaveProduct product ->
        match state with
        | Loaded (authKey, ({ EditingProduct = Some editingProduct } as state)) ->
            let state = Loaded (authKey, { state with EditingProduct = Some { editingProduct with Data = { editingProduct.Data with State = Form.View.State.Loading } } })
            let cmd =
                match editingProduct.Id with
                | Some productId -> Cmd.OfAsync.perform (updateProduct authKey productId) product (Result.map (fun () -> productId) >> SaveProductResult)
                | None -> Cmd.OfAsync.perform (createProduct authKey) product SaveProductResult
            state, cmd
        | _ -> state, Cmd.none
    | SaveProductResult (Ok productId) ->
        match state with
        | Loaded (authKey, ({ EditingProduct = Some editingProduct } as state)) ->
            Loaded (authKey, { state with EditingProduct = Some { editingProduct with Id = Some productId; Data = { editingProduct.Data with State = Form.View.State.Success "Artikel erfolgreich gespeichert." } } }),
            Cmd.none
        | _ -> state, Cmd.none
    | SaveProductResult (Error e) ->
        match state with
        | Loaded (authKey, ({ EditingProduct = Some editingProduct } as state)) ->
            let errorMessage =
                match e with
                | ExpectedError SaveProductError.InvalidAuthKey
                | ExpectedError SaveProductError.NotAuthorized
                | UnexpectedError _ -> "Fehler beim Speichern des Benutzers."
            Loaded (authKey, { state with EditingProduct = Some { editingProduct with Data = { editingProduct.Data with State = Form.View.State.Error errorMessage } } }),
            Cmd.none
        | _ -> state, Cmd.none
    | CancelEditProduct ->
        match state with
        | Loaded (authKey, state) -> Loaded (authKey, { state with EditingProduct = None }), Cmd.ofMsg (Load authKey)
        | _ -> state, Cmd.none

[<ReactComponent>]
let ProductAdministration authKey setAuthKeyInvalid (setMenuItems: ReactElement list -> ReactElement) =
    let (state, dispatch) = React.useElmish(init authKey, update, [| authKey :> obj |])

    React.useEffect(fun () ->
        match state with
        | LoadError (_, ExpectedError LoadExistingProductsError.InvalidAuthKey)
        | LoadError (_, ExpectedError LoadExistingProductsError.NotAuthorized) ->
            setAuthKeyInvalid ()
        | _ -> ()
    )

    match state with
    | NotLoaded -> Html.none // Handled by parent component
    | Loading _ -> View.loadIconBig
    | LoadError (_, ExpectedError LoadExistingProductsError.InvalidAuthKey)
    | LoadError (_, ExpectedError LoadExistingProductsError.NotAuthorized) -> Html.none // Handled by parent component
    | LoadError (authKey, UnexpectedError _) ->
        View.errorNotificationWithRetry "Fehler beim Laden der Daten." (fun () -> dispatch (Load authKey))
    | Loaded (_, { Products = [] }) ->
        View.infoNotification "Keine Artikel vorhanden"
    | Loaded (_, state) ->
        React.fragment [
            setMenuItems [
                Bulma.levelItem [
                    Bulma.button.a [
                        color.isSuccess
                        prop.onClick (fun _ -> dispatch EditNewProduct)
                        
                        prop.children [
                            Bulma.icon [ Fa.i [ Fa.Solid.Plus ] [] ]
                            Html.span [ prop.text "Neuer Artikel" ]
                        ]
                    ]
                ]
            ]

            Bulma.container [
                for (index, group) in List.indexed state.Products do
                    yield Bulma.level [
                        Bulma.levelLeft [
                            Bulma.levelItem [
                                Bulma.title [
                                    prop.text group.Data.Name
                                ]
                            ]
                            Bulma.levelItem [
                                Bulma.buttons [
                                    Bulma.button.a [
                                        color.isSuccess
                                        prop.disabled <| (index = 0)
                                        prop.onClick (fun _ -> dispatch (MoveUpProductGroup group.Id))
                                        match state.MovingUpProductGroup with
                                        | Some (groupId, MovingProductGroup) when groupId = group.Id -> button.isLoading
                                        | _ -> ()
                                        
                                        prop.children [
                                            Bulma.icon [ Fa.i [ Fa.Solid.ArrowUp ] [] ]
                                        ]
                                    ]
                                    Bulma.button.a [
                                        color.isSuccess
                                        prop.disabled <| (index = state.Products.Length - 1)
                                        prop.onClick (fun _ -> dispatch (MoveDownProductGroup group.Id))
                                        match state.MovingDownProductGroup with
                                        | Some (groupId, MovingProductGroup) when groupId = group.Id -> button.isLoading
                                        | _ -> ()
                                        
                                        prop.children [
                                            Bulma.icon [ Fa.i [ Fa.Solid.ArrowDown ] [] ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                    yield Bulma.table [
                        table.isFullWidth
                        prop.children [
                            Html.thead [
                                Html.tr [
                                    Html.th "Name"
                                    Html.th "Preis"
                                    Html.th "Aktiv"
                                    Html.th []
                                ]
                            ]
                            Html.tbody [
                                for (index, product) in List.indexed group.Products ->
                                    let deleteProductState = Map.tryFind product.Id state.ProductStates
                                    let isDeleted =
                                        match deleteProductState with
                                        | Some (DeletedProduct (Ok _)) -> true
                                        | _ -> false
                                    Html.tr [
                                        if isDeleted then color.hasTextGreyLight

                                        prop.children [
                                            Html.td [
                                                prop.text product.Data.Name.Value
                                            ]
                                            Html.td [
                                                prop.textf "%.2f€" (NonNegativeDecimal.value product.Data.Price)
                                            ]
                                            Html.td [
                                                    match product.Data.State with
                                                    | Enabled ->
                                                        color.hasTextSuccess
                                                        prop.text "✔"
                                                    | Disabled ->
                                                        color.hasTextDanger
                                                        prop.text "✘"
                                            ]
                                            Html.td [
                                                Bulma.buttons [
                                                    Bulma.button.a [
                                                        color.isSuccess
                                                        prop.disabled (isDeleted || index = 0)
                                                        prop.onClick (fun _ -> dispatch (MoveUpProduct product.Id))
                                                        match state.MovingUpProduct with
                                                        | Some (productId, MovingProduct) when productId = product.Id -> button.isLoading
                                                        | _ -> ()
                                                        
                                                        prop.children [
                                                            Bulma.icon [ Fa.i [ Fa.Solid.ArrowUp ] [] ]
                                                        ]
                                                    ]
                                                    Bulma.button.a [
                                                        color.isSuccess
                                                        prop.disabled (isDeleted || index = group.Products.Length - 1)
                                                        prop.onClick (fun _ -> dispatch (MoveDownProduct product.Id))
                                                        match state.MovingDownProduct with
                                                        | Some (productId, MovingProduct) when productId = product.Id -> button.isLoading
                                                        | _ -> ()
                                                        
                                                        prop.children [
                                                            Bulma.icon [ Fa.i [ Fa.Solid.ArrowDown ] [] ]
                                                        ]
                                                    ]
                                                    Bulma.button.a [
                                                        color.isWarning
                                                        prop.disabled isDeleted
                                                        prop.onClick (fun _ -> dispatch (EditProduct product))
                                                        
                                                        prop.children [
                                                            Bulma.icon [ Fa.i [ Fa.Solid.Edit ] [] ]
                                                        ]
                                                    ]
                                                    Bulma.button.a [
                                                        color.isDanger
                                                        prop.disabled isDeleted

                                                        match deleteProductState with
                                                        | None -> prop.onClick (fun _ -> dispatch (DeleteProduct product.Id))
                                                        | Some DeletingProduct -> button.isLoading
                                                        | Some (DeletedProduct (Error _)) -> prop.onClick (fun _ -> dispatch (DeleteProduct product.Id))
                                                        | Some (DeletedProduct (Ok _)) -> ()
                                                        
                                                        prop.children [
                                                            Bulma.icon [ Fa.i [ Fa.Solid.TrashAlt ] [] ]
                                                        ]
                                                    ]
                                                ]
                                                match deleteProductState with
                                                | Some (DeletedProduct (Error _)) ->
                                                    Bulma.help [
                                                        color.isDanger
                                                        prop.text "Fehler beim Löschen des Artikels."
                                                    ]
                                                | _ -> ()
                                            ]
                                        ]
                                    ]
                            ]
                        ]
                    ]
            ]

            match state.EditingProduct with
            | Some editingProduct ->
                let form : Form.Form<ProductFormData, Msg> =
                    let nameField =
                        Form.textField
                            {
                                Parser = fun value ->
                                    match NotEmptyString.tryCreate value with
                                    | Some v -> Ok v
                                    | None -> Error "Name darf nicht leer sein"
                                Value = fun product -> product.Name
                                Update = fun v product -> { product with Name = v }
                                Error = fun _ -> None
                                Attributes =
                                    {
                                        Label = "Name"
                                        Placeholder = ""
                                    }
                            }

                    let priceField =
                        Form.textField
                            {
                                Parser = fun value ->
                                    match NonNegativeDecimal.tryParse value with
                                    | Some v -> Ok v
                                    | None -> Error "Kein gültiger Preis"
                                Value = fun product -> product.Price
                                Update = fun v product -> { product with Price = v }
                                Error = fun _ -> None
                                Attributes =
                                    {
                                        Label = "Preis"
                                        Placeholder = ""
                                    }
                            }

                    let stateField =
                        Form.radioField
                            {
                                Parser = fun value ->
                                    match ProductState.tryParse value with
                                    | Some state -> Ok state
                                    | None -> Error (sprintf "Ungültiger Zustand \"%s\"" value)
                                Value = fun product -> product.State
                                Update = fun v product -> { product with State = v }
                                Error = fun _ -> None
                                Attributes =
                                    {
                                        Label = "Aktiv"
                                        Options =
                                            [ Enabled; Disabled ]
                                            |> List.map (fun role ->
                                                (ProductState.toString role, ProductState.label role)
                                            )
                                    }
                            }

                    let onSubmit = fun name price state ->
                        SaveProduct { Name = name; Price = price; State = state }

                    Form.succeed onSubmit
                    |> Form.append nameField
                    |> Form.append priceField
                    |> Form.append stateField

                let title =
                    match editingProduct.Id with
                    | Some _ -> "Artikel bearbeiten"
                    | None -> "Artikel anlegen"

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
                            View.modal title (fun () -> dispatch CancelEditProduct)
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
                Form.View.custom htmlViewConfig config form editingProduct.Data
            | None -> ()
        ]
