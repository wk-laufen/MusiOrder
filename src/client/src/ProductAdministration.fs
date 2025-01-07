module ProductAdministration

open Api
open Api.ProductAdministration
open Elmish
open Fable.FontAwesome
open Fable.Form.Simple
open Feliz
open Feliz.UseElmish
open global.JS
open Fable.Core.JsInterop
open MusiOrder.Models
open MusiOrder.Models.ProductAdministration

module Form =
    open Fable.Form.Base

    let disable flag (form : Form<'Values, 'Output, 'Field>) : Form<'Values, 'Output, 'Field> =
        Form (
            fun values ->
                let filled =
                    fill form values
                {
                    Fields =
                        filled.Fields
                        |> List.map (fun filledField ->
                            { filledField with IsDisabled = flag}
                        )
                    Result = filled.Result
                    IsEmpty = filled.IsEmpty
                }
        )

type ProductFormData = {
    ProductGroupId: string
    Name: string
    Price: string
    State: string
}
module ProductFormData =
    let fromProductData (ProductGroupId productGroupId) (v: ProductData) =
        {
            ProductGroupId = productGroupId
            Name = v.Name.Value
            Price = NonNegativeDecimal.value v.Price |> View.formatNumber
            State = ProductState.toString v.State
        }
    let empty =
        {
            ProductGroupId = ""
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
    | Loading of AuthKey option
    | LoadError of AuthKey option * ApiError<LoadExistingProductsError>
    | Loaded of AuthKey option * LoadedModel

type Msg =
    | Load of AuthKey option
    | LoadResult of Result<ExistingProductGroup list, ApiError<LoadExistingProductsError>>
    | MoveUpProductGroup of ProductGroupId
    | MoveUpProductGroupResult of ProductGroupId * Result<unit, ApiError<MoveProductGroupError>>
    | MoveDownProductGroup of ProductGroupId
    | MoveDownProductGroupResult of ProductGroupId * Result<unit, ApiError<MoveProductGroupError>>
    | EditProduct of ProductGroupId * ExistingProduct
    | MoveUpProduct of ProductId
    | MoveUpProductResult of ProductId * Result<unit, ApiError<MoveProductError>>
    | MoveDownProduct of ProductId
    | MoveDownProductResult of ProductId * Result<unit, ApiError<MoveProductError>>
    | DeleteProduct of ProductId
    | DeleteProductResult of ProductId * Result<unit, ApiError<DeleteProductError>>
    | EditNewProduct
    | FormChanged of Form.View.Model<ProductFormData>
    | SaveProduct of ProductGroupId * ProductData
    | SaveProductResult of Result<ProductId, ApiError<SaveProductError>>
    | CancelEditProduct

let init authKey =
    NotLoaded, Cmd.ofMsg (Load authKey)

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
    | EditProduct (productGroupId, product) ->
        match state with
        | Loaded (authKey, state) ->
            Loaded (authKey, { state with EditingProduct = Some { Id = Some product.Id; Data = Form.View.idle (ProductFormData.fromProductData productGroupId product.Data) } }),
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
    | SaveProduct (productGroupId, product) ->
        match state with
        | Loaded (authKey, ({ EditingProduct = Some editingProduct } as state)) ->
            let state = Loaded (authKey, { state with EditingProduct = Some { editingProduct with Data = { editingProduct.Data with State = Form.View.State.Loading } } })
            let cmd =
                match editingProduct.Id with
                | Some productId -> Cmd.OfAsync.perform (updateProduct authKey productId) product (Result.map (fun () -> productId) >> SaveProductResult)
                | None -> Cmd.OfAsync.perform (createProduct authKey) (productGroupId, product) SaveProductResult
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
                | UnexpectedError _ -> "Fehler beim Speichern des Artikels."
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
        View.infoNotification "Keine Artikel vorhanden." []
    | Loaded (_, state) ->
        React.fragment [
            setMenuItems [
                Html.button [
                    prop.className "!flex items-center gap-2 btn btn-solid btn-green"
                    prop.onClick (fun _ -> dispatch EditNewProduct)
                    
                    prop.children [
                        Fa.i [ Fa.Solid.Plus ] []
                        Html.span [ prop.text "Neuer Artikel" ]
                    ]
                ]
            ]

            Html.div [
                prop.className "container flex flex-col gap-4"
                prop.children [
                    for (index, group) in List.indexed state.Products do
                        Html.div [
                            prop.className "flex flex-col gap-2"
                            prop.children [
                                Html.div [
                                    prop.className "flex items-center gap-2"
                                    prop.children [
                                        Html.h3 [
                                            prop.className "text-2xl"
                                            prop.text group.Data.Name
                                        ]
                                        Html.button [
                                            prop.className "btn btn-solid btn-green"
                                            let isMoving =
                                                match state.MovingUpProductGroup with
                                                | Some (groupId, MovingProductGroup) when groupId = group.Id -> true
                                                | _ -> false
                                            prop.disabled ((isMoving || index = 0))
                                            prop.onClick (fun _ -> dispatch (MoveUpProductGroup group.Id))
                                            
                                            prop.children [
                                                Fa.i [ Fa.Solid.ArrowUp ] []
                                            ]
                                        ]
                                        Html.button [
                                            prop.className "btn btn-solid btn-green"
                                            let isMoving =
                                                match state.MovingDownProductGroup with
                                                | Some (groupId, MovingProductGroup) when groupId = group.Id -> true
                                                | _ -> false
                                            prop.disabled ((isMoving || index = state.Products.Length - 1))
                                            prop.onClick (fun _ -> dispatch (MoveDownProductGroup group.Id))
                                            
                                            prop.children [
                                                Fa.i [ Fa.Solid.ArrowDown ] []
                                            ]
                                        ]
                                    ]
                                ]
                                Html.table [
                                    prop.className "w-full"
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
                                                let isDeletingOrDeleted =
                                                    match deleteProductState with
                                                    | None -> false
                                                    | Some DeletingProduct -> true
                                                    | Some (DeletedProduct (Ok _)) -> true
                                                    | Some (DeletedProduct (Error _)) -> false
                                                Html.tr [
                                                    prop.classes [
                                                        if isDeleted then "opacity-50"
                                                    ]

                                                    prop.children [
                                                        Html.td product.Data.Name.Value
                                                        Html.td (NonNegativeDecimal.value product.Data.Price |> View.formatPrice)
                                                        Html.td [
                                                            match product.Data.State with
                                                            | Enabled ->
                                                                prop.className "text-musi-green"
                                                                prop.text "✔"
                                                            | Disabled ->
                                                                prop.className "text-musi-red"
                                                                prop.text "✘"
                                                        ]
                                                        Html.td [
                                                            Html.div [
                                                                prop.className "flex gap-2"
                                                                prop.children [
                                                                    Html.button [
                                                                        prop.className "btn btn-solid btn-green"
                                                                        let isMoving =
                                                                            match state.MovingUpProduct with
                                                                            | Some (productId, MovingProduct) when productId = product.Id -> true
                                                                            | _ -> false
                                                                        prop.disabled (isMoving || isDeletingOrDeleted || index = 0)
                                                                        prop.onClick (fun _ -> dispatch (MoveUpProduct product.Id))
                                                                        
                                                                        prop.children [
                                                                            Fa.i [ Fa.Solid.ArrowUp ] []
                                                                        ]
                                                                    ]
                                                                    Html.button [
                                                                        prop.className "btn btn-solid btn-green"
                                                                        let isMoving =
                                                                            match state.MovingDownProduct with
                                                                            | Some (productId, MovingProduct) when productId = product.Id -> true
                                                                            | _ -> false
                                                                        prop.disabled (isMoving || isDeletingOrDeleted || index = group.Products.Length - 1)
                                                                        prop.onClick (fun _ -> dispatch (MoveDownProduct product.Id))
                                                                        
                                                                        prop.children [
                                                                            Fa.i [ Fa.Solid.ArrowDown ] []
                                                                        ]
                                                                    ]
                                                                    Html.button [
                                                                        prop.className "btn btn-solid btn-blue"
                                                                        prop.disabled isDeletingOrDeleted
                                                                        prop.onClick (fun _ -> dispatch (EditProduct (group.Id, product)))
                                                                        
                                                                        prop.children [
                                                                            Fa.i [ Fa.Solid.Edit ] []
                                                                        ]
                                                                    ]
                                                                    Html.button [
                                                                        prop.className "btn btn-solid btn-red"
                                                                        prop.disabled isDeletingOrDeleted

                                                                        match deleteProductState with
                                                                        | None
                                                                        | Some (DeletedProduct (Error _)) -> prop.onClick (fun _ -> dispatch (DeleteProduct product.Id))
                                                                        | Some DeletingProduct
                                                                        | Some (DeletedProduct (Ok _)) -> ()
                                                                        
                                                                        prop.children [
                                                                            Fa.i [ Fa.Solid.TrashAlt ] []
                                                                        ]
                                                                    ]
                                                                ]
                                                            ]
                                                            match deleteProductState with
                                                            | Some (DeletedProduct (Error _)) ->
                                                                Html.span [
                                                                    prop.className "text-sm text-musi-red"
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
                        ]
                ]
            ]

            match state.EditingProduct with
            | Some editingProduct ->
                let form : Form.Form<ProductFormData, Msg> =
                    let productGroupField =
                        Form.selectField
                            {
                                Parser = ProductGroupId >> Ok
                                Value = fun values -> values.ProductGroupId
                                Update = fun newValue values -> { values with ProductGroupId = newValue }
                                Error = fun _ -> None
                                Attributes =
                                    {
                                        Label = "Artikelgruppe"
                                        Placeholder = "Wähle eine Artikelgruppe"
                                        Options =
                                            state.Products
                                            |> List.map (fun productGroup -> (let (ProductGroupId v) = productGroup.Id in v, productGroup.Data.Name))
                                    }
                            }
                        |> Form.disable (Option.isSome editingProduct.Id)

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
                                    // dirty hack to change HTML attributes
                                    (
                                        {
                                            Label = "Preis [€]"
                                            Placeholder = ""
                                        } : Field.TextField.Attributes
                                    )
                                    |> fun v -> !!{| v with Type = "number"; Min = 0; Step = 0.1 |}
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

                    let onSubmit = fun productGroupId name price state ->
                        SaveProduct (productGroupId, { Name = name; Price = price; State = state })

                    Form.succeed onSubmit
                    |> Form.append productGroupField
                    |> Form.append nameField
                    |> Form.append priceField
                    |> Form.append stateField

                let title =
                    match editingProduct.Id with
                    | Some _ -> "Artikel bearbeiten"
                    | None -> "Artikel anlegen"

                View.form title form editingProduct.Data dispatch CancelEditProduct FormChanged
            | None -> ()
        ]
