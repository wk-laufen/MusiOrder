namespace MusiOrder.Server.HttpHandler

open Microsoft.Extensions.DependencyInjection
open FSharp.Control.Tasks.V2.ContextInsensitive
open Giraffe
open MusiOrder.Core
open MusiOrder.Models

module Order =
    open MusiOrder.Core.Order
    open MusiOrder.Models.Order
    open AuthHandler

    let handleGetProducts : HttpHandler = fun next ctx -> task {
        let! data = getProductGroups ()
        return! Successful.OK data next ctx
    }

    let saveOrder userId : HttpHandler = fun next ctx -> task {
        let! data = ctx.BindModelAsync<NewOrder>()
        match! saveOrder data userId with
        | Ok () -> return! Successful.OK () next ctx
        | Error errors -> return! RequestErrors.BAD_REQUEST errors next ctx
    }

    let handlePostOrder : HttpHandler = fun next ctx -> task {
        let authHandler = ctx.RequestServices.GetRequiredService<IAuthHandler>()
        let authKey = ctx.TryGetQueryStringValue "authKey"
        let! authUser = authKey |> Option.bindTask (AuthKey >> User.getByAuthKey)
        let orderUserId = ctx.TryGetQueryStringValue "userId" |> Option.map UserId
        match authHandler.CommitOrder authUser orderUserId with
        | AllowCommitOrder userId -> return! saveOrder userId next ctx
        | DenyCommitOrderNotAuthorized when Option.isNone authKey -> return! RequestErrors.BAD_REQUEST [ AddOrderError.NotAuthorized ] next ctx
        | DenyCommitOrderNotAuthorized -> return! RequestErrors.BAD_REQUEST [ AddOrderError.InvalidAuthKey ] next ctx
        | DenyCommitOrderNoOrderUser -> return! RequestErrors.BAD_REQUEST [ AddOrderError.NoOrderUser ] next ctx
    }

    let loadOrderSummary (user: OrderSummaryUser) : HttpHandler = fun next ctx -> task {
        let! balance = User.getBalance user.Id
        let! latestOrders = getLatestOrdersFromUser user.Id
        let result =
            {
                ClientFullName = user.Name
                Balance = balance
                LatestOrders = latestOrders
            }
        return! Successful.OK result next ctx
    }

    let handleGetOrderSummary : HttpHandler = fun next ctx -> task {
        let authHandler = ctx.RequestServices.GetRequiredService<IAuthHandler>()
        let authKey = ctx.TryGetQueryStringValue "authKey"
        let! authUser = authKey |> Option.bindTask (AuthKey >> User.getByAuthKey)
        let! orderUser = ctx.TryGetQueryStringValue "userId" |> Option.bindTask (UserId >> User.getById)
        match authHandler.GetOrderSummary authUser orderUser with
        | GetOrderSummaryAllowed user -> return! loadOrderSummary user next ctx
        | GetOrderSummaryNotAuthorized when Option.isNone authKey -> return! RequestErrors.BAD_REQUEST LoadOrderSummaryError.NotAuthorized next ctx
        | GetOrderSummaryNotAuthorized -> return! RequestErrors.BAD_REQUEST LoadOrderSummaryError.InvalidAuthKey next ctx
        | GetOrderSummaryNoUser -> return! RequestErrors.BAD_REQUEST LoadOrderSummaryError.NoOrderSummaryUser next ctx
    }

    let handleGetUsers : HttpHandler = fun next ctx -> task {
        let authHandler = ctx.RequestServices.GetRequiredService<IAuthHandler>()
        let authKey = ctx.TryGetQueryStringValue "authKey"
        let! authUser = authKey |> Option.bindTask (AuthKey >> User.getByAuthKey)
        match authHandler.GetUsers authUser with
        | GetUsersAllowed ->
            let! result = getUserInfo ()
            return! Successful.OK result next ctx
        | GetUsersNotAuthorized -> return! RequestErrors.BAD_REQUEST LoadUsersError.NotAuthorized next ctx
    }

module UserPaymentAdministration =
    open MusiOrder.Core.UserPaymentAdministration
    open MusiOrder.Models.UserPaymentAdministration

    let handleGetUsers : HttpHandler = fun next ctx -> task {
        match! ctx.TryGetQueryStringValue "authKey" |> Option.bindTask (AuthKey >> User.getByAuthKey) with
        | Some user when User.isAdmin user ->
            let! result = getUserInfo ()
            return! Successful.OK result next ctx
        | Some _ -> return! RequestErrors.BAD_REQUEST LoadUsersError.NotAuthorized next ctx
        | None -> return! RequestErrors.BAD_REQUEST LoadUsersError.InvalidAuthKey next ctx
    }

    let handlePostPayment userId : HttpHandler = fun next ctx -> task {
        match! ctx.TryGetQueryStringValue "authKey" |> Option.bindTask (AuthKey >> User.getByAuthKey) with
        | Some user when User.isAdmin user ->
            let! data = ctx.BindModelAsync<Payment>()
            do! savePayment userId data.Amount
            let! balance = User.getBalance userId
            return! Successful.OK balance next ctx
        | Some _ -> return! RequestErrors.BAD_REQUEST AddPaymentError.InvalidAuthKey next ctx
        | None -> return! RequestErrors.BAD_REQUEST AddPaymentError.NotAuthorized next ctx
    }

module UserAdministration =
    open MusiOrder.Core.UserAdministration
    open MusiOrder.Models.UserAdministration

    let handleGetUsers : HttpHandler = fun next ctx -> task {
        match! ctx.TryGetQueryStringValue "authKey" |> Option.bindTask (AuthKey >> User.getByAuthKey) with
        | Some user when User.isAdmin user ->
            let! result = getExistingUsers ()
            return! Successful.OK result next ctx
        | Some _ -> return! RequestErrors.BAD_REQUEST LoadExistingUsersError.NotAuthorized next ctx
        | None -> return! RequestErrors.BAD_REQUEST LoadExistingUsersError.InvalidAuthKey next ctx
    }

    let handlePostUser : HttpHandler = fun next ctx -> task {
        match! ctx.TryGetQueryStringValue "authKey" |> Option.bindTask (AuthKey >> User.getByAuthKey) with
        | Some user when User.isAdmin user ->
            let! data = ctx.BindModelAsync<ExistingUserData>()
            match! createUser data with
            | Ok newUserId -> return! Successful.OK newUserId next ctx
            | Error e -> return! RequestErrors.BAD_REQUEST e next ctx
        | Some _ -> return! RequestErrors.BAD_REQUEST SaveUserError.NotAuthorized next ctx
        | None -> return! RequestErrors.BAD_REQUEST SaveUserError.InvalidAuthKey next ctx
    }

    let handlePutUser userId : HttpHandler = fun next ctx -> task {
        match! ctx.TryGetQueryStringValue "authKey" |> Option.bindTask (AuthKey >> User.getByAuthKey) with
        | Some user when User.isAdmin user ->
            let! data = ctx.BindModelAsync<PatchUserData>()
            if user.Id = userId && (data.Role <> None && data.Role <> Some Admin) then
                return! RequestErrors.BAD_REQUEST DowngradeSelfNotAllowed next ctx
            elif user.Id = userId && (data.SetAuthKey && Option.isNone data.AuthKey) then
                return! RequestErrors.BAD_REQUEST RemoveKeyCodeNotAllowed next ctx
            else
                match! updateUser userId data with
                | Ok () -> return! Successful.OK () next ctx
                | Error e -> return! RequestErrors.BAD_REQUEST e next ctx
        | Some _ -> return! RequestErrors.BAD_REQUEST SaveUserError.NotAuthorized next ctx
        | None -> return! RequestErrors.BAD_REQUEST SaveUserError.InvalidAuthKey next ctx
    }

    let handleDeleteUser userId : HttpHandler = fun next ctx -> task {
        match! ctx.TryGetQueryStringValue "authKey" |> Option.bindTask (AuthKey >> User.getByAuthKey) with
        | Some user when User.isAdmin user ->
            match ctx.TryGetQueryStringValue "force" with
            | Some _ ->
                do! deleteUser userId
                return! Successful.OK () next ctx
            | None ->
                let! userAuthKey = task {
                    let! user = User.getById userId
                    return user |> Option.bind (fun v -> v.AuthKey)
                }
                let! balance = User.getBalance userId
                let result = [
                    if Option.isSome userAuthKey then AuthKeyPresent
                    if balance <> 0.0m then CurrentBalanceNotZero balance
                ]
                return! Successful.OK result next ctx
        | Some _ -> return! RequestErrors.BAD_REQUEST SaveUserError.NotAuthorized next ctx
        | None -> return! RequestErrors.BAD_REQUEST SaveUserError.InvalidAuthKey next ctx
    }

module ProductAdministration =
    open MusiOrder.Core.ProductAdministration
    open MusiOrder.Models.ProductAdministration

    let handleGetProducts : HttpHandler = fun next ctx -> task {
        match! ctx.TryGetQueryStringValue "authKey" |> Option.bindTask (AuthKey >> User.getByAuthKey) with
        | Some user when User.isAdmin user ->
            let! result = getProducts ()
            return! Successful.OK result next ctx
        | Some _ -> return! RequestErrors.BAD_REQUEST LoadExistingProductsError.NotAuthorized next ctx
        | None -> return! RequestErrors.BAD_REQUEST LoadExistingProductsError.InvalidAuthKey next ctx
    }

    let handlePostProductGroup : HttpHandler = fun next ctx -> task {
        match! ctx.TryGetQueryStringValue "authKey" |> Option.bindTask (AuthKey >> User.getByAuthKey) with
        | Some user when User.isAdmin user ->
            let! data = ctx.BindModelAsync<ProductGroupData>()
            let! newProductGroupId = createProductGroup data
            return! Successful.OK newProductGroupId next ctx
        | Some _ -> return! RequestErrors.BAD_REQUEST SaveProductGroupError.NotAuthorized next ctx
        | None -> return! RequestErrors.BAD_REQUEST SaveProductGroupError.InvalidAuthKey next ctx
    }

    let handlePutProductGroup productGroupId : HttpHandler = fun next ctx -> task {
        match! ctx.TryGetQueryStringValue "authKey" |> Option.bindTask (AuthKey >> User.getByAuthKey) with
        | Some user when User.isAdmin user ->
            let! data = ctx.BindModelAsync<ProductGroupData>()
            do! updateProductGroup productGroupId data
            return! Successful.OK () next ctx
        | Some _ -> return! RequestErrors.BAD_REQUEST SaveProductGroupError.NotAuthorized next ctx
        | None -> return! RequestErrors.BAD_REQUEST SaveProductGroupError.InvalidAuthKey next ctx
    }

    let handleMoveUpProductGroup productGroupId : HttpHandler = fun next ctx -> task {
        match! ctx.TryGetQueryStringValue "authKey" |> Option.bindTask (AuthKey >> User.getByAuthKey) with
        | Some user when User.isAdmin user ->
            do! moveUpProductGroup productGroupId
            return! Successful.OK () next ctx
        | Some _ -> return! RequestErrors.BAD_REQUEST MoveProductGroupError.NotAuthorized next ctx
        | None -> return! RequestErrors.BAD_REQUEST MoveProductGroupError.InvalidAuthKey next ctx
    }

    let handleMoveDownProductGroup productGroupId : HttpHandler = fun next ctx -> task {
        match! ctx.TryGetQueryStringValue "authKey" |> Option.bindTask (AuthKey >> User.getByAuthKey) with
        | Some user when User.isAdmin user ->
            do! moveDownProductGroup productGroupId
            return! Successful.OK () next ctx
        | Some _ -> return! RequestErrors.BAD_REQUEST MoveProductGroupError.NotAuthorized next ctx
        | None -> return! RequestErrors.BAD_REQUEST MoveProductGroupError.InvalidAuthKey next ctx
    }

    let handleDeleteProductGroup productGroupId : HttpHandler = fun next ctx -> task {
        match! ctx.TryGetQueryStringValue "authKey" |> Option.bindTask (AuthKey >> User.getByAuthKey) with
        | Some user when User.isAdmin user ->
            let! isDeleted = deleteProductGroup productGroupId
            if isDeleted then return! Successful.OK () next ctx
            else return! RequestErrors.BAD_REQUEST DeleteProductGroupError.GroupNotEmpty next ctx
        | Some _ -> return! RequestErrors.BAD_REQUEST DeleteProductGroupError.NotAuthorized next ctx
        | None -> return! RequestErrors.BAD_REQUEST DeleteProductGroupError.InvalidAuthKey next ctx
    }

    let handlePostProduct productGroupId : HttpHandler = fun next ctx -> task {
        match! ctx.TryGetQueryStringValue "authKey" |> Option.bindTask (AuthKey >> User.getByAuthKey) with
        | Some user when User.isAdmin user ->
            let! data = ctx.BindModelAsync<ProductData>()
            let! newProductId = createProduct productGroupId data
            return! Successful.OK newProductId next ctx
        | Some _ -> return! RequestErrors.BAD_REQUEST SaveProductError.NotAuthorized next ctx
        | None -> return! RequestErrors.BAD_REQUEST SaveProductError.InvalidAuthKey next ctx
    }

    let handlePutProduct productId : HttpHandler = fun next ctx -> task {
        match! ctx.TryGetQueryStringValue "authKey" |> Option.bindTask (AuthKey >> User.getByAuthKey) with
        | Some user when User.isAdmin user ->
            let! data = ctx.BindModelAsync<ProductData>()
            do! updateProduct productId data
            return! Successful.OK () next ctx
        | Some _ -> return! RequestErrors.BAD_REQUEST SaveProductError.NotAuthorized next ctx
        | None -> return! RequestErrors.BAD_REQUEST SaveProductError.InvalidAuthKey next ctx
    }

    let handleMoveUpProduct productId : HttpHandler = fun next ctx -> task {
        match! ctx.TryGetQueryStringValue "authKey" |> Option.bindTask (AuthKey >> User.getByAuthKey) with
        | Some user when User.isAdmin user ->
            do! moveUpProduct productId
            return! Successful.OK () next ctx
        | Some _ -> return! RequestErrors.BAD_REQUEST MoveProductError.NotAuthorized next ctx
        | None -> return! RequestErrors.BAD_REQUEST MoveProductError.InvalidAuthKey next ctx
    }

    let handleMoveDownProduct productId : HttpHandler = fun next ctx -> task {
        match! ctx.TryGetQueryStringValue "authKey" |> Option.bindTask (AuthKey >> User.getByAuthKey) with
        | Some user when User.isAdmin user ->
            do! moveDownProduct productId
            return! Successful.OK () next ctx
        | Some _ -> return! RequestErrors.BAD_REQUEST MoveProductError.NotAuthorized next ctx
        | None -> return! RequestErrors.BAD_REQUEST MoveProductError.InvalidAuthKey next ctx
    }

    let handleDeleteProduct productId : HttpHandler = fun next ctx -> task {
        match! ctx.TryGetQueryStringValue "authKey" |> Option.bindTask (AuthKey >> User.getByAuthKey) with
        | Some user when User.isAdmin user ->
            do! deleteProduct productId
            return! Successful.OK () next ctx
        | Some _ -> return! RequestErrors.BAD_REQUEST DeleteProductError.NotAuthorized next ctx
        | None -> return! RequestErrors.BAD_REQUEST DeleteProductError.InvalidAuthKey next ctx
    }

module OrderAdministration =
    open MusiOrder.Core.OrderAdministration
    open MusiOrder.Models.OrderAdministration

    let handleGetOrders : HttpHandler = fun next ctx -> task {
        match! ctx.TryGetQueryStringValue "authKey" |> Option.bindTask (AuthKey >> User.getByAuthKey) with
        | Some user when User.isAdmin user ->
            let! result = getOrders ()
            return! Successful.OK result next ctx
        | Some _ -> return! RequestErrors.BAD_REQUEST LoadOrderInfoError.NotAuthorized next ctx
        | None -> return! RequestErrors.BAD_REQUEST LoadOrderInfoError.InvalidAuthKey next ctx
    }

    let handleDeleteOrder orderId : HttpHandler = fun next ctx -> task {
        match! ctx.TryGetQueryStringValue "authKey" |> Option.bindTask (AuthKey >> User.getByAuthKey) with
        | Some user when User.isAdmin user ->
            do! deleteOrder orderId
            return! Successful.OK () next ctx
        | Some _ -> return! RequestErrors.BAD_REQUEST DeleteOrderError.NotAuthorized next ctx
        | None -> return! RequestErrors.BAD_REQUEST DeleteOrderError.InvalidAuthKey next ctx
    }

module DataExport =
    open MusiOrder.Models.DataExport
    open System.IO

    let handleExportDatabase : HttpHandler = fun next ctx -> task {
        match! ctx.TryGetQueryStringValue "authKey" |> Option.bindTask (AuthKey >> User.getByAuthKey) with
        | Some user when User.isAdmin user ->
            use stream = File.OpenRead DB.dbPath
            return! Successful.ok (streamData false stream None None) next ctx
        | Some _ -> return! RequestErrors.BAD_REQUEST ExportDatabaseError.NotAuthorized next ctx
        | None -> return! RequestErrors.BAD_REQUEST ExportDatabaseError.InvalidAuthKey next ctx
    }
