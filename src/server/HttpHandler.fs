namespace MusiOrder.Server.HttpHandler

open System
open Microsoft.Extensions.DependencyInjection
open FSharp.Control.Tasks.V2.ContextInsensitive
open Giraffe
open MusiOrder.Core
open MusiOrder.Models

module Order =
    open MusiOrder.Core.Order
    open MusiOrder.Models.Order
    open AuthHandler

    [<CLIMutable>]
    type NewOrderEntryDto = { ProductId: string; Amount: int }

    type OrderProductDto = {
        Id: string
        Name: string
        Price: decimal
    }

    type OrderProductGroupDto = {
        Name: string
        Products: OrderProductDto[]
    }

    type HistoricOrderDto = {
        Timestamp: DateTimeOffset
        ProductName: string
        Amount: int
    }

    type OrderSummaryDto = {
        ClientFullName: string
        Balance: decimal
        LatestOrders: HistoricOrderDto[]
    }

    type OrderUserDto = {
        Id: string
        FirstName: string
        LastName: string
        Balance: decimal
    }

    type OrderUserListDto = {
        Self: OrderUserDto option
        Others: OrderUserDto[]
    }

    module private UserIdDto =
        let fromDomain (UserId s) = s

    module private ProductIdDto =
        let fromDomain (ProductId s) = s

    let handleGetProducts: HttpHandler =
        fun next ctx ->
            task {
                let! data = getProductGroups ()

                let dto =
                    data
                    |> List.map (fun g ->
                        let products =
                            g.Products
                            |> List.map (fun p -> {
                                Id = ProductIdDto.fromDomain p.Id
                                Name = p.Name
                                Price = p.Price
                            })
                            |> List.toArray

                        { Name = g.Name; Products = products })
                    |> List.toArray

                return! Successful.OK dto next ctx
            }

    let saveOrder userId : HttpHandler =
        fun next ctx ->
            task {
                let! entries = ctx.BindModelAsync<NewOrderEntryDto[]>()

                let data =
                    entries
                    |> Array.choose (fun e ->
                        PositiveInteger.tryCreate e.Amount
                        |> Option.map (fun amount ->
                            ({
                                ProductId = ProductId e.ProductId
                                Amount = amount
                            }
                            : NewOrderEntry)))
                    |> Array.toList

                match! saveOrder data userId with
                | Ok() -> return! Successful.OK () next ctx
                | Error errors ->
                    let dto =
                        errors
                        |> List.map (function
                            | AddOrderError.InvalidAuthKey -> box "InvalidAuthKey"
                            | AddOrderError.NotAuthorized -> box "NotAuthorized"
                            | AddOrderError.NoOrderUser -> box "NoOrderUser"
                            | AddOrderError.OrderEntryErrors(ProductId productId, _) ->
                                box [| box "OrderEntryErrors"; box productId |])
                        |> List.toArray

                    return! RequestErrors.BAD_REQUEST dto next ctx
            }

    let handlePostOrder: HttpHandler =
        fun next ctx ->
            task {
                let authHandler = ctx.RequestServices.GetRequiredService<IAuthHandler>()
                let authKey = ctx.TryGetQueryStringValue "authKey" |> Option.bind AuthKey.tryParse
                let! authUser = authKey |> Option.bindTask User.getByAuthKey
                let orderUserId = ctx.TryGetQueryStringValue "userId" |> Option.map UserId

                match authHandler.CommitOrder authUser orderUserId with
                | AllowCommitOrder userId -> return! saveOrder userId next ctx
                | DenyCommitOrderNotAuthorized when Option.isNone authKey ->
                    return! RequestErrors.BAD_REQUEST [| "NotAuthorized" |] next ctx
                | DenyCommitOrderNotAuthorized -> return! RequestErrors.BAD_REQUEST [| "InvalidAuthKey" |] next ctx
                | DenyCommitOrderNoOrderUser -> return! RequestErrors.BAD_REQUEST [| "NoOrderUser" |] next ctx
            }

    let loadOrderSummary (user: OrderSummaryUser) : HttpHandler =
        fun next ctx ->
            task {
                let! balance = User.getBalance user.Id
                let! latestOrders = getLatestOrdersFromUser user.Id

                let dto = {
                    ClientFullName = user.Name
                    Balance = balance
                    LatestOrders =
                        latestOrders
                        |> List.map (fun o -> {
                            Timestamp = o.Timestamp
                            ProductName = o.ProductName
                            Amount = o.Amount
                        })
                        |> List.toArray
                }

                return! Successful.OK dto next ctx
            }

    let handleGetOrderSummary: HttpHandler =
        fun next ctx ->
            task {
                let authHandler = ctx.RequestServices.GetRequiredService<IAuthHandler>()
                let authKey = ctx.TryGetQueryStringValue "authKey" |> Option.bind AuthKey.tryParse
                let! authUser = authKey |> Option.bindTask User.getByAuthKey
                let! orderUser = ctx.TryGetQueryStringValue "userId" |> Option.bindTask (UserId >> User.getById)

                match authHandler.GetOrderSummary authUser orderUser with
                | GetOrderSummaryAllowed user -> return! loadOrderSummary user next ctx
                | GetOrderSummaryNotAuthorized when Option.isNone authKey ->
                    return! RequestErrors.BAD_REQUEST "NotAuthorized" next ctx
                | GetOrderSummaryNotAuthorized -> return! RequestErrors.BAD_REQUEST "InvalidAuthKey" next ctx
                | GetOrderSummaryNoUser -> return! RequestErrors.BAD_REQUEST "NoOrderSummaryUser" next ctx
            }

    let handleGetUsers: HttpHandler =
        fun next ctx ->
            task {
                let authHandler = ctx.RequestServices.GetRequiredService<IAuthHandler>()
                let authKey = ctx.TryGetQueryStringValue "authKey" |> Option.bind AuthKey.tryParse
                let! authUser = authKey |> Option.bindTask User.getByAuthKey

                match authHandler.GetUsers authUser with
                | GetUsersAllowed ->
                    let! users = getUserInfo ()

                    let mapUser (u: UserInfo) = {
                        Id = UserIdDto.fromDomain u.Id
                        FirstName = u.FirstName
                        LastName = u.LastName
                        Balance = u.Balance
                    }

                    let selfUser =
                        authUser |> Option.bind (fun v -> users |> List.tryFind (fun u -> u.Id = v.Id))

                    let dto = {
                        Self = selfUser |> Option.map mapUser
                        Others =
                            match selfUser with
                            | Some self -> users |> List.except [ self ] |> List.map mapUser |> List.toArray
                            | None -> users |> List.map mapUser |> List.toArray
                    }

                    return! Successful.OK dto next ctx
                | GetUsersNotAuthorized -> return! RequestErrors.BAD_REQUEST "NotAuthorized" next ctx
            }

module UserPaymentAdministration =
    open MusiOrder.Core.UserPaymentAdministration
    open MusiOrder.Models.UserPaymentAdministration

    [<CLIMutable>]
    type PaymentDto = { Amount: decimal }

    type UserPaymentInfoDto = {
        Id: string
        FirstName: string
        LastName: string
        LatestOrderTimestamp: DateTimeOffset option
        Balance: decimal
        SuggestedBalanceChanges: decimal[]
    }

    module private UserIdDto =
        let fromDomain (UserId s) = s

    let handleGetUsers: HttpHandler =
        fun next ctx ->
            task {
                match!
                    ctx.TryGetQueryStringValue "authKey"
                    |> Option.bind AuthKey.tryParse
                    |> Option.bindTask User.getByAuthKey
                with
                | Some user when User.isAdmin user ->
                    let! result = getUserInfo ()

                    let dto =
                        result
                        |> List.map (fun u -> {
                            Id = UserIdDto.fromDomain u.Id
                            FirstName = u.FirstName
                            LastName = u.LastName
                            LatestOrderTimestamp = u.LatestOrderTimestamp
                            Balance = u.Balance
                            SuggestedBalanceChanges = u.SuggestedBalanceChanges |> List.toArray
                        })
                        |> List.toArray

                    return! Successful.OK dto next ctx
                | Some _ -> return! RequestErrors.BAD_REQUEST "NotAuthorized" next ctx
                | None -> return! RequestErrors.BAD_REQUEST "InvalidAuthKey" next ctx
            }

    let handlePostPayment userId : HttpHandler =
        fun next ctx ->
            task {
                match!
                    ctx.TryGetQueryStringValue "authKey"
                    |> Option.bind AuthKey.tryParse
                    |> Option.bindTask User.getByAuthKey
                with
                | Some user when User.isAdmin user ->
                    let! dto = ctx.BindModelAsync<PaymentDto>()
                    do! savePayment userId dto.Amount
                    let! balance = User.getBalance userId
                    return! Successful.OK balance next ctx
                | Some _ -> return! RequestErrors.BAD_REQUEST "NotAuthorized" next ctx
                | None -> return! RequestErrors.BAD_REQUEST "InvalidAuthKey" next ctx
            }

module UserAdministration =
    open MusiOrder.Core.UserAdministration
    open MusiOrder.Models.UserAdministration

    [<CLIMutable>]
    type AuthKeyDto = { KeyType: string; KeyCode: string }

    module AuthKeyDto =
        let toDomain (dto: AuthKeyDto) =
            if dto.KeyType.Equals("nfc", StringComparison.InvariantCultureIgnoreCase) then
                Some(NFCAuthKey dto.KeyCode)
            else
                None

        let fromDomain =
            function
            | NFCAuthKey keyCode -> { KeyType = "nfc"; KeyCode = keyCode }

    [<CLIMutable>]
    type ExistingUserDataDto = {
        FirstName: string
        LastName: string
        AuthKeys: AuthKeyDto[]
        Role: string
    }

    module ExistingUserDataDto =
        let toDomain (dto: ExistingUserDataDto) : ExistingUserData =
            let authKeys = if isNull (dto.AuthKeys :> obj) then [||] else dto.AuthKeys

            {
                FirstName =
                    dto.FirstName
                    |> NotEmptyString.tryCreate
                    |> Option.defaultWith (fun () -> failwith "FirstName required")
                LastName =
                    dto.LastName
                    |> NotEmptyString.tryCreate
                    |> Option.defaultWith (fun () -> failwith "LastName required")
                AuthKeys = authKeys |> Array.choose AuthKeyDto.toDomain |> Array.toList
                Role =
                    dto.Role
                    |> UserRole.tryParse
                    |> Option.defaultWith (fun () -> failwith "Invalid role")
            }

    [<CLIMutable>]
    type PatchUserDataDto = {
        FirstName: string
        LastName: string
        AddAuthKeys: AuthKeyDto[]
        RemoveAuthKeys: AuthKeyDto[]
        Role: string
    }

    module PatchUserDataDto =
        let toDomain (dto: PatchUserDataDto) : PatchUserData =
            let addKeys =
                if isNull (dto.AddAuthKeys :> obj) then
                    [||]
                else
                    dto.AddAuthKeys

            let removeKeys =
                if isNull (dto.RemoveAuthKeys :> obj) then
                    [||]
                else
                    dto.RemoveAuthKeys

            {
                FirstName = dto.FirstName |> Option.ofObj |> Option.bind NotEmptyString.tryCreate
                LastName = dto.LastName |> Option.ofObj |> Option.bind NotEmptyString.tryCreate
                AddAuthKeys = addKeys |> Array.choose AuthKeyDto.toDomain |> Array.toList
                RemoveAuthKeys = removeKeys |> Array.choose AuthKeyDto.toDomain |> Array.toList
                Role = dto.Role |> Option.ofObj |> Option.bind UserRole.tryParse
            }

    type ExistingUserDto = {
        Id: string
        Data: ExistingUserDataDto
    }

    module private UserIdDto =
        let fromDomain (UserId s) = s

    module private UserRoleDto =
        let fromDomain =
            function
            | Admin -> "Admin"
            | OrderAssistant -> "OrderAssistant"
            | User -> "User"

    let handleGetUsers: HttpHandler =
        fun next ctx ->
            task {
                match!
                    ctx.TryGetQueryStringValue "authKey"
                    |> Option.bind AuthKey.tryParse
                    |> Option.bindTask User.getByAuthKey
                with
                | Some user when User.isAdmin user ->
                    let! result = getExistingUsers ()

                    let dto =
                        result
                        |> List.map (fun u -> {
                            Id = UserIdDto.fromDomain u.Id
                            Data = {
                                FirstName = u.Data.FirstName.Value
                                LastName = u.Data.LastName.Value
                                AuthKeys = u.Data.AuthKeys |> List.map AuthKeyDto.fromDomain |> List.toArray
                                Role = UserRoleDto.fromDomain u.Data.Role
                            }
                        })
                        |> List.toArray

                    return! Successful.OK dto next ctx
                | Some _ -> return! RequestErrors.BAD_REQUEST "NotAuthorized" next ctx
                | None -> return! RequestErrors.BAD_REQUEST "InvalidAuthKey" next ctx
            }

    let handlePostUser: HttpHandler =
        fun next ctx ->
            task {
                match!
                    ctx.TryGetQueryStringValue "authKey"
                    |> Option.bind AuthKey.tryParse
                    |> Option.bindTask User.getByAuthKey
                with
                | Some user when User.isAdmin user ->
                    let! dto = ctx.BindModelAsync<ExistingUserDataDto>()

                    match! createUser (ExistingUserDataDto.toDomain dto) with
                    | Ok newUserId -> return! Successful.OK (UserIdDto.fromDomain newUserId) next ctx
                    | Error(KeyCodeTaken names) ->
                        return! RequestErrors.BAD_REQUEST [| box "KeyCodeTaken"; box (List.toArray names) |] next ctx
                    | Error e -> return! RequestErrors.BAD_REQUEST (string e) next ctx
                | Some _ -> return! RequestErrors.BAD_REQUEST "NotAuthorized" next ctx
                | None -> return! RequestErrors.BAD_REQUEST "InvalidAuthKey" next ctx
            }

    let handlePutUser userId : HttpHandler =
        fun next ctx ->
            task {
                match ctx.TryGetQueryStringValue "authKey" |> Option.bind AuthKey.tryParse with
                | Some authKey ->
                    match! User.getByAuthKey authKey with
                    | Some user when User.isAdmin user ->
                        let! dto = ctx.BindModelAsync<PatchUserDataDto>()
                        let patchData = PatchUserDataDto.toDomain dto

                        if user.Id = userId && (patchData.Role <> None && patchData.Role <> Some Admin) then
                            return! RequestErrors.BAD_REQUEST "DowngradeSelfNotAllowed" next ctx
                        elif user.Id = userId && patchData.RemoveAuthKeys |> List.contains authKey then
                            return! RequestErrors.BAD_REQUEST "RemoveActiveAuthKeyNotAllowed" next ctx
                        else
                            match! updateUser userId patchData with
                            | Ok() -> return! Successful.OK () next ctx
                            | Error(KeyCodeTaken names) ->
                                return!
                                    RequestErrors.BAD_REQUEST
                                        [| box "KeyCodeTaken"; box (List.toArray names) |]
                                        next
                                        ctx
                            | Error e -> return! RequestErrors.BAD_REQUEST (string e) next ctx
                    | Some _ -> return! RequestErrors.BAD_REQUEST "NotAuthorized" next ctx
                    | None -> return! RequestErrors.BAD_REQUEST "InvalidAuthKey" next ctx
                | None -> return! RequestErrors.BAD_REQUEST "InvalidAuthKey" next ctx
            }

    let handleDeleteUser userId : HttpHandler =
        fun next ctx ->
            task {
                match!
                    ctx.TryGetQueryStringValue "authKey"
                    |> Option.bind AuthKey.tryParse
                    |> Option.bindTask User.getByAuthKey
                with
                | Some user when User.isAdmin user ->
                    match ctx.TryGetQueryStringValue "force" with
                    | Some _ ->
                        do! deleteUser userId
                        return! Successful.OK () next ctx
                    | None ->
                        let! userAuthKeys =
                            task {
                                let! user = User.getById userId
                                return user |> Option.map (fun v -> v.AuthKeys) |> Option.defaultValue []
                            }

                        let! balance = User.getBalance userId

                        let result = [
                            if not <| List.isEmpty userAuthKeys then
                                AuthKeyPresent
                            if balance <> 0.0m then
                                CurrentBalanceNotZero balance
                        ]

                        let mapWarning =
                            function
                            | AuthKeyPresent -> box "AuthKeyPresent"
                            | CurrentBalanceNotZero b -> box [| box "CurrentBalanceNotZero"; box b |]

                        let dto = result |> List.map mapWarning |> List.toArray
                        return! Successful.OK dto next ctx
                | Some _ -> return! RequestErrors.BAD_REQUEST "NotAuthorized" next ctx
                | None -> return! RequestErrors.BAD_REQUEST "InvalidAuthKey" next ctx
            }

module ProductAdministration =
    open MusiOrder.Core.ProductAdministration
    open MusiOrder.Models.ProductAdministration

    [<CLIMutable>]
    type ProductGroupDataDto = { Name: string }

    module ProductGroupDataDto =
        let toDomain (dto: ProductGroupDataDto) : ProductGroupData = {
            Name =
                dto.Name
                |> NotEmptyString.tryCreate
                |> Option.defaultWith (fun () -> failwith "Name required")
        }

    [<CLIMutable>]
    type ProductDataDto = {
        Name: string
        Price: decimal
        State: string
    }

    module ProductDataDto =
        let toDomain (dto: ProductDataDto) : ProductData = {
            Name =
                dto.Name
                |> NotEmptyString.tryCreate
                |> Option.defaultWith (fun () -> failwith "Name required")
            Price =
                dto.Price
                |> NonNegativeDecimal.tryCreate
                |> Option.defaultWith (fun () -> failwith "Invalid price")
            State =
                dto.State.ToLowerInvariant()
                |> ProductState.tryParse
                |> Option.defaultWith (fun () -> failwith "Invalid state")
        }

    type ExistingProductDataDto = {
        Name: string
        Price: decimal
        State: string
    }

    type ExistingProductDto = {
        Id: string
        Data: ExistingProductDataDto
    }

    type ExistingProductGroupDto = {
        Id: string
        Data: ProductGroupDataDto
        Products: ExistingProductDto[]
    }

    module private ProductIdDto =
        let fromDomain (ProductId s) = s

    module private ProductGroupIdDto =
        let fromDomain (ProductGroupId s) = s

    let handleGetProducts: HttpHandler =
        fun next ctx ->
            task {
                match!
                    ctx.TryGetQueryStringValue "authKey"
                    |> Option.bind AuthKey.tryParse
                    |> Option.bindTask User.getByAuthKey
                with
                | Some user when User.isAdmin user ->
                    let! result = getProducts ()

                    let dto =
                        result
                        |> List.map (fun g ->
                            let products =
                                g.Products
                                |> List.map (fun p -> {
                                    Id = ProductIdDto.fromDomain p.Id
                                    Data = {
                                        Name = p.Data.Name.Value
                                        Price = NonNegativeDecimal.value p.Data.Price
                                        State = ProductState.toString p.Data.State
                                    }
                                })
                                |> List.toArray

                            {
                                Id = ProductGroupIdDto.fromDomain g.Id
                                Data = { Name = g.Data.Name.Value }
                                Products = products
                            })
                        |> List.toArray

                    return! Successful.OK dto next ctx
                | Some _ -> return! RequestErrors.BAD_REQUEST "NotAuthorized" next ctx
                | None -> return! RequestErrors.BAD_REQUEST "InvalidAuthKey" next ctx
            }

    let handlePostProductGroup: HttpHandler =
        fun next ctx ->
            task {
                match!
                    ctx.TryGetQueryStringValue "authKey"
                    |> Option.bind AuthKey.tryParse
                    |> Option.bindTask User.getByAuthKey
                with
                | Some user when User.isAdmin user ->
                    let! dto = ctx.BindModelAsync<ProductGroupDataDto>()
                    let! newProductGroupId = createProductGroup (ProductGroupDataDto.toDomain dto)
                    return! Successful.OK (ProductGroupIdDto.fromDomain newProductGroupId) next ctx
                | Some _ -> return! RequestErrors.BAD_REQUEST "NotAuthorized" next ctx
                | None -> return! RequestErrors.BAD_REQUEST "InvalidAuthKey" next ctx
            }

    let handlePutProductGroup productGroupId : HttpHandler =
        fun next ctx ->
            task {
                match!
                    ctx.TryGetQueryStringValue "authKey"
                    |> Option.bind AuthKey.tryParse
                    |> Option.bindTask User.getByAuthKey
                with
                | Some user when User.isAdmin user ->
                    let! dto = ctx.BindModelAsync<ProductGroupDataDto>()
                    do! updateProductGroup productGroupId (ProductGroupDataDto.toDomain dto)
                    return! Successful.OK () next ctx
                | Some _ -> return! RequestErrors.BAD_REQUEST "NotAuthorized" next ctx
                | None -> return! RequestErrors.BAD_REQUEST "InvalidAuthKey" next ctx
            }

    let handleMoveUpProductGroup productGroupId : HttpHandler =
        fun next ctx ->
            task {
                match!
                    ctx.TryGetQueryStringValue "authKey"
                    |> Option.bind AuthKey.tryParse
                    |> Option.bindTask User.getByAuthKey
                with
                | Some user when User.isAdmin user ->
                    do! moveUpProductGroup productGroupId
                    return! Successful.OK () next ctx
                | Some _ -> return! RequestErrors.BAD_REQUEST "NotAuthorized" next ctx
                | None -> return! RequestErrors.BAD_REQUEST "InvalidAuthKey" next ctx
            }

    let handleMoveDownProductGroup productGroupId : HttpHandler =
        fun next ctx ->
            task {
                match!
                    ctx.TryGetQueryStringValue "authKey"
                    |> Option.bind AuthKey.tryParse
                    |> Option.bindTask User.getByAuthKey
                with
                | Some user when User.isAdmin user ->
                    do! moveDownProductGroup productGroupId
                    return! Successful.OK () next ctx
                | Some _ -> return! RequestErrors.BAD_REQUEST "NotAuthorized" next ctx
                | None -> return! RequestErrors.BAD_REQUEST "InvalidAuthKey" next ctx
            }

    let handleDeleteProductGroup productGroupId : HttpHandler =
        fun next ctx ->
            task {
                match!
                    ctx.TryGetQueryStringValue "authKey"
                    |> Option.bind AuthKey.tryParse
                    |> Option.bindTask User.getByAuthKey
                with
                | Some user when User.isAdmin user ->
                    let! isDeleted = deleteProductGroup productGroupId

                    if isDeleted then
                        return! Successful.OK () next ctx
                    else
                        return! RequestErrors.BAD_REQUEST "GroupNotEmpty" next ctx
                | Some _ -> return! RequestErrors.BAD_REQUEST "NotAuthorized" next ctx
                | None -> return! RequestErrors.BAD_REQUEST "InvalidAuthKey" next ctx
            }

    let handlePostProduct productGroupId : HttpHandler =
        fun next ctx ->
            task {
                match!
                    ctx.TryGetQueryStringValue "authKey"
                    |> Option.bind AuthKey.tryParse
                    |> Option.bindTask User.getByAuthKey
                with
                | Some user when User.isAdmin user ->
                    let! dto = ctx.BindModelAsync<ProductDataDto>()
                    let! newProductId = createProduct productGroupId (ProductDataDto.toDomain dto)
                    return! Successful.OK (ProductGroupIdDto.fromDomain newProductId) next ctx
                | Some _ -> return! RequestErrors.BAD_REQUEST "NotAuthorized" next ctx
                | None -> return! RequestErrors.BAD_REQUEST "InvalidAuthKey" next ctx
            }

    let handlePutProduct productId : HttpHandler =
        fun next ctx ->
            task {
                match!
                    ctx.TryGetQueryStringValue "authKey"
                    |> Option.bind AuthKey.tryParse
                    |> Option.bindTask User.getByAuthKey
                with
                | Some user when User.isAdmin user ->
                    let! dto = ctx.BindModelAsync<ProductDataDto>()
                    do! updateProduct productId (ProductDataDto.toDomain dto)
                    return! Successful.OK () next ctx
                | Some _ -> return! RequestErrors.BAD_REQUEST "NotAuthorized" next ctx
                | None -> return! RequestErrors.BAD_REQUEST "InvalidAuthKey" next ctx
            }

    let handleMoveUpProduct productId : HttpHandler =
        fun next ctx ->
            task {
                match!
                    ctx.TryGetQueryStringValue "authKey"
                    |> Option.bind AuthKey.tryParse
                    |> Option.bindTask User.getByAuthKey
                with
                | Some user when User.isAdmin user ->
                    do! moveUpProduct productId
                    return! Successful.OK () next ctx
                | Some _ -> return! RequestErrors.BAD_REQUEST "NotAuthorized" next ctx
                | None -> return! RequestErrors.BAD_REQUEST "InvalidAuthKey" next ctx
            }

    let handleMoveDownProduct productId : HttpHandler =
        fun next ctx ->
            task {
                match!
                    ctx.TryGetQueryStringValue "authKey"
                    |> Option.bind AuthKey.tryParse
                    |> Option.bindTask User.getByAuthKey
                with
                | Some user when User.isAdmin user ->
                    do! moveDownProduct productId
                    return! Successful.OK () next ctx
                | Some _ -> return! RequestErrors.BAD_REQUEST "NotAuthorized" next ctx
                | None -> return! RequestErrors.BAD_REQUEST "InvalidAuthKey" next ctx
            }

    let handleDeleteProduct productId : HttpHandler =
        fun next ctx ->
            task {
                match!
                    ctx.TryGetQueryStringValue "authKey"
                    |> Option.bind AuthKey.tryParse
                    |> Option.bindTask User.getByAuthKey
                with
                | Some user when User.isAdmin user ->
                    do! deleteProduct productId
                    return! Successful.OK () next ctx
                | Some _ -> return! RequestErrors.BAD_REQUEST "NotAuthorized" next ctx
                | None -> return! RequestErrors.BAD_REQUEST "InvalidAuthKey" next ctx
            }

module OrderAdministration =
    open MusiOrder.Core.OrderAdministration
    open MusiOrder.Models.OrderAdministration

    type OrderInfoDto = {
        Id: string
        FirstName: string
        LastName: string
        ProductName: string
        Amount: int
        PricePerUnit: decimal
        Timestamp: DateTimeOffset
    }

    module private OrderIdDto =
        let fromDomain (OrderId s) = s

    let handleGetOrders: HttpHandler =
        fun next ctx ->
            task {
                match!
                    ctx.TryGetQueryStringValue "authKey"
                    |> Option.bind AuthKey.tryParse
                    |> Option.bindTask User.getByAuthKey
                with
                | Some user when User.isAdmin user ->
                    let! result = getOrders ()

                    let dto =
                        result
                        |> List.map (fun o -> {
                            Id = OrderIdDto.fromDomain o.Id
                            FirstName = o.FirstName
                            LastName = o.LastName
                            ProductName = o.ProductName
                            Amount = o.Amount
                            PricePerUnit = o.PricePerUnit
                            Timestamp = o.Timestamp
                        })
                        |> List.toArray

                    return! Successful.OK dto next ctx
                | Some _ -> return! RequestErrors.BAD_REQUEST "NotAuthorized" next ctx
                | None -> return! RequestErrors.BAD_REQUEST "InvalidAuthKey" next ctx
            }

    let handleDeleteOrder orderId : HttpHandler =
        fun next ctx ->
            task {
                match!
                    ctx.TryGetQueryStringValue "authKey"
                    |> Option.bind AuthKey.tryParse
                    |> Option.bindTask User.getByAuthKey
                with
                | Some user when User.isAdmin user ->
                    do! deleteOrder orderId
                    return! Successful.OK () next ctx
                | Some _ -> return! RequestErrors.BAD_REQUEST "NotAuthorized" next ctx
                | None -> return! RequestErrors.BAD_REQUEST "InvalidAuthKey" next ctx
            }

module OrderStatistics =
    open MusiOrder.Core.OrderStatistics
    open MusiOrder.Models.OrderStatistics

    type OrderStatisticsDto = {
        FirstName: string
        LastName: string
        ProductName: string
        Amount: int
        PricePerUnit: decimal
        Timestamp: DateTimeOffset
    }

    let handleGetOrders: HttpHandler =
        fun next ctx ->
            task {
                match!
                    ctx.TryGetQueryStringValue "authKey"
                    |> Option.bind AuthKey.tryParse
                    |> Option.bindTask User.getByAuthKey
                with
                | Some user when User.isAdmin user ->
                    let startTime =
                        ctx.TryGetQueryStringValue "startTime" |> Option.bind DateTime.tryParseDate

                    let endTime =
                        ctx.TryGetQueryStringValue "endTime" |> Option.bind DateTime.tryParseDate

                    match startTime, endTime with
                    | Some startTime, Some endTime ->
                        let! result = getOrders startTime endTime

                        let dto =
                            result
                            |> List.map (fun o -> {
                                FirstName = o.FirstName
                                LastName = o.LastName
                                ProductName = o.ProductName
                                Amount = o.Amount
                                PricePerUnit = o.PricePerUnit
                                Timestamp = o.Timestamp
                            })
                            |> List.toArray

                        return! Successful.OK dto next ctx
                    | _ -> return! RequestErrors.BAD_REQUEST "MissingTimeRange" next ctx
                | Some _ -> return! RequestErrors.BAD_REQUEST "NotAuthorized" next ctx
                | None -> return! RequestErrors.BAD_REQUEST "InvalidAuthKey" next ctx
            }

module DataExport =
    open MusiOrder.Models.DataExport
    open System.IO

    let handleExportDatabase: HttpHandler =
        fun next ctx ->
            task {
                match!
                    ctx.TryGetQueryStringValue "authKey"
                    |> Option.bind AuthKey.tryParse
                    |> Option.bindTask User.getByAuthKey
                with
                | Some user when User.isAdmin user ->
                    use stream = File.OpenRead DB.dbPath
                    return! Successful.ok (streamData false stream None None) next ctx
                | Some _ -> return! RequestErrors.BAD_REQUEST "NotAuthorized" next ctx
                | None -> return! RequestErrors.BAD_REQUEST "InvalidAuthKey" next ctx
            }
