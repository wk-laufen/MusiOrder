namespace MusiOrder.Server.HttpHandler

open FSharp.Control.Tasks.V2.ContextInsensitive
open Giraffe
open MusiOrder.Core
open Microsoft.Data.Sqlite
open MusiOrder.Models
open System

module Order =
    open MusiOrder.Core.Order
    open MusiOrder.Models.Order

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
        let! authUser = ctx.TryGetQueryStringValue "authKey" |> Option.bindTask (AuthKey >> User.getByAuthKey)
        let orderUserId = ctx.TryGetQueryStringValue "userId" |> Option.map UserId
        match authUser, orderUserId with
        | Some authUser, Some userId when User.isAdmin authUser -> return! saveOrder userId next ctx
        | Some authUser, None -> return! saveOrder authUser.Id next ctx
        | Some _, Some _ -> return! RequestErrors.BAD_REQUEST [ AddOrderError.NotAuthorized ] next ctx
        | None, _ -> return! RequestErrors.BAD_REQUEST [ AddOrderError.InvalidAuthKey ] next ctx
    }

    let loadOrderSummary (user: User) : HttpHandler = fun next ctx -> task {
        let! balance = User.getBalance user.Id
        let! latestOrders = getLatestOrdersFromUser user.Id
        let result =
            {
                ClientFullName = sprintf "%s %s" user.FirstName user.LastName
                Balance = balance
                LatestOrders = latestOrders
            }
        return! Successful.OK result next ctx
    }

    let handleGetOrderSummary : HttpHandler = fun next ctx -> task {
        let! authUser = ctx.TryGetQueryStringValue "authKey" |> Option.bindTask (AuthKey >> User.getByAuthKey)
        let! orderUser = ctx.TryGetQueryStringValue "userId" |> Option.bindTask (UserId >> User.getById)
        match authUser, orderUser with
        | Some authUser, Some user when User.isAdmin authUser -> return! loadOrderSummary user next ctx
        | Some authUser, None -> return! loadOrderSummary authUser next ctx
        | Some _, Some _ -> return! RequestErrors.BAD_REQUEST LoadOrderSummaryError.NotAuthorized next ctx
        | None, _ -> return! RequestErrors.BAD_REQUEST LoadOrderSummaryError.InvalidAuthKey next ctx
    }

    let handleGetUsers : HttpHandler = fun next ctx -> task {
        match! ctx.TryGetQueryStringValue "authKey" |> Option.bindTask (AuthKey >> User.getByAuthKey) with
        | Some user when User.isAdmin user ->
            let! result = getUserInfo ()
            return! Successful.OK result next ctx
        | Some _ -> return! RequestErrors.BAD_REQUEST LoadUsersError.NotAuthorized next ctx
        | None -> return! RequestErrors.BAD_REQUEST LoadUsersError.InvalidAuthKey next ctx
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
        let! data = ctx.BindModelAsync<Payment>()
        match! ctx.TryGetQueryStringValue "authKey" |> Option.bindTask (AuthKey >> User.getByAuthKey) with
        | Some user when User.isAdmin user ->
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
        let! data = ctx.BindModelAsync<UserData>()
        match! ctx.TryGetQueryStringValue "authKey" |> Option.bindTask (AuthKey >> User.getByAuthKey) with
        | Some user when User.isAdmin user ->
            match! createUser data with
            | Ok newUserId -> return! Successful.OK newUserId next ctx
            | Error e -> return! RequestErrors.BAD_REQUEST e next ctx
        | Some _ -> return! RequestErrors.BAD_REQUEST SaveUserError.NotAuthorized next ctx
        | None -> return! RequestErrors.BAD_REQUEST SaveUserError.InvalidAuthKey next ctx
    }

    let handlePutUser userId : HttpHandler = fun next ctx -> task {
        let! data = ctx.BindModelAsync<UserData>()
        match! ctx.TryGetQueryStringValue "authKey" |> Option.bindTask (AuthKey >> User.getByAuthKey) with
        | Some user when User.isAdmin user ->
            if user.Id = userId && data.Role <> Admin then
                return! RequestErrors.BAD_REQUEST DowngradeSelfNotAllowed next ctx
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
