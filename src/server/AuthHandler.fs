namespace AuthHandler

open MusiOrder.Core
open MusiOrder.Models

type SingleUserConfig() =
    member val Id = "" with get, set
    member val Name = "" with get, set

type AuthHandlerOptions() =
    member val Name = "" with get, set
    member val User = SingleUserConfig() with get, set

type OrderSummaryUser = {
    Id: UserId
    Name: string
}
module OrderSummaryUser =
    let fromConfig (user: SingleUserConfig) =
        {
            Id = UserId user.Id
            Name = user.Name
        }
    let fromUser (user: User) =
        {
            Id = user.Id
            Name = $"%s{user.FirstName} %s{user.LastName}"
        }

type GetOrderSummaryResult =
    | GetOrderSummaryAllowed of OrderSummaryUser
    | GetOrderSummaryNotAuthorized
    | GetOrderSummaryNoUser

type GetUsersResult =
    | GetUsersAllowed
    | GetUsersNotAuthorized

type CommitOrderResult =
    | AllowCommitOrder of UserId
    | DenyCommitOrderNotAuthorized
    | DenyCommitOrderNoOrderUser

type IAuthHandler =
    abstract member GetOrderSummary: authUser: User option -> summaryUserId: User option -> GetOrderSummaryResult
    abstract member GetUsers: authUser: User option -> GetUsersResult
    abstract member CommitOrder: authUser: User option -> orderUserId: UserId option -> CommitOrderResult

type AuthenticatedUsersAuthHandler() =
    interface IAuthHandler with
        member _.GetOrderSummary authUser summaryUser =
            match authUser, summaryUser with
            | Some authUser, Some user when User.canOrderForOtherUsers authUser -> GetOrderSummaryAllowed (OrderSummaryUser.fromUser user)
            | Some authUser, None when User.canOrderForOtherUsers authUser -> GetOrderSummaryNoUser
            | Some authUser, None -> GetOrderSummaryAllowed (OrderSummaryUser.fromUser authUser)
            | _ -> GetOrderSummaryNotAuthorized

        member _.GetUsers authUser =
            match authUser with
            | Some user when User.canOrderForOtherUsers user -> GetUsersAllowed
            | Some _
            | None -> GetUsersNotAuthorized

        member _.CommitOrder authUser orderUserId =
            match authUser, orderUserId with
            | Some authUser, Some orderUserId when User.canOrderForOtherUsers authUser -> AllowCommitOrder orderUserId
            | Some authUser, None -> AllowCommitOrder authUser.Id
            | Some _, Some _
            | None, _ -> DenyCommitOrderNotAuthorized

type NoAuthenticationAuthHandler() =
    interface IAuthHandler with
        member _.GetOrderSummary authUser summaryUser =
            match authUser, summaryUser with
            | _, Some user -> GetOrderSummaryAllowed (OrderSummaryUser.fromUser user)
            | Some authUser, _ -> GetOrderSummaryAllowed (OrderSummaryUser.fromUser authUser)
            | _ -> GetOrderSummaryNoUser

        member _.GetUsers authUser = GetUsersAllowed

        member _.CommitOrder authUser orderUserId =
            match authUser, orderUserId with
            | _, Some orderUserId -> AllowCommitOrder orderUserId
            | _, None -> DenyCommitOrderNoOrderUser

type SingleUserAuthHandler(user: OrderSummaryUser) =
    interface IAuthHandler with
        member _.GetOrderSummary authUser summaryUserId = GetOrderSummaryAllowed user
        member _.GetUsers authUser = GetUsersAllowed
        member _.CommitOrder authUser orderUserId = AllowCommitOrder user.Id
