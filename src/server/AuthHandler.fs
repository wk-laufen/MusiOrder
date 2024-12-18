namespace AuthHandler

open MusiOrder.Core
open MusiOrder.Models

type GetOrderSummaryResult =
    | GetOrderSummaryAllowed of User
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
        member _.GetOrderSummary authUser summaryUserId =
            match authUser, summaryUserId with
            | Some authUser, Some userId when User.isAdmin authUser -> GetOrderSummaryAllowed userId
            | Some authUser, None -> GetOrderSummaryAllowed authUser
            | _ -> GetOrderSummaryNotAuthorized

        member _.GetUsers authUser =
            match authUser with
            | Some user when User.isAdmin user -> GetUsersAllowed
            | Some _
            | None -> GetUsersNotAuthorized

        member _.CommitOrder authUser orderUserId =
            match authUser, orderUserId with
            | Some authUser, Some orderUserId when User.isAdmin authUser -> AllowCommitOrder orderUserId
            | Some authUser, None -> AllowCommitOrder authUser.Id
            | Some _, Some _
            | None, _ -> DenyCommitOrderNotAuthorized

type NoAuthenticationAuthHandler() =
    interface IAuthHandler with
        member _.GetOrderSummary authUser summaryUserId =
            match authUser, summaryUserId with
            | _, Some userId -> GetOrderSummaryAllowed userId
            | Some authUser, _ -> GetOrderSummaryAllowed authUser
            | _ -> GetOrderSummaryNoUser

        member _.GetUsers authUser = GetUsersAllowed

        member _.CommitOrder authUser orderUserId =
            match authUser, orderUserId with
            | _, Some orderUserId -> AllowCommitOrder orderUserId
            | _, None -> DenyCommitOrderNoOrderUser

type SingleUserAuthHandler(user: User) =
    interface IAuthHandler with
        member _.GetOrderSummary authUser summaryUserId = GetOrderSummaryAllowed user
        member _.GetUsers authUser = GetUsersAllowed
        member _.CommitOrder authUser orderUserId = AllowCommitOrder user.Id
