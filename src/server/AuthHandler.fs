namespace AuthHandler

open MusiOrder.Core
open MusiOrder.Models

type CommitOrderResult =
    | AllowCommitOrder of UserId
    | DenyCommitOrderNotAuthorized
    | DenyCommitOrderNoOrderUser

type IAuthHandler =
    abstract member CommitOrder: authUser: User option -> orderUserId: UserId option -> CommitOrderResult

type AuthenticatedUsersAuthHandler() =
    interface IAuthHandler with
        member _.CommitOrder authUser orderUserId =
            match authUser, orderUserId with
            | Some authUser, Some orderUserId when User.isAdmin authUser -> AllowCommitOrder orderUserId
            | Some authUser, None -> AllowCommitOrder authUser.Id
            | Some _, Some _
            | None, _ -> DenyCommitOrderNotAuthorized

type NoAuthenticationAuthHandler() =
    interface IAuthHandler with
        member _.CommitOrder authUser orderUserId =
            match authUser, orderUserId with
            | _, Some orderUserId -> AllowCommitOrder orderUserId
            | _, None -> DenyCommitOrderNoOrderUser

type SingleUserAuthHandler(userId) =
    interface IAuthHandler with
        member _.CommitOrder _ _ = AllowCommitOrder userId
