namespace MusiOrder.Server.Controllers

open Microsoft.AspNetCore.Mvc
open MusiOrder.Core
open MusiOrder.Core.UserAdministration
open MusiOrder.Models
open MusiOrder.Models.UserAdministration
open System

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

[<Route("api/administration/user/users")>]
[<Produces("application/json")>]
type UserAdministrationController() =

    let fromUserRole =
        function
        | Admin -> "Admin"
        | OrderAssistant -> "OrderAssistant"
        | User -> "User"

    [<HttpGet>]
    member _.GetUsers([<FromQuery>] authKey: string) =
        task {
            match! User.getByAuthKeyDto authKey with
            | Some user when User.isAdmin user ->
                let! result = getExistingUsers ()

                let dto =
                    result
                    |> List.map (fun u ->
                        let (UserId s) = u.Id

                        {
                            Id = s
                            Data = {
                                FirstName = u.Data.FirstName.Value
                                LastName = u.Data.LastName.Value
                                AuthKeys = u.Data.AuthKeys |> List.map AuthKeyDto.fromDomain |> List.toArray
                                Role = fromUserRole u.Data.Role
                            }
                        })
                    |> List.toArray

                return OkObjectResult(dto) :> IActionResult
            | Some _ -> return BadRequestObjectResult("NotAuthorized") :> IActionResult
            | None -> return BadRequestObjectResult("InvalidAuthKey") :> IActionResult
        }

    [<HttpPost>]
    member _.PostUser([<FromQuery>] authKey: string, [<FromBody>] dto: ExistingUserDataDto) =
        task {
            match! User.getByAuthKeyDto authKey with
            | Some user when User.isAdmin user ->
                match! createUser (ExistingUserDataDto.toDomain dto) with
                | Ok newUserId ->
                    let (UserId s) = newUserId
                    return OkObjectResult(s) :> IActionResult
                | Error(KeyCodeTaken names) ->
                    return BadRequestObjectResult([| box "KeyCodeTaken"; box (List.toArray names) |]) :> IActionResult
                | Error e -> return BadRequestObjectResult(string e) :> IActionResult
            | Some _ -> return BadRequestObjectResult("NotAuthorized") :> IActionResult
            | None -> return BadRequestObjectResult("InvalidAuthKey") :> IActionResult
        }

    [<HttpPut("{userId}")>]
    member _.PutUser([<FromRoute>] userId: string, [<FromQuery>] authKey: string, [<FromBody>] dto: PatchUserDataDto) =
        task {
            match authKey |> Option.ofObj |> Option.bind AuthKey.tryParse with
            | Some parsedKey ->
                match! User.getByAuthKey parsedKey with
                | Some user when User.isAdmin user ->
                    let patchData = PatchUserDataDto.toDomain dto
                    let uid = UserId userId

                    if user.Id = uid && (patchData.Role <> None && patchData.Role <> Some Admin) then
                        return BadRequestObjectResult("DowngradeSelfNotAllowed") :> IActionResult
                    elif user.Id = uid && patchData.RemoveAuthKeys |> List.contains parsedKey then
                        return BadRequestObjectResult("RemoveActiveAuthKeyNotAllowed") :> IActionResult
                    else
                        match! updateUser uid patchData with
                        | Ok() -> return OkResult() :> IActionResult
                        | Error(KeyCodeTaken names) ->
                            return
                                BadRequestObjectResult([| box "KeyCodeTaken"; box (List.toArray names) |])
                                :> IActionResult
                        | Error e -> return BadRequestObjectResult(string e) :> IActionResult
                | Some _ -> return BadRequestObjectResult("NotAuthorized") :> IActionResult
                | None -> return BadRequestObjectResult("InvalidAuthKey") :> IActionResult
            | None -> return BadRequestObjectResult("InvalidAuthKey") :> IActionResult
        }

    [<HttpDelete("{userId}")>]
    member _.DeleteUser([<FromRoute>] userId: string, [<FromQuery>] authKey: string, [<FromQuery>] force: string) =
        task {
            match! User.getByAuthKeyDto authKey with
            | Some user when User.isAdmin user ->
                let uid = UserId userId

                match force |> Option.ofObj with
                | Some _ ->
                    do! deleteUser uid
                    return OkResult() :> IActionResult
                | None ->
                    let! userAuthKeys =
                        task {
                            let! u = User.getById uid
                            return u |> Option.map (fun v -> v.AuthKeys) |> Option.defaultValue []
                        }

                    let! balance = User.getBalance uid

                    let warnings = [
                        if not <| List.isEmpty userAuthKeys then
                            AuthKeyPresent
                        if balance <> 0.0m then
                            CurrentBalanceNotZero balance
                    ]

                    let mapWarning =
                        function
                        | AuthKeyPresent -> box "AuthKeyPresent"
                        | CurrentBalanceNotZero b -> box [| box "CurrentBalanceNotZero"; box b |]

                    let dto = warnings |> List.map mapWarning |> List.toArray
                    return OkObjectResult(dto) :> IActionResult
            | Some _ -> return BadRequestObjectResult("NotAuthorized") :> IActionResult
            | None -> return BadRequestObjectResult("InvalidAuthKey") :> IActionResult
        }
