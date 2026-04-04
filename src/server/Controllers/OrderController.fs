namespace MusiOrder.Server.Controllers

open AuthHandler
open Microsoft.AspNetCore.Mvc
open MusiOrder.Core
open MusiOrder.Core.Order
open MusiOrder.Models
open MusiOrder.Models.Order
open System

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

[<Route("api/order")>]
[<Produces("application/json")>]
type OrderController(authHandler: IAuthHandler) =

    [<HttpGet("products")>]
    member _.GetProducts() =
        task {
            let! data = getProductGroups ()

            let dto =
                data
                |> List.map (fun g ->
                    let products =
                        g.Products
                        |> List.map (fun p ->
                            let (ProductId s) = p.Id

                            {
                                Id = s
                                Name = p.Name
                                Price = p.Price
                            })
                        |> List.toArray

                    { Name = g.Name; Products = products })
                |> List.toArray

            return OkObjectResult(dto) :> IActionResult
        }

    [<HttpPost>]
    member _.PostOrder
        ([<FromQuery>] authKey: string, [<FromQuery>] userId: string, [<FromBody>] entries: NewOrderEntryDto[])
        =
        task {
            let parsedKey = authKey |> Option.ofObj |> Option.bind AuthKey.tryParse
            let! authUser = User.getByAuthKeyDto authKey
            let orderUserId = userId |> Option.ofObj |> Option.map UserId

            match authHandler.CommitOrder authUser orderUserId with
            | AllowCommitOrder uId ->
                let safeEntries = if isNull (entries :> obj) then [||] else entries

                let data =
                    safeEntries
                    |> Array.choose (fun e ->
                        PositiveInteger.tryCreate e.Amount
                        |> Option.map (fun amount ->
                            ({
                                ProductId = ProductId e.ProductId
                                Amount = amount
                            }
                            : NewOrderEntry)))
                    |> Array.toList

                match! saveOrder data uId with
                | Ok() -> return OkResult() :> IActionResult
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

                    return BadRequestObjectResult(dto) :> IActionResult
            | DenyCommitOrderNotAuthorized when Option.isNone parsedKey ->
                return BadRequestObjectResult([| "NotAuthorized" |]) :> IActionResult
            | DenyCommitOrderNotAuthorized -> return BadRequestObjectResult([| "InvalidAuthKey" |]) :> IActionResult
            | DenyCommitOrderNoOrderUser -> return BadRequestObjectResult([| "NoOrderUser" |]) :> IActionResult
        }

    [<HttpGet("summary")>]
    member _.GetOrderSummary([<FromQuery>] authKey: string, [<FromQuery>] userId: string) =
        task {
            let parsedKey = authKey |> Option.ofObj |> Option.bind AuthKey.tryParse
            let! authUser = User.getByAuthKeyDto authKey
            let! orderUser = userId |> Option.ofObj |> Option.bindTask (UserId >> User.getById)

            match authHandler.GetOrderSummary authUser orderUser with
            | GetOrderSummaryAllowed user ->
                let! balance = User.getBalance user.Id
                let! latestOrders = getLatestOrdersFromUser user.Id

                let dto = {
                    ClientFullName = user.Name
                    Balance = balance
                    LatestOrders =
                        latestOrders
                        |> List.map (fun o ->
                            ({
                                Timestamp = o.Timestamp
                                ProductName = o.ProductName
                                Amount = o.Amount
                            }
                            : HistoricOrderDto))
                        |> List.toArray
                }

                return OkObjectResult(dto) :> IActionResult
            | GetOrderSummaryNotAuthorized when Option.isNone parsedKey ->
                return BadRequestObjectResult("NotAuthorized") :> IActionResult
            | GetOrderSummaryNotAuthorized -> return BadRequestObjectResult("InvalidAuthKey") :> IActionResult
            | GetOrderSummaryNoUser -> return BadRequestObjectResult("NoOrderSummaryUser") :> IActionResult
        }

    [<HttpGet("users")>]
    member _.GetUsers([<FromQuery>] authKey: string) =
        task {
            let! authUser = User.getByAuthKeyDto authKey

            match authHandler.GetUsers authUser with
            | GetUsersAllowed ->
                let! users = getUserInfo ()

                let mapUser (u: UserInfo) =
                    let (UserId s) = u.Id

                    {
                        Id = s
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

                return OkObjectResult(dto) :> IActionResult
            | GetUsersNotAuthorized -> return BadRequestObjectResult("NotAuthorized") :> IActionResult
        }
