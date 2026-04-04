namespace MusiOrder.Server.Controllers

open Microsoft.AspNetCore.Mvc
open MusiOrder.Core
open MusiOrder.Core.ProductAdministration
open MusiOrder.Models
open MusiOrder.Models.ProductAdministration

type ProductGroupDataDto = { Name: string }

module ProductGroupDataDto =
    let toDomain (dto: ProductGroupDataDto) : ProductGroupData = {
        Name =
            dto.Name
            |> NotEmptyString.tryCreate
            |> Option.defaultWith (fun () -> failwith "Name required")
    }

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

[<Route("api/administration/product")>]
[<Produces("application/json")>]
type ProductAdministrationController() =

    [<HttpGet("products")>]
    member _.GetProducts([<FromQuery>] authKey: string) =
        task {
            match! User.getByAuthKeyDto authKey with
            | Some user when User.isAdmin user ->
                let! result = getProducts ()

                let dto =
                    result
                    |> List.map (fun g ->
                        let products =
                            g.Products
                            |> List.map (fun p ->
                                let (ProductId s) = p.Id

                                {
                                    Id = s
                                    Data = {
                                        Name = p.Data.Name.Value
                                        Price = NonNegativeDecimal.value p.Data.Price
                                        State = ProductState.toString p.Data.State
                                    }
                                })
                            |> List.toArray

                        let (ProductGroupId gs) = g.Id

                        {
                            Id = gs
                            Data = { Name = g.Data.Name.Value }
                            Products = products
                        })
                    |> List.toArray

                return OkObjectResult(dto) :> IActionResult
            | Some _ -> return BadRequestObjectResult("NotAuthorized") :> IActionResult
            | None -> return BadRequestObjectResult("InvalidAuthKey") :> IActionResult
        }

    [<HttpPost("groups")>]
    member _.PostProductGroup([<FromQuery>] authKey: string, [<FromBody>] dto: ProductGroupDataDto) =
        task {
            match! User.getByAuthKeyDto authKey with
            | Some user when User.isAdmin user ->
                let! newProductGroupId = createProductGroup (ProductGroupDataDto.toDomain dto)
                let (ProductGroupId s) = newProductGroupId
                return OkObjectResult(s) :> IActionResult
            | Some _ -> return BadRequestObjectResult("NotAuthorized") :> IActionResult
            | None -> return BadRequestObjectResult("InvalidAuthKey") :> IActionResult
        }

    [<HttpPut("groups/{productGroupId}")>]
    member _.PutProductGroup
        ([<FromRoute>] productGroupId: string, [<FromQuery>] authKey: string, [<FromBody>] dto: ProductGroupDataDto)
        =
        task {
            match! User.getByAuthKeyDto authKey with
            | Some user when User.isAdmin user ->
                do! updateProductGroup (ProductGroupId productGroupId) (ProductGroupDataDto.toDomain dto)
                return OkResult() :> IActionResult
            | Some _ -> return BadRequestObjectResult("NotAuthorized") :> IActionResult
            | None -> return BadRequestObjectResult("InvalidAuthKey") :> IActionResult
        }

    [<HttpPost("groups/{productGroupId}/move-up")>]
    member _.MoveUpProductGroup([<FromRoute>] productGroupId: string, [<FromQuery>] authKey: string) =
        task {
            match! User.getByAuthKeyDto authKey with
            | Some user when User.isAdmin user ->
                do! moveUpProductGroup (ProductGroupId productGroupId)
                return OkResult() :> IActionResult
            | Some _ -> return BadRequestObjectResult("NotAuthorized") :> IActionResult
            | None -> return BadRequestObjectResult("InvalidAuthKey") :> IActionResult
        }

    [<HttpPost("groups/{productGroupId}/move-down")>]
    member _.MoveDownProductGroup([<FromRoute>] productGroupId: string, [<FromQuery>] authKey: string) =
        task {
            match! User.getByAuthKeyDto authKey with
            | Some user when User.isAdmin user ->
                do! moveDownProductGroup (ProductGroupId productGroupId)
                return OkResult() :> IActionResult
            | Some _ -> return BadRequestObjectResult("NotAuthorized") :> IActionResult
            | None -> return BadRequestObjectResult("InvalidAuthKey") :> IActionResult
        }

    [<HttpDelete("groups/{productGroupId}")>]
    member _.DeleteProductGroup([<FromRoute>] productGroupId: string, [<FromQuery>] authKey: string) =
        task {
            match! User.getByAuthKeyDto authKey with
            | Some user when User.isAdmin user ->
                let! isDeleted = deleteProductGroup (ProductGroupId productGroupId)

                if isDeleted then
                    return OkResult() :> IActionResult
                else
                    return BadRequestObjectResult("GroupNotEmpty") :> IActionResult
            | Some _ -> return BadRequestObjectResult("NotAuthorized") :> IActionResult
            | None -> return BadRequestObjectResult("InvalidAuthKey") :> IActionResult
        }

    [<HttpPost("groups/{productGroupId}/products")>]
    member _.PostProduct
        ([<FromRoute>] productGroupId: string, [<FromQuery>] authKey: string, [<FromBody>] dto: ProductDataDto)
        =
        task {
            match! User.getByAuthKeyDto authKey with
            | Some user when User.isAdmin user ->
                let! newProductId = createProduct (ProductGroupId productGroupId) (ProductDataDto.toDomain dto)
                let (ProductGroupId s) = newProductId
                return OkObjectResult(s) :> IActionResult
            | Some _ -> return BadRequestObjectResult("NotAuthorized") :> IActionResult
            | None -> return BadRequestObjectResult("InvalidAuthKey") :> IActionResult
        }

    [<HttpPut("products/{productId}")>]
    member _.PutProduct
        ([<FromRoute>] productId: string, [<FromQuery>] authKey: string, [<FromBody>] dto: ProductDataDto)
        =
        task {
            match! User.getByAuthKeyDto authKey with
            | Some user when User.isAdmin user ->
                do! updateProduct (ProductId productId) (ProductDataDto.toDomain dto)
                return OkResult() :> IActionResult
            | Some _ -> return BadRequestObjectResult("NotAuthorized") :> IActionResult
            | None -> return BadRequestObjectResult("InvalidAuthKey") :> IActionResult
        }

    [<HttpPost("products/{productId}/move-up")>]
    member _.MoveUpProduct([<FromRoute>] productId: string, [<FromQuery>] authKey: string) =
        task {
            match! User.getByAuthKeyDto authKey with
            | Some user when User.isAdmin user ->
                do! moveUpProduct (ProductId productId)
                return OkResult() :> IActionResult
            | Some _ -> return BadRequestObjectResult("NotAuthorized") :> IActionResult
            | None -> return BadRequestObjectResult("InvalidAuthKey") :> IActionResult
        }

    [<HttpPost("products/{productId}/move-down")>]
    member _.MoveDownProduct([<FromRoute>] productId: string, [<FromQuery>] authKey: string) =
        task {
            match! User.getByAuthKeyDto authKey with
            | Some user when User.isAdmin user ->
                do! moveDownProduct (ProductId productId)
                return OkResult() :> IActionResult
            | Some _ -> return BadRequestObjectResult("NotAuthorized") :> IActionResult
            | None -> return BadRequestObjectResult("InvalidAuthKey") :> IActionResult
        }

    [<HttpDelete("products/{productId}")>]
    member _.DeleteProduct([<FromRoute>] productId: string, [<FromQuery>] authKey: string) =
        task {
            match! User.getByAuthKeyDto authKey with
            | Some user when User.isAdmin user ->
                do! deleteProduct (ProductId productId)
                return OkResult() :> IActionResult
            | Some _ -> return BadRequestObjectResult("NotAuthorized") :> IActionResult
            | None -> return BadRequestObjectResult("InvalidAuthKey") :> IActionResult
        }
