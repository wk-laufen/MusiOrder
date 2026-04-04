namespace MusiOrder.Server.Controllers

open FSharp.Control.Tasks.V2.ContextInsensitive
open Microsoft.AspNetCore.Mvc
open MusiOrder.Core
open MusiOrder.Models
open System.IO

[<Route("api/administration/data-export")>]
[<Produces("application/json")>]
type DataExportController() =

    [<HttpGet("export-db")>]
    member _.ExportDb([<FromQuery>] authKey: string) =
        task {
            match! User.getByAuthKeyDto authKey with
            | Some user when User.isAdmin user ->
                let stream = File.OpenRead DB.dbPath
                return FileStreamResult(stream, "application/octet-stream") :> IActionResult
            | Some _ -> return BadRequestObjectResult("NotAuthorized") :> IActionResult
            | None -> return BadRequestObjectResult("InvalidAuthKey") :> IActionResult
        }
