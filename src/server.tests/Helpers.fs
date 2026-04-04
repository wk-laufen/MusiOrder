module MusiOrder.Server.Tests.Helpers

open System
open System.IO
open System.Net.Http
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.TestHost
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Giraffe
open Giraffe.Serialization
open MusiOrder.Server.Serialization
open AuthHandler
open MusiOrder.Models

let initTestDb () =
    let dbPath = Path.Combine(Path.GetTempPath(), $"musiorder-test-{Guid.NewGuid()}.sqlite")
    let schemaPath = Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "db-schema.sql")
    let schema = File.ReadAllText(schemaPath)
    use connection = new Microsoft.Data.Sqlite.SqliteConnection($"Data Source={dbPath}")
    connection.Open()
    schema.Split(';')
    |> Array.map (fun s -> s.Trim())
    |> Array.filter (fun s -> s.Length > 0)
    |> Array.iter (fun sql ->
        use cmd = connection.CreateCommand(CommandText = sql)
        cmd.ExecuteNonQuery() |> ignore
    )
    dbPath

let createTestClient (authHandler: IAuthHandler) = task {
    let host =
        (new HostBuilder())
            .ConfigureWebHost(fun webBuilder ->
                webBuilder
                    .UseTestServer()
                    .ConfigureServices(fun services ->
                        services.AddGiraffe() |> ignore
                        services.AddSingleton<IJsonSerializer>(SystemTextJsonSerializer()) |> ignore
                        services.AddSingleton<IAuthHandler>(authHandler) |> ignore
                    )
                    .Configure(fun app ->
                        app.UseGiraffe(MusiOrder.Server.App.webApp)
                    )
                |> ignore
            )
    let! builtHost = host.StartAsync()
    return builtHost.GetTestClient()
}

let seedMember (id: string) (firstName: string) (lastName: string) (role: string) =
    DB.write
        "INSERT INTO `Member` (`id`, `firstName`, `lastName`, `role`) VALUES (@Id, @FirstName, @LastName, @Role)"
        [ ("@Id", box id); ("@FirstName", box firstName); ("@LastName", box lastName); ("@Role", box role) ]

let seedAuthKey (keyCode: string) (userId: string) =
    DB.write
        "INSERT INTO `AuthKey` (`keyCode`, `keyType`, `userId`, `creationTime`) VALUES (@KeyCode, 'nfc', @UserId, @CreationTime)"
        [ ("@KeyCode", box keyCode); ("@UserId", box userId); ("@CreationTime", box (DateTimeOffset.Now.ToString("o"))) ]

/// Seeds an admin member with an NFC auth key. Returns (userId, "nfc/{keyCode}").
let seedAdmin () = task {
    let id = sprintf "%O" (Guid.NewGuid())
    let keyCode = sprintf "%O" (Guid.NewGuid())
    do! seedMember id "Admin" "Test" "admin"
    do! seedAuthKey keyCode id
    return (id, sprintf "nfc/%s" keyCode)
}

/// Seeds a regular user member with an NFC auth key. Returns (userId, "nfc/{keyCode}").
let seedRegularUser () = task {
    let id = sprintf "%O" (Guid.NewGuid())
    let keyCode = sprintf "%O" (Guid.NewGuid())
    do! seedMember id "Regular" "Test" "user"
    do! seedAuthKey keyCode id
    return (id, sprintf "nfc/%s" keyCode)
}

/// Seeds a product group. Returns the group id.
let seedProductGroup (name: string) = task {
    let id = sprintf "%O" (Guid.NewGuid())
    do! DB.write
            "INSERT INTO `ArticleGroup` (`id`, `grade`, `name`) VALUES (@Id, (SELECT COALESCE(MAX(`grade`) + 1, 1) FROM `ArticleGroup`), @Name)"
            [ ("@Id", box id); ("@Name", box name) ]
    return id
}

/// Seeds a product in a group. Returns the product id.
let seedProduct (groupId: string) (name: string) (price: decimal) = task {
    let id = sprintf "%O" (Guid.NewGuid())
    do! DB.write
            "INSERT INTO `Article` (`id`, `groupId`, `state`, `grade`, `name`, `price`) VALUES (@Id, @GroupId, 'enabled', (SELECT COALESCE(MAX(`grade`) + 1, 1) FROM `Article` WHERE `groupId` = @GroupId), @Name, @Price)"
            [ ("@Id", box id); ("@GroupId", box groupId); ("@Name", box name); ("@Price", box price) ]
    return id
}

/// Seeds an order entry. Returns the order id.
let seedOrder (userId: string) (productName: string) (amount: int) (price: decimal) = task {
    let id = sprintf "%O" (Guid.NewGuid())
    do! DB.write
            "INSERT INTO `Order` (`id`, `userId`, `articleName`, `amount`, `pricePerUnit`, `timestamp`) VALUES (@Id, @UserId, @Name, @Amount, @Price, @Timestamp)"
            [ ("@Id", box id); ("@UserId", box userId); ("@Name", box productName); ("@Amount", box amount); ("@Price", box price); ("@Timestamp", box DateTimeOffset.Now) ]
    return id
}

/// Seeds a payment. Returns the payment id.
let seedPayment (userId: string) (amount: decimal) = task {
    let id = sprintf "%O" (Guid.NewGuid())
    do! DB.write
            "INSERT INTO `MemberPayment` (`id`, `userId`, `amount`, `timestamp`) VALUES (@Id, @UserId, @Amount, @Timestamp)"
            [ ("@Id", box id); ("@UserId", box userId); ("@Amount", box amount); ("@Timestamp", box DateTimeOffset.Now) ]
    return id
}

/// Creates a JSON HTTP StringContent.
let jsonContent (json: string) = new StringContent(json, System.Text.Encoding.UTF8, "application/json")

/// URL-encodes an auth key for use in a query string: "?authKey={key}"
let authQuery (key: string) = sprintf "?authKey=%s" (System.Uri.EscapeDataString(key: string))
