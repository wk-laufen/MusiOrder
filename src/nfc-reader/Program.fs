namespace MusiOrder.NfcReader

open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open MusiOrder.NfcReader.Controllers

module Program =
    let exitCode = 0

    [<EntryPoint>]
    let main args =

        let builder = WebApplication.CreateBuilder(args)

        builder.Services.AddCors(fun v ->
            v.AddDefaultPolicy(fun policy ->
                policy
                    .AllowAnyOrigin()
                    .AllowAnyMethod()
                |> ignore
            )
        ) |> ignore
        builder.Services.AddControllers() |> ignore
        builder.Services.AddTransient<ICardReader>(fun serviceProvider ->
            let cardReaderSection = builder.Configuration.GetSection("CardReader")
            let cardReaderType = cardReaderSection.["Type"]
            if cardReaderType = "console" then ConsoleCardReader() :> ICardReader
            elif cardReaderType = "pcsc" then new PcscCardReader()
            elif cardReaderType = "pn532-uart" then
                let devicePaths = cardReaderSection.GetRequiredSection("DevicePaths").Get<string[]>()
                PN532UARTCardReader(devicePaths, serviceProvider.GetService<ILogger<PN532UARTCardReader>>())
            else failwith $"Unknown card reader type \"{cardReaderType}\"") |> ignore

        let app = builder.Build()

        app.UseHttpsRedirection() |> ignore

        app.UseAuthorization() |> ignore
        app.UseCors() |> ignore
        app.MapControllers() |> ignore

        app.Run()

        exitCode
