namespace MusiOrder.NfcReader

open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
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
        builder.Services.AddTransient<ICardReader, PcscCardReader>() |> ignore

        let app = builder.Build()

        app.UseHttpsRedirection() |> ignore

        app.UseAuthorization() |> ignore
        app.UseCors() |> ignore
        app.MapControllers() |> ignore

        app.Run()

        exitCode
