namespace MusiOrder.NfcReader

open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting

module Program =
    let exitCode = 0

    [<EntryPoint>]
    let main args =

        let builder = WebApplication.CreateBuilder(args)

        builder.Services.AddCors(fun v -> v.AddDefaultPolicy(fun policy -> policy.WithOrigins([|"*"|]) |> ignore)) |> ignore
        builder.Services.AddControllers() |> ignore

        let app = builder.Build()

        app.UseHttpsRedirection() |> ignore

        app.UseAuthorization() |> ignore
        app.UseCors() |> ignore
        app.MapControllers() |> ignore

        app.Run()

        exitCode
