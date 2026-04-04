module MusiOrder.Server.App

open AuthHandler
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open System

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder args

    builder.Services
        .AddControllers()
        .AddJsonOptions(fun opts ->
            opts.JsonSerializerOptions.PropertyNamingPolicy <- Text.Json.JsonNamingPolicy.CamelCase

            // TODO: remove when client no longer uses Thoth.Json (which encodes decimals as JSON strings)
            opts.JsonSerializerOptions.NumberHandling <-
                Text.Json.Serialization.JsonNumberHandling.AllowReadingFromString)
    |> ignore

    let authHandlerOptions =
        builder.Configuration.GetRequiredSection("AuthHandler").Get<AuthHandlerOptions>()

    if authHandlerOptions.Name.Equals("AuthenticatedUsers", StringComparison.InvariantCultureIgnoreCase) then
        builder.Services.AddTransient<IAuthHandler, AuthenticatedUsersAuthHandler>()
        |> ignore
    elif authHandlerOptions.Name.Equals("NoAuthentication", StringComparison.InvariantCultureIgnoreCase) then
        builder.Services.AddTransient<IAuthHandler, NoAuthenticationAuthHandler>()
        |> ignore
    elif authHandlerOptions.Name.Equals("SingleUser", StringComparison.InvariantCultureIgnoreCase) then
        builder.Services.AddTransient<IAuthHandler>(fun _ ->
            SingleUserAuthHandler(OrderSummaryUser.fromConfig authHandlerOptions.User))
        |> ignore

    let app = builder.Build()

    if app.Environment.IsDevelopment() then
        app.UseDeveloperExceptionPage() |> ignore

    app.UseDefaultFiles() |> ignore
    app.UseStaticFiles() |> ignore
    app.UseHttpsRedirection() |> ignore
    app.MapControllers() |> ignore

    app.Run()
    0
