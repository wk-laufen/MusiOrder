module MusiOrder.Server.App

open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Giraffe.Serialization
open MusiOrder.Models
open MusiOrder.Server.HttpHandlers
open Thoth.Json.Net
open Thoth.Json.Giraffe

// ---------------------------------
// Web app
// ---------------------------------

let webApp =
    choose [
        subRoute "/api"
            (choose [
                GET >=> choose [
                    route "/grouped-products" >=> handleGetGroupedProducts
                    route "/order/summary" >=> handleGetOrderSummary
                    route "/order/info" >=> handleGetOrderInfo
                    route "/user" >=> handleGetUserData
                    route "/user/info" >=> handleGetUserInfo
                ]
                POST >=> choose [
                    route "/order" >=> handlePostOrder
                    route "/payment" >=> handlePostPayment
                    route "/user" >=> handleCreateUser
                ]
                PUT >=> choose [
                    routef "/user/%s" handleUpdateUser
                ]
                DELETE >=> choose [
                    routef "/order/%s" handleDeleteOrder
                ]
            ])
        setStatusCode 404 >=> text "Not Found" ]

// ---------------------------------
// Error handler
// ---------------------------------

let errorHandler (ex : Exception) (logger : ILogger) =
    logger.LogError(ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

// ---------------------------------
// Config and Main
// ---------------------------------

let configureCors (builder : CorsPolicyBuilder) =
    builder
        .WithOrigins("http://localhost:8080")
        .AllowAnyMethod()
        .AllowAnyHeader()
   |> ignore

let configureApp (ctx: WebHostBuilderContext) (app : IApplicationBuilder) =
    if ctx.HostingEnvironment.IsDevelopment() then
        app.UseDeveloperExceptionPage() |> ignore
    else
        app.UseGiraffeErrorHandler(errorHandler) |> ignore

    app
        .UseDefaultFiles()
        .UseStaticFiles()
        .UseHttpsRedirection()
        .UseCors(configureCors)
        .UseGiraffe(webApp)

let configureServices (services : IServiceCollection) =
    services
        .AddCors()
        .AddGiraffe()
    |> ignore

    services.AddSingleton<IJsonSerializer>(ThothSerializer(caseStrategy = CamelCase, extra = Json.coders)) |> ignore

let configureLogging (ctx: HostBuilderContext) (builder : ILoggingBuilder) =
    builder
        .AddFilter(fun l -> ctx.HostingEnvironment.IsDevelopment() || l >= LogLevel.Error)
        .AddConsole()
        .AddDebug()
    |> ignore

[<EntryPoint>]
let main args =
    Host.CreateDefaultBuilder(args)
        .ConfigureWebHostDefaults(fun webHostBuilder -> webHostBuilder.Configure(configureApp) |> ignore)
        .ConfigureLogging(configureLogging)
        .ConfigureServices(configureServices)
        .Build()
        .Run()
    0