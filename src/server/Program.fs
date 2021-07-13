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
open Thoth.Json.Net
open Thoth.Json.Giraffe

// ---------------------------------
// Web app
// ---------------------------------

let webApp =
    choose [
        subRoute "/api" (
            choose [
                subRoute "/order" (
                    choose [
                        GET >=> route "/products" >=> HttpHandler.Order.handleGetProducts
                        GET >=> route "/summary" >=> HttpHandler.Order.handleGetOrderSummary
                        GET >=> route "/users" >=> HttpHandler.Order.handleGetUsers
                        POST >=> route "" >=> HttpHandler.Order.handlePostOrder
                    ]
                )
                subRoute "/administration" (
                    choose [
                        subRoute "/user-payment" (
                            choose [
                                GET >=> route "/users" >=> HttpHandler.UserPaymentAdministration.handleGetUsers
                                POST >=> routef "/%s" (UserId >> HttpHandler.UserPaymentAdministration.handlePostPayment)
                            ]
                        )
                        subRoute "/user" (
                            choose [
                                GET >=> route "/users" >=> HttpHandler.UserAdministration.handleGetUsers
                                POST >=> route "/users" >=> HttpHandler.UserAdministration.handlePostUser
                                PUT >=> routef "/users/%s" (UserId >> HttpHandler.UserAdministration.handlePutUser)
                                DELETE >=> routef "/users/%s" (UserId >> HttpHandler.UserAdministration.handleDeleteUser)
                            ]
                        )
                        subRoute "/order" (
                            choose [
                                GET >=> route "/orders" >=> HttpHandler.OrderAdministration.handleGetOrders
                                DELETE >=> routef "/orders/%s" (OrderId >> HttpHandler.OrderAdministration.handleDeleteOrder)
                            ]
                        )
                    ]
                )
            ])
        setStatusCode 404 >=> text "Not Found"
    ]

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