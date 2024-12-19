module MusiOrder.Server.App

open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Giraffe.Serialization
open MusiOrder.Models
open Thoth.Json.Net
open Thoth.Json.Giraffe
open AuthHandler

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
                        subRoute "/product" (
                            choose [
                                GET >=> route "/products" >=> HttpHandler.ProductAdministration.handleGetProducts
                                POST >=> route "/groups" >=> HttpHandler.ProductAdministration.handlePostProductGroup
                                PUT >=> routef "/groups/%s" (ProductGroupId >> HttpHandler.ProductAdministration.handlePutProductGroup)
                                POST >=> routef "/groups/%s/move-up" (ProductGroupId >> HttpHandler.ProductAdministration.handleMoveUpProductGroup)
                                POST >=> routef "/groups/%s/move-down" (ProductGroupId >> HttpHandler.ProductAdministration.handleMoveDownProductGroup)
                                DELETE >=> routef "/groups/%s" (ProductGroupId >> HttpHandler.ProductAdministration.handleDeleteProductGroup)
                                POST >=> routef "/groups/%s/products" (ProductGroupId >> HttpHandler.ProductAdministration.handlePostProduct)
                                PUT >=> routef "/products/%s" (ProductId >> HttpHandler.ProductAdministration.handlePutProduct)
                                POST >=> routef "/products/%s/move-up" (ProductId >> HttpHandler.ProductAdministration.handleMoveUpProduct)
                                POST >=> routef "/products/%s/move-down" (ProductId >> HttpHandler.ProductAdministration.handleMoveDownProduct)
                                DELETE >=> routef "/products/%s" (ProductId >> HttpHandler.ProductAdministration.handleDeleteProduct)
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

let configureServices (ctx: HostBuilderContext) (services : IServiceCollection) =
    services
        .AddCors()
        .AddGiraffe()
    |> ignore

    services.AddSingleton<IJsonSerializer>(ThothSerializer(caseStrategy = CamelCase, extra = Json.coders)) |> ignore

    let authHandlerOptions = ctx.Configuration.GetRequiredSection("AuthHandler").Get<AuthHandlerOptions>()
    if authHandlerOptions.Name.Equals("AuthenticatedUsers", StringComparison.InvariantCultureIgnoreCase) then
        services.AddTransient<IAuthHandler, AuthenticatedUsersAuthHandler>() |> ignore
    elif authHandlerOptions.Name.Equals("NoAuthentication", StringComparison.InvariantCultureIgnoreCase) then
        services.AddTransient<IAuthHandler, NoAuthenticationAuthHandler>() |> ignore
    elif authHandlerOptions.Name.Equals("SingleUser", StringComparison.InvariantCultureIgnoreCase) then
        services.AddTransient<IAuthHandler>(fun _ -> SingleUserAuthHandler(OrderSummaryUser.fromConfig authHandlerOptions.User)) |> ignore

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