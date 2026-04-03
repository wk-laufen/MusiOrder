module MusiOrder.Server.Tests.OrderAdministrationTests

open System.Net
open System.Net.Http
open System.Net.Http.Json
open Expecto
open AuthHandler
open Helpers

let tests =
    testList "OrderAdministration" [
        testList "getOrders" [
            testTask "missing authKey returns 400 InvalidAuthKey" {
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.GetAsync("/api/administration/order/orders")
                Expect.equal response.StatusCode HttpStatusCode.BadRequest "should return 400"
                let! body = response.Content.ReadFromJsonAsync<string>()
                Expect.equal body "InvalidAuthKey" "should report InvalidAuthKey"
            }

            testTask "non-admin authKey returns 400 NotAuthorized" {
                let! (_, userKey) = seedRegularUser ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.GetAsync($"/api/administration/order/orders{authQuery userKey}")
                Expect.equal response.StatusCode HttpStatusCode.BadRequest "should return 400"
                let! body = response.Content.ReadFromJsonAsync<string>()
                Expect.equal body "NotAuthorized" "non-admin should be NotAuthorized"
            }

            testTask "admin sees seeded order" {
                let! (userId, _) = seedRegularUser ()
                let! (_, adminKey) = seedAdmin ()
                let! _ = seedOrder userId "OrderTestBeer" 3 2.0m
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.GetAsync($"/api/administration/order/orders{authQuery adminKey}")
                Expect.equal response.StatusCode HttpStatusCode.OK "should return 200"
                let! (body: {| productName: string |} array) = response.Content.ReadFromJsonAsync<{| productName: string |} array>()
                Expect.exists body (fun o -> o.productName = "OrderTestBeer") "should contain seeded order product name"
            }
        ]

        testList "deleteOrder" [
            testTask "missing authKey returns 400 InvalidAuthKey" {
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let orderId = sprintf "%O" (System.Guid.NewGuid())
                let! (response: HttpResponseMessage) = client.DeleteAsync($"/api/administration/order/orders/{orderId}")
                Expect.equal response.StatusCode HttpStatusCode.BadRequest "should return 400"
                let! body = response.Content.ReadFromJsonAsync<string>()
                Expect.equal body "InvalidAuthKey" "should report InvalidAuthKey"
            }

            testTask "non-admin authKey returns 400 NotAuthorized" {
                let! (_, userKey) = seedRegularUser ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let orderId = sprintf "%O" (System.Guid.NewGuid())
                let! (response: HttpResponseMessage) = client.DeleteAsync($"/api/administration/order/orders/{orderId}{authQuery userKey}")
                Expect.equal response.StatusCode HttpStatusCode.BadRequest "should return 400"
                let! body = response.Content.ReadFromJsonAsync<string>()
                Expect.equal body "NotAuthorized" "non-admin should be NotAuthorized"
            }

            testTask "admin deletes non-existent order returns 200 (idempotent)" {
                let! (_, adminKey) = seedAdmin ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let orderId = sprintf "%O" (System.Guid.NewGuid())
                let! (response: HttpResponseMessage) = client.DeleteAsync($"/api/administration/order/orders/{orderId}{authQuery adminKey}")
                Expect.equal response.StatusCode HttpStatusCode.OK "deleting non-existent order should be idempotent"
            }

            testTask "admin deletes seeded order, then it no longer appears in GET" {
                let! (userId, _) = seedRegularUser ()
                let! (_, adminKey) = seedAdmin ()
                let! orderId = seedOrder userId "DeleteTestBeer" 1 1.0m
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (getResponse: HttpResponseMessage) = client.GetAsync($"/api/administration/order/orders{authQuery adminKey}")
                let! (beforeBody: {| productName: string |} array) = getResponse.Content.ReadFromJsonAsync<{| productName: string |} array>()
                Expect.exists beforeBody (fun o -> o.productName = "DeleteTestBeer") "order should exist before deletion"
                let! (deleteResponse: HttpResponseMessage) = client.DeleteAsync($"/api/administration/order/orders/{orderId}{authQuery adminKey}")
                Expect.equal deleteResponse.StatusCode HttpStatusCode.OK "delete should return 200"
                let! (getResponse2: HttpResponseMessage) = client.GetAsync($"/api/administration/order/orders{authQuery adminKey}")
                let! (afterBody: {| productName: string |} array) = getResponse2.Content.ReadFromJsonAsync<{| productName: string |} array>()
                Expect.isEmpty (afterBody |> Array.filter (fun o -> o.productName = "DeleteTestBeer")) "order should not appear after deletion"
            }
        ]
    ]
