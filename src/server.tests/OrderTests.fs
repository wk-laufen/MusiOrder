module MusiOrder.Server.Tests.OrderTests

open System.Net
open System.Net.Http
open System.Net.Http.Json
open System.Text.Json
open Expecto
open AuthHandler
open Helpers

let tests =
    testList "Order" [
        testList "getProducts" [
            testTask "returns 200 with empty array on empty DB" {
                let! (client: HttpClient) = createTestClient (NoAuthenticationAuthHandler())
                let! (response: HttpResponseMessage) = client.GetAsync("/api/order/products")
                Expect.equal response.StatusCode HttpStatusCode.OK "should return 200"
                let! body = response.Content.ReadFromJsonAsync<JsonElement array>()
                Expect.isEmpty body "empty DB should return empty array"
            }
        ]

        testList "getOrderSummary" [
            testTask "without userId returns 400 NoOrderSummaryUser (NoAuthentication, no users)" {
                let! (client: HttpClient) = createTestClient (NoAuthenticationAuthHandler())
                let! (response: HttpResponseMessage) = client.GetAsync("/api/order/summary")
                Expect.equal response.StatusCode HttpStatusCode.BadRequest "should return 400"
                let! body = response.Content.ReadFromJsonAsync<string>()
                Expect.equal body "NoOrderSummaryUser" "should report no summary user"
            }

            testTask "with valid-format but unknown authKey returns 400 InvalidAuthKey (AuthenticatedUsers)" {
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.GetAsync("/api/order/summary?authKey=nfc/unknownkey")
                Expect.equal response.StatusCode HttpStatusCode.BadRequest "should return 400"
                let! body = response.Content.ReadFromJsonAsync<string>()
                Expect.equal body "InvalidAuthKey" "should report invalid auth key"
            }

            testTask "without authKey returns 400 NotAuthorized (AuthenticatedUsers)" {
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.GetAsync("/api/order/summary")
                Expect.equal response.StatusCode HttpStatusCode.BadRequest "should return 400"
                let! body = response.Content.ReadFromJsonAsync<string>()
                Expect.equal body "NotAuthorized" "should report not authorized"
            }
        ]

        testList "getUsers" [
            testTask "returns 200 (NoAuthentication always allows)" {
                let! (client: HttpClient) = createTestClient (NoAuthenticationAuthHandler())
                let! (response: HttpResponseMessage) = client.GetAsync("/api/order/users")
                Expect.equal response.StatusCode HttpStatusCode.OK "should return 200"
            }
        ]

        testList "postOrder" [
            testTask "without userId returns 400 NoOrderUser (NoAuthentication)" {
                let! (client: HttpClient) = createTestClient (NoAuthenticationAuthHandler())
                let content = new StringContent("[]", System.Text.Encoding.UTF8, "application/json")
                let! (response: HttpResponseMessage) = client.PostAsync("/api/order", content)
                Expect.equal response.StatusCode HttpStatusCode.BadRequest "should return 400"
                let! (body: string array) = response.Content.ReadFromJsonAsync<string array>()
                Expect.contains body "NoOrderUser" "should report no order user"
            }
        ]
    ]
