module MusiOrder.Server.Tests.OrderStatisticsTests

open System.Net
open System.Net.Http
open System.Net.Http.Json
open Expecto
open AuthHandler
open Helpers

let tests =
    testList "OrderStatistics" [
        testList "getOrders" [
            testTask "missing authKey returns 400 InvalidAuthKey" {
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.GetAsync("/api/administration/report/orders")
                Expect.equal response.StatusCode HttpStatusCode.BadRequest "should return 400"
                let! body = response.Content.ReadFromJsonAsync<string>()
                Expect.equal body "InvalidAuthKey" "should report InvalidAuthKey"
            }

            testTask "non-admin authKey returns 400 NotAuthorized" {
                let! (_, userKey) = seedRegularUser ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.GetAsync($"/api/administration/report/orders{authQuery userKey}")
                Expect.equal response.StatusCode HttpStatusCode.BadRequest "should return 400"
                let! body = response.Content.ReadFromJsonAsync<string>()
                Expect.equal body "NotAuthorized" "non-admin should be NotAuthorized"
            }

            testTask "admin with no time range params returns 400 MissingTimeRange" {
                let! (_, adminKey) = seedAdmin ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.GetAsync($"/api/administration/report/orders{authQuery adminKey}")
                Expect.equal response.StatusCode HttpStatusCode.BadRequest "should return 400"
                let! body = response.Content.ReadFromJsonAsync<string>()
                Expect.equal body "MissingTimeRange" "missing time range should be reported"
            }

            testTask "admin with only startTime returns 400 MissingTimeRange" {
                let! (_, adminKey) = seedAdmin ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.GetAsync($"/api/administration/report/orders{authQuery adminKey}&startTime=2024-01-01")
                Expect.equal response.StatusCode HttpStatusCode.BadRequest "should return 400"
                let! body = response.Content.ReadFromJsonAsync<string>()
                Expect.equal body "MissingTimeRange" "missing endTime should cause MissingTimeRange"
            }

            testTask "admin with invalid date format returns 400 MissingTimeRange" {
                let! (_, adminKey) = seedAdmin ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.GetAsync($"/api/administration/report/orders{authQuery adminKey}&startTime=01/01/2024&endTime=12/31/2024")
                Expect.equal response.StatusCode HttpStatusCode.BadRequest "should return 400"
                let! body = response.Content.ReadFromJsonAsync<string>()
                Expect.equal body "MissingTimeRange" "invalid date format should cause MissingTimeRange"
            }

            testTask "admin with valid yyyy-MM-dd date range returns 200" {
                let! (_, adminKey) = seedAdmin ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.GetAsync($"/api/administration/report/orders{authQuery adminKey}&startTime=2024-01-01&endTime=2025-01-01")
                Expect.equal response.StatusCode HttpStatusCode.OK "should return 200"
                let! body = response.Content.ReadFromJsonAsync<System.Text.Json.JsonElement array>()
                Expect.isNotNull (box body) "should return JSON array"
            }
        ]
    ]
