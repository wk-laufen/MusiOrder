module MusiOrder.Server.Tests.UserPaymentAdministrationTests

open System.Net
open System.Net.Http
open System.Net.Http.Json
open Expecto
open AuthHandler
open Helpers

let tests =
    testList "UserPaymentAdministration" [
        testList "getUsers" [
            testTask "missing authKey returns 400 InvalidAuthKey" {
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.GetAsync("/api/administration/user-payment/users")
                Expect.equal response.StatusCode HttpStatusCode.BadRequest "should return 400"
                let! body = response.Content.ReadFromJsonAsync<string>()
                Expect.equal body "InvalidAuthKey" "should report InvalidAuthKey"
            }

            testTask "unknown-format authKey returns 400 InvalidAuthKey" {
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.GetAsync("/api/administration/user-payment/users?authKey=notanfc")
                Expect.equal response.StatusCode HttpStatusCode.BadRequest "should return 400"
                let! body = response.Content.ReadFromJsonAsync<string>()
                Expect.equal body "InvalidAuthKey" "non-nfc/ key should be InvalidAuthKey"
            }

            testTask "valid-format but unknown authKey returns 400 InvalidAuthKey" {
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.GetAsync("/api/administration/user-payment/users?authKey=nfc/unknownkey")
                Expect.equal response.StatusCode HttpStatusCode.BadRequest "should return 400"
                let! body = response.Content.ReadFromJsonAsync<string>()
                Expect.equal body "InvalidAuthKey" "unknown key should be InvalidAuthKey"
            }

            testTask "non-admin authKey returns 400 NotAuthorized" {
                let! (_, userKey) = seedRegularUser ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.GetAsync($"/api/administration/user-payment/users{authQuery userKey}")
                Expect.equal response.StatusCode HttpStatusCode.BadRequest "should return 400"
                let! body = response.Content.ReadFromJsonAsync<string>()
                Expect.equal body "NotAuthorized" "non-admin key should be NotAuthorized"
            }

            testTask "admin authKey returns 200 with user list" {
                let! (_, adminKey) = seedAdmin ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.GetAsync($"/api/administration/user-payment/users{authQuery adminKey}")
                Expect.equal response.StatusCode HttpStatusCode.OK "should return 200"
                let! body = response.Content.ReadFromJsonAsync<{| id: string |} array>()
                Expect.isNonEmpty body "should return non-empty user list"
            }
        ]

        testList "postPayment" [
            testTask "missing authKey returns 400 InvalidAuthKey" {
                let! (userId, _) = seedRegularUser ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.PostAsync($"/api/administration/user-payment/{userId}", jsonContent """{"amount":5.0}""")
                Expect.equal response.StatusCode HttpStatusCode.BadRequest "should return 400"
                let! body = response.Content.ReadFromJsonAsync<string>()
                Expect.equal body "InvalidAuthKey" "missing key → InvalidAuthKey"
            }

            testTask "non-admin authKey returns 400 NotAuthorized" {
                let! (userId, _) = seedRegularUser ()
                let! (_, userKey) = seedRegularUser ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.PostAsync($"/api/administration/user-payment/{userId}{authQuery userKey}", jsonContent """{"amount":5.0}""")
                Expect.equal response.StatusCode HttpStatusCode.BadRequest "should return 400"
                let! body = response.Content.ReadFromJsonAsync<string>()
                Expect.equal body "NotAuthorized" "non-admin key → NotAuthorized"
            }

            testTask "admin authKey with valid amount returns 200 with new balance" {
                let! (userId, _) = seedRegularUser ()
                let! (_, adminKey) = seedAdmin ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.PostAsync($"/api/administration/user-payment/{userId}{authQuery adminKey}", jsonContent """{"amount":10.0}""")
                Expect.equal response.StatusCode HttpStatusCode.OK "should return 200"
                let! (body: string) = response.Content.ReadFromJsonAsync<string>()
                let balance = System.Decimal.Parse(body, System.Globalization.CultureInfo.InvariantCulture)
                Expect.equal balance 10.0m "balance should be 10.0 after payment"
            }

            testTask "admin authKey, user with existing order returns correct balance" {
                let! (userId, _) = seedRegularUser ()
                let! (_, adminKey) = seedAdmin ()
                do! seedOrder userId "TestBeer" 2 3.5m |> (fun (t: System.Threading.Tasks.Task<string>) -> t :> System.Threading.Tasks.Task)
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.PostAsync($"/api/administration/user-payment/{userId}{authQuery adminKey}", jsonContent """{"amount":20.0}""")
                Expect.equal response.StatusCode HttpStatusCode.OK "should return 200"
                let! (body2: string) = response.Content.ReadFromJsonAsync<string>()
                let balance = System.Decimal.Parse(body2, System.Globalization.CultureInfo.InvariantCulture)
                // balance = payment - orders = 20.0 - (2 * 3.5) = 20.0 - 7.0 = 13.0
                Expect.equal balance 13.0m "balance should account for existing orders"
            }
        ]
    ]
