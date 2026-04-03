module MusiOrder.Server.Tests.UserAdministrationTests

open System.Net
open System.Net.Http
open System.Net.Http.Json
open System.Text.Json
open Expecto
open AuthHandler
open Helpers

let tests =
    testList "UserAdministration" [
        testList "getUsers" [
            testTask "missing authKey returns 400 InvalidAuthKey" {
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.GetAsync("/api/administration/user/users")
                Expect.equal response.StatusCode HttpStatusCode.BadRequest "should return 400"
                let! body = response.Content.ReadFromJsonAsync<string>()
                Expect.equal body "InvalidAuthKey" "should report InvalidAuthKey"
            }

            testTask "non-admin authKey returns 400 NotAuthorized" {
                let! (_, userKey) = seedRegularUser ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.GetAsync($"/api/administration/user/users{authQuery userKey}")
                Expect.equal response.StatusCode HttpStatusCode.BadRequest "should return 400"
                let! body = response.Content.ReadFromJsonAsync<string>()
                Expect.equal body "NotAuthorized" "non-admin should be NotAuthorized"
            }

            testTask "admin authKey returns 200 with user list" {
                let! (adminId, adminKey) = seedAdmin ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.GetAsync($"/api/administration/user/users{authQuery adminKey}")
                Expect.equal response.StatusCode HttpStatusCode.OK "should return 200"
                let! (body: {| id: string |} array) = response.Content.ReadFromJsonAsync<{| id: string |} array>()
                Expect.exists body (fun u -> u.id = adminId) "response should contain seeded admin id"
            }
        ]

        testList "postUser" [
            testTask "missing authKey returns 400 InvalidAuthKey" {
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.PostAsync("/api/administration/user/users", jsonContent "{}")
                Expect.equal response.StatusCode HttpStatusCode.BadRequest "should return 400"
                let! body = response.Content.ReadFromJsonAsync<string>()
                Expect.equal body "InvalidAuthKey" "should report InvalidAuthKey"
            }

            testTask "non-admin authKey returns 400 NotAuthorized" {
                let! (_, userKey) = seedRegularUser ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.PostAsync($"/api/administration/user/users{authQuery userKey}", jsonContent "{}")
                Expect.equal response.StatusCode HttpStatusCode.BadRequest "should return 400"
                let! body = response.Content.ReadFromJsonAsync<string>()
                Expect.equal body "NotAuthorized" "non-admin should be NotAuthorized"
            }

            testTask "admin authKey with valid body returns 200 with new user id" {
                let! (_, adminKey) = seedAdmin ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let body = """{"firstName":"Alice","lastName":"Smith","authKeys":[],"role":"User"}"""
                let! (response: HttpResponseMessage) = client.PostAsync($"/api/administration/user/users{authQuery adminKey}", jsonContent body)
                Expect.equal response.StatusCode HttpStatusCode.OK "should return 200"
                let! responseBody = response.Content.ReadFromJsonAsync<string>()
                Expect.isNotEmpty responseBody "response body should contain a user id"
            }

            testTask "duplicate auth key returns 400 KeyCodeTaken" {
                let! (_, adminKey) = seedAdmin ()
                let! (_, existingKeyStr: string) = seedRegularUser ()
                let existingKeyCode = existingKeyStr.Substring("nfc/".Length)
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let body = sprintf """{"firstName":"Bob","lastName":"Test","authKeys":[{"keyType":"nfc","keyCode":"%s"}],"role":"User"}""" existingKeyCode
                let! (response: HttpResponseMessage) = client.PostAsync($"/api/administration/user/users{authQuery adminKey}", jsonContent body)
                Expect.equal response.StatusCode HttpStatusCode.BadRequest "should return 400"
                let! (responseBody: JsonElement array) = response.Content.ReadFromJsonAsync<JsonElement array>()
                Expect.equal (responseBody.[0].GetString()) "KeyCodeTaken" "should report KeyCodeTaken"
            }
        ]

        testList "putUser" [
            testTask "missing authKey returns 400 InvalidAuthKey" {
                let! (userId, _) = seedRegularUser ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.PutAsync($"/api/administration/user/users/{userId}", jsonContent "{}")
                Expect.equal response.StatusCode HttpStatusCode.BadRequest "should return 400"
                let! body = response.Content.ReadFromJsonAsync<string>()
                Expect.equal body "InvalidAuthKey" "should report InvalidAuthKey"
            }

            testTask "admin downgrading self returns 400 DowngradeSelfNotAllowed" {
                let! (adminId, adminKey) = seedAdmin ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let body = """{"addAuthKeys":[],"removeAuthKeys":[],"role":"User"}"""
                let! (response: HttpResponseMessage) = client.PutAsync($"/api/administration/user/users/{adminId}{authQuery adminKey}", jsonContent body)
                Expect.equal response.StatusCode HttpStatusCode.BadRequest "should return 400"
                let! responseBody = response.Content.ReadFromJsonAsync<string>()
                Expect.equal responseBody "DowngradeSelfNotAllowed" "self-downgrade should be rejected"
            }

            testTask "admin removing own active auth key returns 400 RemoveActiveAuthKeyNotAllowed" {
                let! (adminId: string, adminKey: string) = seedAdmin ()
                let keyCode = adminKey.Substring("nfc/".Length)
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let body = sprintf """{"addAuthKeys":[],"removeAuthKeys":[{"keyType":"nfc","keyCode":"%s"}]}""" keyCode
                let! (response: HttpResponseMessage) = client.PutAsync($"/api/administration/user/users/{adminId}{authQuery adminKey}", jsonContent body)
                Expect.equal response.StatusCode HttpStatusCode.BadRequest "should return 400"
                let! responseBody = response.Content.ReadFromJsonAsync<string>()
                Expect.equal responseBody "RemoveActiveAuthKeyNotAllowed" "removing own active key should be rejected"
            }

            testTask "admin updating another user's name returns 200" {
                let! (userId, _) = seedRegularUser ()
                let! (_, adminKey) = seedAdmin ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let body = """{"firstName":"UpdatedName","addAuthKeys":[],"removeAuthKeys":[]}"""
                let! (response: HttpResponseMessage) = client.PutAsync($"/api/administration/user/users/{userId}{authQuery adminKey}", jsonContent body)
                Expect.equal response.StatusCode HttpStatusCode.OK "should return 200"
            }
        ]

        testList "deleteUser" [
            testTask "missing authKey returns 400 InvalidAuthKey" {
                let! (userId, _) = seedRegularUser ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let request = new HttpRequestMessage(HttpMethod.Delete, $"/api/administration/user/users/{userId}")
                let! (response: HttpResponseMessage) = client.SendAsync(request)
                Expect.equal response.StatusCode HttpStatusCode.BadRequest "should return 400"
                let! body = response.Content.ReadFromJsonAsync<string>()
                Expect.equal body "InvalidAuthKey" "should report InvalidAuthKey"
            }

            testTask "without force: user with auth key returns 200 with AuthKeyPresent warning" {
                let! (userId, _) = seedRegularUser ()
                let! (_, adminKey) = seedAdmin ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.DeleteAsync($"/api/administration/user/users/{userId}{authQuery adminKey}")
                Expect.equal response.StatusCode HttpStatusCode.OK "should return 200"
                let! body = response.Content.ReadFromJsonAsync<string array>()
                Expect.contains body "AuthKeyPresent" "should warn about existing auth key"
            }

            testTask "without force: user with non-zero balance returns 200 with CurrentBalanceNotZero warning" {
                let! (userId, _) = seedRegularUser ()
                let! (_, adminKey) = seedAdmin ()
                do! seedPayment userId 5.0m |> (fun (t: System.Threading.Tasks.Task<string>) -> t :> System.Threading.Tasks.Task)
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.DeleteAsync($"/api/administration/user/users/{userId}{authQuery adminKey}")
                Expect.equal response.StatusCode HttpStatusCode.OK "should return 200"
                let! (body: JsonElement array) = response.Content.ReadFromJsonAsync<JsonElement array>()
                let currentBalanceWarning = body |> Array.find (fun e -> e.ValueKind = JsonValueKind.Array)
                let warningName = currentBalanceWarning.EnumerateArray() |> Seq.head |> (fun e -> e.GetString())
                Expect.equal warningName "CurrentBalanceNotZero" "should warn about non-zero balance"
            }

            testTask "with force: admin deletes user successfully" {
                let! (userId, _) = seedRegularUser ()
                let! (_, adminKey) = seedAdmin ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let encodedKey = System.Uri.EscapeDataString(adminKey: string)
                let url = $"/api/administration/user/users/{userId}?force=true&authKey={encodedKey}"
                let! (response: HttpResponseMessage) = client.DeleteAsync(url)
                Expect.equal response.StatusCode HttpStatusCode.OK "should return 200"
            }
        ]
    ]
