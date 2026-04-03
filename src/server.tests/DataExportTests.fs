module MusiOrder.Server.Tests.DataExportTests

open System.Net
open System.Net.Http
open System.Net.Http.Json
open Expecto
open AuthHandler
open Helpers

let tests =
    testList "DataExport" [
        testList "exportDatabase" [
            testTask "missing authKey returns 400 InvalidAuthKey" {
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.GetAsync("/api/administration/data-export/export-db")
                Expect.equal response.StatusCode HttpStatusCode.BadRequest "should return 400"
                let! body = response.Content.ReadFromJsonAsync<string>()
                Expect.equal body "InvalidAuthKey" "should report InvalidAuthKey"
            }

            testTask "non-admin authKey returns 400 NotAuthorized" {
                let! (_, userKey) = seedRegularUser ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.GetAsync($"/api/administration/data-export/export-db{authQuery userKey}")
                Expect.equal response.StatusCode HttpStatusCode.BadRequest "should return 400"
                let! body = response.Content.ReadFromJsonAsync<string>()
                Expect.equal body "NotAuthorized" "non-admin should be NotAuthorized"
            }

            testTask "admin authKey returns 200 with valid SQLite database content" {
                let! (_, adminKey) = seedAdmin ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.GetAsync($"/api/administration/data-export/export-db{authQuery adminKey}")
                Expect.equal response.StatusCode HttpStatusCode.OK "should return 200"
                let! (bytes: byte[]) = response.Content.ReadAsByteArrayAsync()
                Expect.isGreaterThan bytes.Length 0 "exported database should be non-empty"
                // SQLite files start with the magic header string "SQLite format 3"
                let header = System.Text.Encoding.ASCII.GetString(bytes, 0, 16)
                Expect.stringContains header "SQLite format 3" "exported file should be a valid SQLite database"
            }
        ]
    ]
