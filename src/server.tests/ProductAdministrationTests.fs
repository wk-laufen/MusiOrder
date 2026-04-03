module MusiOrder.Server.Tests.ProductAdministrationTests

open System.Net
open System.Net.Http
open System.Net.Http.Json
open Expecto
open AuthHandler
open Helpers

let tests =
    testList "ProductAdministration" [
        testList "getProducts" [
            testTask "missing authKey returns 400 InvalidAuthKey" {
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.GetAsync("/api/administration/product/products")
                Expect.equal response.StatusCode HttpStatusCode.BadRequest "should return 400"
                let! body = response.Content.ReadFromJsonAsync<string>()
                Expect.equal body "InvalidAuthKey" "should report InvalidAuthKey"
            }

            testTask "non-admin authKey returns 400 NotAuthorized" {
                let! (_, userKey) = seedRegularUser ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.GetAsync($"/api/administration/product/products{authQuery userKey}")
                Expect.equal response.StatusCode HttpStatusCode.BadRequest "should return 400"
                let! body = response.Content.ReadFromJsonAsync<string>()
                Expect.equal body "NotAuthorized" "non-admin should be NotAuthorized"
            }

            testTask "admin sees seeded group with product" {
                let! groupId = seedProductGroup "WineGroup"
                let! _ = seedProduct groupId "RedWine" 4.0m
                let! (_, adminKey) = seedAdmin ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.GetAsync($"/api/administration/product/products{authQuery adminKey}")
                Expect.equal response.StatusCode HttpStatusCode.OK "should return 200"
                let! (body: {| data: {| name: string |}; products: {| data: {| name: string |} |} array |} array) = response.Content.ReadFromJsonAsync<{| data: {| name: string |}; products: {| data: {| name: string |} |} array |} array>()
                Expect.exists body (fun g -> g.data.name = "WineGroup") "should contain the group name"
                let wines = body |> Array.find (fun g -> g.data.name = "WineGroup")
                Expect.exists wines.products (fun p -> p.data.name = "RedWine") "should contain the product name"
            }
        ]

        testList "postProductGroup" [
            testTask "missing authKey returns 400 InvalidAuthKey" {
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.PostAsync("/api/administration/product/groups", jsonContent """{"name":"Drinks"}""")
                Expect.equal response.StatusCode HttpStatusCode.BadRequest "should return 400"
                let! body = response.Content.ReadFromJsonAsync<string>()
                Expect.equal body "InvalidAuthKey" "should report InvalidAuthKey"
            }

            testTask "non-admin authKey returns 400 NotAuthorized" {
                let! (_, userKey) = seedRegularUser ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.PostAsync($"/api/administration/product/groups{authQuery userKey}", jsonContent """{"name":"Drinks"}""")
                Expect.equal response.StatusCode HttpStatusCode.BadRequest "should return 400"
                let! body = response.Content.ReadFromJsonAsync<string>()
                Expect.equal body "NotAuthorized" "non-admin should be NotAuthorized"
            }

            testTask "admin authKey with valid body returns 200 with new group id" {
                let! (_, adminKey) = seedAdmin ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.PostAsync($"/api/administration/product/groups{authQuery adminKey}", jsonContent """{"name":"TestDrinks"}""")
                Expect.equal response.StatusCode HttpStatusCode.OK "should return 200"
                let! body = response.Content.ReadFromJsonAsync<string>()
                Expect.isNotEmpty body "response should contain a group id"
            }
        ]

        testList "putProductGroup" [
            testTask "admin updates group name returns 200" {
                let! groupId = seedProductGroup "OriginalName"
                let! (_, adminKey) = seedAdmin ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.PutAsync($"/api/administration/product/groups/{groupId}{authQuery adminKey}", jsonContent """{"name":"UpdatedName"}""")
                Expect.equal response.StatusCode HttpStatusCode.OK "should return 200"
            }
        ]

        testList "moveUpProductGroup" [
            testTask "admin moves group up returns 200" {
                let! groupId1 = seedProductGroup "Group1"
                let! groupId2 = seedProductGroup "Group2"
                let! (_, adminKey) = seedAdmin ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.PostAsync($"/api/administration/product/groups/{groupId2}/move-up{authQuery adminKey}", jsonContent "")
                Expect.equal response.StatusCode HttpStatusCode.OK $"move-up group2 should return 200 (group1={groupId1})"
            }
        ]

        testList "moveDownProductGroup" [
            testTask "admin moves group down returns 200" {
                let! groupId1 = seedProductGroup "Group3"
                let! groupId2 = seedProductGroup "Group4"
                let! (_, adminKey) = seedAdmin ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.PostAsync($"/api/administration/product/groups/{groupId1}/move-down{authQuery adminKey}", jsonContent "")
                Expect.equal response.StatusCode HttpStatusCode.OK $"move-down group1 should return 200 (group2={groupId2})"
            }
        ]

        testList "deleteProductGroup" [
            testTask "group with products returns 400 GroupNotEmpty" {
                let! groupId = seedProductGroup "NonEmptyGroup"
                let! _ = seedProduct groupId "SomeProduct" 1.5m
                let! (_, adminKey) = seedAdmin ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.DeleteAsync($"/api/administration/product/groups/{groupId}{authQuery adminKey}")
                Expect.equal response.StatusCode HttpStatusCode.BadRequest "should return 400"
                let! body = response.Content.ReadFromJsonAsync<string>()
                Expect.equal body "GroupNotEmpty" "should report GroupNotEmpty"
            }

            testTask "empty group returns 200" {
                let! groupId = seedProductGroup "EmptyGroup"
                let! (_, adminKey) = seedAdmin ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.DeleteAsync($"/api/administration/product/groups/{groupId}{authQuery adminKey}")
                Expect.equal response.StatusCode HttpStatusCode.OK "should return 200"
            }

            testTask "after deleting all products, admin can delete group" {
                let! groupId = seedProductGroup "CleanupGroup"
                let! productId = seedProduct groupId "CleanupProduct" 1.0m
                let! (_, adminKey) = seedAdmin ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (deleteProductResponse: HttpResponseMessage) = client.DeleteAsync($"/api/administration/product/products/{productId}{authQuery adminKey}")
                Expect.equal deleteProductResponse.StatusCode HttpStatusCode.OK "delete product should return 200"
                let! (deleteGroupResponse: HttpResponseMessage) = client.DeleteAsync($"/api/administration/product/groups/{groupId}{authQuery adminKey}")
                Expect.equal deleteGroupResponse.StatusCode HttpStatusCode.OK "delete now-empty group should return 200"
            }
        ]

        testList "postProduct" [
            testTask "admin creates product in group returns 200 with product id" {
                let! groupId = seedProductGroup "BeerGroup"
                let! (_, adminKey) = seedAdmin ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.PostAsync($"/api/administration/product/groups/{groupId}/products{authQuery adminKey}", jsonContent """{"name":"Lager","price":2.5,"state":"Enabled"}""")
                Expect.equal response.StatusCode HttpStatusCode.OK "should return 200"
                let! body = response.Content.ReadFromJsonAsync<string>()
                Expect.isNotEmpty body "response should contain a product id"
            }
        ]

        testList "putProduct" [
            testTask "admin updates product returns 200" {
                let! groupId = seedProductGroup "UpdateGroup"
                let! productId = seedProduct groupId "OldName" 1.0m
                let! (_, adminKey) = seedAdmin ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.PutAsync($"/api/administration/product/products/{productId}{authQuery adminKey}", jsonContent """{"name":"NewName","price":3.0,"state":"Enabled"}""")
                Expect.equal response.StatusCode HttpStatusCode.OK "should return 200"
            }
        ]

        testList "moveUpProduct" [
            testTask "admin moves product up returns 200" {
                let! groupId = seedProductGroup "MoveGroup"
                let! productId1 = seedProduct groupId "Product1" 1.0m
                let! productId2 = seedProduct groupId "Product2" 2.0m
                let! (_, adminKey) = seedAdmin ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.PostAsync($"/api/administration/product/products/{productId2}/move-up{authQuery adminKey}", jsonContent "")
                Expect.equal response.StatusCode HttpStatusCode.OK $"move-up product2 should return 200 (product1={productId1})"
            }
        ]

        testList "moveDownProduct" [
            testTask "admin moves product down returns 200" {
                let! groupId = seedProductGroup "MoveGroup2"
                let! productId1 = seedProduct groupId "ProductA" 1.0m
                let! productId2 = seedProduct groupId "ProductB" 2.0m
                let! (_, adminKey) = seedAdmin ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.PostAsync($"/api/administration/product/products/{productId1}/move-down{authQuery adminKey}", jsonContent "")
                Expect.equal response.StatusCode HttpStatusCode.OK $"move-down product1 should return 200 (product2={productId2})"
            }
        ]

        testList "deleteProduct" [
            testTask "admin deletes product returns 200" {
                let! groupId = seedProductGroup "DeleteProductGroup"
                let! productId = seedProduct groupId "DeleteMe" 1.0m
                let! (_, adminKey) = seedAdmin ()
                let! (client: HttpClient) = createTestClient (AuthenticatedUsersAuthHandler())
                let! (response: HttpResponseMessage) = client.DeleteAsync($"/api/administration/product/products/{productId}{authQuery adminKey}")
                Expect.equal response.StatusCode HttpStatusCode.OK "should return 200"
            }
        ]
    ]
