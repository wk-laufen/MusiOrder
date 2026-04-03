module MusiOrder.Server.Tests.Program

open Expecto

let allTests =
    testList "MusiOrder.Server" [
        OrderTests.tests
        UserPaymentAdministrationTests.tests
        UserAdministrationTests.tests
        ProductAdministrationTests.tests
        OrderAdministrationTests.tests
        OrderStatisticsTests.tests
        DataExportTests.tests
    ]

[<EntryPoint>]
let main argv =
    let dbPath = Helpers.initTestDb ()
    System.Environment.SetEnvironmentVariable("DB_PATH", dbPath)
    try
        runTestsWithCLIArgs [] argv allTests
    finally
        if System.IO.File.Exists(dbPath) then
            System.IO.File.Delete(dbPath)
