open Microsoft.Data.Sqlite
open System
open System.IO

[<EntryPoint>]
let main argv =
    use connection = new SqliteConnection("Data Source=data\\musiorder.db")
    connection.Open()

    let scriptContent = File.ReadAllText "db-schema.sql"
    let command = connection.CreateCommand(CommandText = scriptContent)
    command.ExecuteNonQuery() |> ignore

    let runWithIdParam query parameters =
        let cmd = connection.CreateCommand(CommandText = query)
        let rowId = sprintf "%O" (Guid.NewGuid())
        cmd.Parameters.AddWithValue("@Id", rowId) |> ignore
        parameters
        |> List.iter (fun (key, value) -> cmd.Parameters.AddWithValue(key, value) |> ignore)
        cmd.ExecuteNonQuery() |> ignore
        rowId

    let user1Id = runWithIdParam "INSERT INTO Member (`id`, `firstName`, `lastName`, `keyCode`, `role`) VALUES (@Id, 'Robin', 'Cella', '1234', 'admin')" []
    let user2Id = runWithIdParam "INSERT INTO Member (`id`, `firstName`, `lastName`, `keyCode`, `role`) VALUES (@Id, 'Sebastian', 'Manz', 'qwer', 'user')" []
    runWithIdParam "INSERT INTO Member (`id`, `firstName`, `lastName`, `keyCode`, `role`) VALUES (@Id, 'Thomas', 'Morgenstern', 'asdf', 'user')" [] |> ignore
    runWithIdParam "INSERT INTO Member (`id`, `firstName`, `lastName`, `keyCode`, `role`) VALUES (@Id, 'Matheo', 'Goss', 'zxcv', 'user')" [] |> ignore
    runWithIdParam "INSERT INTO Member (`id`, `firstName`, `lastName`, `keyCode`, `role`) VALUES (@Id, 'Dominik', 'Haspel', 'wert', 'user')" [] |> ignore

    let drinksId = runWithIdParam "INSERT INTO ArticleGroup (`id`, `grade`, `name`) VALUES (@Id, 1, 'Getränke')" []
    let mealsId = runWithIdParam "INSERT INTO ArticleGroup (`id`, `grade`, `name`) VALUES (@Id, 2, 'Speisen')" []

    runWithIdParam "INSERT INTO Article (`id`, `groupId`, `state`, `grade`, `name`, `price`) VALUES (@Id, @GroupId, 'enabled', 1, 'Bier', 2.5)" [ ("GroupId", drinksId) ] |> ignore
    runWithIdParam "INSERT INTO Article (`id`, `groupId`, `state`, `grade`, `name`, `price`) VALUES (@Id, @GroupId, 'enabled', 2, 'Wasser', 20)" [ ("GroupId", drinksId) ] |> ignore
    runWithIdParam "INSERT INTO Article (`id`, `groupId`, `state`, `grade`, `name`, `price`) VALUES (@Id, @GroupId, 'enabled', 3, 'Saft', 10)" [ ("GroupId", drinksId) ] |> ignore
    runWithIdParam "INSERT INTO Article (`id`, `groupId`, `state`, `grade`, `name`, `price`) VALUES (@Id, @GroupId, 'enabled', 4, 'Wein', 3)" [ ("GroupId", drinksId) ] |> ignore
    runWithIdParam "INSERT INTO Article (`id`, `groupId`, `state`, `grade`, `name`, `price`) VALUES (@Id, @GroupId, 'disabled', 5, 'alter Wein', 2.3)" [ ("GroupId", drinksId) ] |> ignore

    runWithIdParam "INSERT INTO Article (`id`, `groupId`, `state`, `grade`, `name`, `price`) VALUES (@Id, @GroupId, 'enabled', 1, 'Käseburger', 5.2)" [ ("GroupId", mealsId) ] |> ignore
    runWithIdParam "INSERT INTO Article (`id`, `groupId`, `state`, `grade`, `name`, `price`) VALUES (@Id, @GroupId, 'enabled', 2, 'Schinkenburger', 4.7)" [ ("GroupId", mealsId) ] |> ignore

    runWithIdParam "INSERT INTO MemberPayment (`id`, `userId`, `amount`, `timestamp`) VALUES (@Id, @UserId, 10, '2020-07-04 13:55:20.0263132+02:00')" [ ("UserId", user1Id) ] |> ignore
    runWithIdParam "INSERT INTO MemberPayment (`id`, `userId`, `amount`, `timestamp`) VALUES (@Id, @UserId, 2.5, '2020-06-04 19:55:20.0263132+02:00')" [ ("UserId", user1Id) ] |> ignore
    runWithIdParam "INSERT INTO MemberPayment (`id`, `userId`, `amount`, `timestamp`) VALUES (@Id, @UserId, 10, '2020-07-04 13:55:20.0263132+02:00')" [ ("UserId", user2Id) ] |> ignore

    0
