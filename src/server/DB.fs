module DB

open FSharp.Control.Tasks.V2.ContextInsensitive
open Microsoft.Data.Sqlite
open MusiOrder.Models

let private dbPath =
    match System.Environment.GetEnvironmentVariable "DB_PATH" |> Option.ofObj with
    | Some dbPath -> dbPath
    | None -> failwith "Environment variable \"DB_PATH\" not set."

let createConnection () =
    let connection = new SqliteConnection(sprintf "Data Source=%s" dbPath)
    connection.Open()
    connection

let createCommand (connection: SqliteConnection) query parameters =
    let command = connection.CreateCommand(CommandText = query)
    parameters
    |> List.iter (fun (key, value) -> command.Parameters.AddWithValue(key, value) |> ignore)
    command


let read query parameters readRow = task {
    use connection = createConnection()
    let command = createCommand connection query parameters
    use! reader = command.ExecuteReaderAsync()
    let rec readRows acc = task {
        let! hasNext = reader.ReadAsync()
        if hasNext then
            let row = readRow reader
            return! readRows (row :: acc)
        else
            return List.rev acc
    }
    return! readRows []
}

let readSingle query parameters readRow = task {
    let! list = read query parameters readRow
    return List.tryExactlyOne list
}

type User = {
    Id: string
    FirstName: string
    LastName: string
    Role: string
}

let getUser (AuthKey authKey) =
    readSingle "SELECT `id`, `firstName`, `lastName`, `role` FROM `Member` WHERE `keyCode` = @KeyCode" [ ("@KeyCode", authKey) ] (fun reader -> { Id = reader.GetString(0); FirstName = reader.GetString(1); LastName = reader.GetString(2); Role = reader.GetString(3) })

let getUserBalance (userId: string) = task {
    let! totalOrderPrice = readSingle "SELECT coalesce(sum(`amount` * `pricePerUnit`), 0) as `price` FROM `Order` WHERE userId = @UserId" [ ("@UserId", userId) ] (fun reader -> reader.GetDecimal(0))
    let! totalBalance = readSingle "SELECT coalesce(sum(`amount`), 0) FROM `MemberPayment` WHERE userId = @UserId" [ ("@UserId", userId) ] (fun reader -> reader.GetDecimal(0))
    return (Option.defaultValue 0m totalBalance) - (Option.defaultValue 0m totalOrderPrice)
}

let readIndexed query parameters readRow = task {
    let! list = read query parameters readRow
    return Map.ofList list
}

let private writeInternal connection query parameters = task {
    let command = createCommand connection query parameters
    let! result = command.ExecuteNonQueryAsync()
    return ()
}

let write query parameters = task {
    use connection = createConnection()
    do! writeInternal connection query parameters
}

let writeMany query parameterLists = task {
    use connection = createConnection()
    use! tx = connection.BeginTransactionAsync()
    for parameters in parameterLists do
        do! writeInternal connection query parameters
    do! tx.CommitAsync()
}

let tryGet (reader: SqliteDataReader) fn index =
    if not <| reader.IsDBNull index then fn index |> Some
    else None
