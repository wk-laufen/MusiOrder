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
}

let getUser (AuthKey authKey) =
    readSingle "SELECT `id`, `firstName`, `lastName` FROM `Member` WHERE `keyCode` = @KeyCode" [ ("@KeyCode", authKey) ] (fun reader -> { Id = reader.GetString(0); FirstName = reader.GetString(1); LastName = reader.GetString(2) })

let readIndexed query parameters readRow = task {
    let! list = read query parameters readRow
    return Map.ofList list
}

let write connection query parameters = task {
    let command = createCommand connection query parameters
    let! result = command.ExecuteNonQueryAsync()
    return ()
}

let writeMany query parameterLists = task {
    use connection = createConnection()
    use! tx = connection.BeginTransactionAsync()
    for parameters in parameterLists do
        do! write connection query parameters
    do! tx.CommitAsync()
}

