module DB

open FSharp.Control.Tasks.V2.ContextInsensitive
open Microsoft.Data.Sqlite

let dbPath =
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
