module MusiOrder.Server.Serialization

open System
open System.Globalization
open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open Giraffe.Serialization

type DecimalStringConverter() =
    inherit JsonConverter<decimal>()
    override _.Read(reader, _, _) =
        match reader.TokenType with
        | JsonTokenType.String -> Decimal.Parse(reader.GetString(), CultureInfo.InvariantCulture)
        | JsonTokenType.Number -> reader.GetDecimal()
        | _ -> failwith "Expected number or string for decimal value"
    override _.Write(writer, value, _) =
        writer.WriteStringValue(value.ToString(CultureInfo.InvariantCulture))

let defaultOptions =
    let opts = JsonSerializerOptions()
    opts.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
    opts.Converters.Add(DecimalStringConverter())
    opts

type SystemTextJsonSerializer() =
    interface IJsonSerializer with
        member _.SerializeToString(value: 'T) =
            JsonSerializer.Serialize(value, defaultOptions)
        member _.SerializeToBytes(value: 'T) =
            JsonSerializer.SerializeToUtf8Bytes(value, defaultOptions)
        member _.SerializeToStreamAsync(value: 'T) (stream: Stream) =
            JsonSerializer.SerializeAsync(stream, value, defaultOptions) :> System.Threading.Tasks.Task
        member _.Deserialize<'T>(json: string) =
            JsonSerializer.Deserialize<'T>(json, defaultOptions)
        member _.Deserialize<'T>(bytes: byte[]) =
            JsonSerializer.Deserialize<'T>(Text.Encoding.UTF8.GetString(bytes), defaultOptions)
        member _.DeserializeAsync<'T>(stream: Stream) =
            JsonSerializer.DeserializeAsync<'T>(stream, defaultOptions).AsTask()
