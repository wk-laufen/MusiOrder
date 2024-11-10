namespace MusiOrder.NfcReader.Controllers

open Microsoft.AspNetCore.Mvc
open Microsoft.Extensions.Logging
open PCSC
open PCSC.Exceptions
open PCSC.Extensions
open System
open System.Threading

type ICardReader =
    abstract member GetReaders: unit -> string array
    abstract member ReadCardId: readerName: string -> CancellationToken -> string option

type PcscCardReader() =
    let contextFactory = ContextFactory.Instance
    let context = contextFactory.Establish(SCardScope.System)

    let waitForCard (context: ISCardContext) readerName (ct: CancellationToken) =
        use readerState = new SCardReaderState(ReaderName = readerName, CurrentState = SCRState.Unaware)
        let rec fn () =
            let statusChangeResult =
                use _ = ct.Register(fun () -> context.Cancel().ThrowIfNotSuccess())
                context.GetStatusChange(context.Infinite, [| readerState |])
            if statusChangeResult = SCardError.Cancelled then None
            elif statusChangeResult = SCardError.Success then
                if readerState.EventState.CardIsPresent() then Some ()
                else
                    readerState.CurrentState <- readerState.EventState
                    readerState.CurrentStateValue <- readerState.EventStateValue
                    fn ()
            else fn ()
        fn ()

    interface ICardReader with
        member _.GetReaders () = context.GetReaders()

        member _.ReadCardId readerName ct = 
            match waitForCard context readerName ct with
            | None -> None
            | Some () ->
                try
                    use reader = context.ConnectReader(readerName, SCardShareMode.Shared, SCardProtocol.Any)
                    let mutable receiveBuffer = Array.zeroCreate<byte> 256
                    let receivedBytes = reader.Transmit([| 0xFFuy; 0xCAuy; 0x00uy; 0x00uy; 0x00uy |], receiveBuffer)
                    if receivedBytes >= 2 then
                        let result = receiveBuffer.[receivedBytes - 2..receivedBytes - 1]
                        if result = [| 0x90uy; 0x00uy |] then
                            let cardId = receiveBuffer |> Array.take (receivedBytes - 2) |> Convert.ToHexString
                            Some cardId
                        elif result = [| 0x63uy; 0x00uy |] then
                            failwith "Transmit error: The operation has failed."
                        elif result = [| 0x6Auy; 0x81uy |] then
                            failwith "Transmit error: Function not supported."
                        else
                            failwith "Transmit error: Unknown error."
                    else
                        failwith $"Transmit error: Received %d{receivedBytes} bytes."
                with
                | :? RemovedCardException -> None

    interface IDisposable with
        member _.Dispose () = context.Dispose()

type ConsoleCardReader() =
    interface ICardReader with
        member _.GetReaders () = [| "console" |]
        member _.ReadCardId readerName ct =
            printfn "1 -> 1234, 2 -> 1235, 3 -> None, 4 -> Error: "
            let rec fn () =
                if Console.KeyAvailable then
                    let keyInfo = Console.ReadKey()
                    if keyInfo.Key = ConsoleKey.D1 then Some "1234"
                    elif keyInfo.Key = ConsoleKey.D2 then Some "1235"
                    elif keyInfo.Key = ConsoleKey.D3 then None
                    elif keyInfo.Key = ConsoleKey.D4 then failwith $"Can't read from %s{readerName}"
                    else fn ()
                else
                    Thread.Sleep(100)
                    ct.ThrowIfCancellationRequested()
                    fn ()
            fn ()

[<ApiController>]
[<Route("/nfc-reader")>]
type NfcReaderController (cardReader: ICardReader, logger : ILogger<NfcReaderController>) =
    inherit ControllerBase()

    [<HttpGet("card-id")>]
    member this.GetCardId(ct: CancellationToken) =
        try
            let readerNames = cardReader.GetReaders()
            logger.LogInformation("Found {CountOfReaders} readers: {ReaderNames}", readerNames.Length, String.Join("\n", readerNames))
            match readerNames |> Array.tryHead with
            | None -> failwith "No readers found"
            | Some readerName ->
                match cardReader.ReadCardId readerName ct with
                | None -> this.BadRequest() :> IActionResult
                | Some cardId -> this.Ok(cardId)
        with :? OperationCanceledException -> this.NoContent()
