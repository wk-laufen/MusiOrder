namespace MusiOrder.NfcReader.Controllers

open Microsoft.AspNetCore.Mvc
open Microsoft.Extensions.Logging
open PCSC
open PCSC.Exceptions
open PCSC.Extensions
open System
open System.Threading

[<ApiController>]
[<Route("/nfc-reader")>]
type NfcReaderController (logger : ILogger<NfcReaderController>) =
    inherit ControllerBase()

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

    [<HttpGet("card-id")>]
    member this.GetCardId(ct: CancellationToken) =
        let contextFactory = ContextFactory.Instance
        use context = contextFactory.Establish(SCardScope.System)
        let readerNames = context.GetReaders()
        logger.LogInformation("Found {CountOfReaders} readers: {ReaderNames}", readerNames.Length, String.Join("\n", readerNames))
        match readerNames |> Array.tryHead with
        | None -> failwith "No readers found"
        | Some readerName ->
            match waitForCard context readerName ct with
            | None ->
                logger.LogInformation("Request cancelled")
                this.NoContent() :> IActionResult
            | Some () ->
                try
                    use reader = context.ConnectReader(readerName, SCardShareMode.Shared, SCardProtocol.Any)
                    let mutable receiveBuffer = Array.zeroCreate<byte> 256
                    let receivedBytes = reader.Transmit([| 0xFFuy; 0xCAuy; 0x00uy; 0x00uy; 0x00uy |], receiveBuffer)
                    if receivedBytes >= 2 then
                        let result = receiveBuffer.[receivedBytes - 2..receivedBytes - 1]
                        if result = [| 0x90uy; 0x00uy |] then
                            let cardId = receiveBuffer |> Array.take (receivedBytes - 2) |> Convert.ToHexString
                            logger.LogInformation("Received card id {CardId}", cardId)
                            this.Ok(cardId)
                        elif result = [| 0x63uy; 0x00uy |] then
                            failwith "Transmit error: The operation has failed."
                        elif result = [| 0x6Auy; 0x81uy |] then
                            failwith "Transmit error: Function not supported."
                        else
                            failwith "Transmit error: Unknown error."
                    else
                        failwith $"Transmit error: Received %d{receivedBytes} bytes."
                with
                | :? RemovedCardException ->
                    logger.LogInformation("Removed card")
                    this.BadRequest({| Error = "RemovedCard" |})
