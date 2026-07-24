namespace TDesu.Transport

open System
open System.IO
open System.Net.WebSockets
open System.Threading
open TDesu.FSharp
open TDesu.FSharp.Operators

/// MTProto transport over WebSocket binary frames using the obfuscated
/// ("obfuscation2") intermediate protocol.
///
/// Telegram's WebSocket endpoints reject the plain intermediate framing that the
/// raw-TCP transport uses; they require the 64-byte obfuscation init followed by
/// AES-CTR-encrypted intermediate frames. The one-time 0xeeeeeeee header is not
/// sent separately — its tag lives inside the obfuscation init instead.
///
/// The DC is resolved to Telegram's web endpoint by id
/// (wss://<name>.web.telegram.org/apiws), falling back to venus (DC2).
type WsTransport(dc: DataCenter) =

    let endpoint =
        let name =
            match dc.Id with
            | 1 -> "pluto"
            | 2 -> "venus"
            | 3 -> "aurora"
            | 4 -> "vesta"
            | 5 -> "flora"
            | _ -> "venus"

        Uri($"wss://%s{name}.web.telegram.org/apiws")

    let mutable ws: ClientWebSocket option = None
    let mutable encryptor: Aes256Ctr option = None
    let mutable decryptor: Aes256Ctr option = None
    let mutable connected = false

    let getWs () =
        match ws with
        | Some s when s.State = WebSocketState.Open -> Ok s
        | _ -> Error TransportError.ConnectionClosed

    member _.IsConnected = connected

    member _.ConnectAsync(ct: CancellationToken) = task {
        try
            let client = new ClientWebSocket()
            client.Options.AddSubProtocol("binary")
            do! client.ConnectAsync(endpoint, ct)

            // Obfuscation handshake: send the init (last 8 bytes encrypted); every
            // subsequent binary message is CTR-encrypted intermediate framing.
            let obf = Obfuscation.create Obfuscation.IntermediateTag dc.Id
            do! client.SendAsync(ReadOnlyMemory obf.InitPacket, WebSocketMessageType.Binary, true, ct)

            ws <- Some client
            encryptor <- Some obf.Send
            decryptor <- Some obf.Recv
            connected <- true
            return Ok ()
        with
        | :? OperationCanceledException -> return Error TransportError.Timeout
        | ex -> return Error (TransportError.ConnectionFailed ex.Message)
    }

    member _.SendAsync(payload: byte[], ct: CancellationToken) = task {
        match getWs (), encryptor with
        | Error e, _ -> return Error e
        | _, None -> return Error TransportError.ConnectionClosed
        | Ok client, Some enc ->
            try
                // Intermediate frame (4-byte LE length + payload), CTR-encrypted,
                // carried as one binary WebSocket message.
                let frame = FrameCodec.encodeFrame payload
                let obfuscated = enc.Process frame
                do! client.SendAsync(ReadOnlyMemory obfuscated, WebSocketMessageType.Binary, true, ct)
                return Ok ()
            with
            | :? OperationCanceledException -> return Error TransportError.Timeout
            | ex -> return Error (TransportError.WriteError ex.Message)
    }

    member _.ReceiveAsync(ct: CancellationToken) = task {
        match getWs (), decryptor with
        | Error e, _ -> return Error e
        | _, None -> return Error TransportError.ConnectionClosed
        | Ok client, Some dec ->
            try
                // A binary WebSocket message may arrive across several frames;
                // accumulate until EndOfMessage, then decrypt the whole message.
                use buffer = new MemoryStream()
                let chunk = Array.zeroCreate<byte> (16 * 1024)
                let mutable endOfMessage = false
                let mutable closed = false

                while not endOfMessage && not closed do
                    let! result = client.ReceiveAsync(ArraySegment chunk, ct)

                    if result.MessageType = WebSocketMessageType.Close then
                        connected <- false
                        closed <- true
                    else
                        buffer.Write(chunk, 0, result.Count)
                        endOfMessage <- result.EndOfMessage

                if closed then
                    return Error TransportError.ConnectionClosed
                else
                    let data = dec.Process(buffer.ToArray())

                    match FrameCodec.decodeFrameLength data with
                    | Error e -> return Error e
                    | Ok length ->
                        if data.Length < 4 + length then
                            return Error (TransportError.InvalidFrame "Truncated WebSocket frame")
                        else
                            return Ok data[4 .. 4 + length - 1]
            with
            | :? OperationCanceledException -> return Error TransportError.Timeout
            | ex -> return Error (TransportError.ReadError ex.Message)
    }

    member _.Disconnect() =
        connected <- false
        encryptor |> Option.iter (fun e -> (e :> IDisposable).Dispose())
        decryptor |> Option.iter (fun d -> (d :> IDisposable).Dispose())
        ws |> Option.iter (fun s -> s.Dispose())
        encryptor <- None
        decryptor <- None
        ws <- None

    interface ITransport with
        member this.IsConnected = this.IsConnected
        member this.ConnectAsync(ct) = this.ConnectAsync(ct)
        member this.SendAsync(payload, ct) = this.SendAsync(payload, ct)
        member this.ReceiveAsync(ct) = this.ReceiveAsync(ct)
        member this.Disconnect() = this.Disconnect()

    interface IDisposable with
        member this.Dispose() = this.Disconnect()
