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

    // Decrypted bytes that have arrived but do not yet form a whole frame, plus whatever followed
    // the frame we last returned. The gateway relays an obfuscated byte stream, so a WebSocket
    // message is not a frame: it may carry two frames or half of one. Treating a message as a
    // frame silently dropped everything past the first one — lost RPC replies and lost updates,
    // with nothing at this level able to notice the hole.
    let mutable pending: byte[] = Array.empty

    let getWs () =
        match ws with
        | Some s when s.State = WebSocketState.Open -> Ok s
        | _ -> Error TransportError.ConnectionClosed

    /// A cancelled or failed read leaves the CTR keystream out of step with the peer, and a
    /// half-decrypted message would corrupt every frame after it. Retire the connection instead.
    let invalidate () =
        connected <- false
        pending <- Array.empty

    member _.IsConnected =
        connected
        && match ws with
           | Some s -> s.State = WebSocketState.Open
           | None -> false

    member _.ConnectAsync(ct: CancellationToken) = task {
        let client = new ClientWebSocket()
        client.Options.AddSubProtocol("binary")
        let obf = Obfuscation.create Obfuscation.IntermediateTag dc.Id

        try
            do! client.ConnectAsync(endpoint, ct)

            // Obfuscation handshake: send the init (last 8 bytes encrypted); every
            // subsequent binary message is CTR-encrypted intermediate framing.
            do! client.SendAsync(ReadOnlyMemory obf.InitPacket, WebSocketMessageType.Binary, true, ct)

            ws <- Some client
            encryptor <- Some obf.Send
            decryptor <- Some obf.Recv
            pending <- Array.empty
            connected <- true
            return Ok ()
        with ex ->
            // Nothing was published, so nothing else will ever dispose these: the socket and both
            // ciphers hold unmanaged handles and this path runs several times an hour.
            client.Dispose()
            (obf.Send :> IDisposable).Dispose()
            (obf.Recv :> IDisposable).Dispose()

            match ex with
            | :? OperationCanceledException -> return Error TransportError.Timeout
            | _ -> return Error(TransportError.ConnectionFailed ex.Message)
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
            with ex ->
                // The cipher already advanced by this frame. If the bytes did not reach the peer,
                // everything we send afterwards decrypts to garbage there, so the connection is
                // finished whatever the caller does next.
                invalidate ()

                match ex with
                | :? OperationCanceledException -> return Error TransportError.Timeout
                | _ -> return Error(TransportError.WriteError ex.Message)
    }

    member _.ReceiveAsync(ct: CancellationToken) = task {
        match getWs (), decryptor with
        | Error e, _ -> return Error e
        | _, None -> return Error TransportError.ConnectionClosed
        | Ok client, Some dec ->
            /// One frame out of `pending`, or None while it still holds less than a whole frame.
            let takeFrame () =
                if pending.Length < 4 then
                    Ok None
                else
                    match FrameCodec.decodeFrameLength pending with
                    | Error e -> Error e
                    | Ok length ->
                        if pending.Length < 4 + length then
                            Ok None
                        else
                            let frame = pending[4 .. 4 + length - 1]
                            pending <- pending[4 + length ..]
                            Ok(Some frame)

            try
                let chunk = Array.zeroCreate<byte> (16 * 1024)
                let mutable outcome = Option.None

                while outcome.IsNone do
                    match takeFrame () with
                    | Error e ->
                        // A length we cannot make sense of means the stream is off its rails.
                        invalidate ()
                        outcome <- Some(Error e)
                    | Ok(Some frame) -> outcome <- Some(Ok frame)
                    | Ok None ->
                        let! received = client.ReceiveAsync(ArraySegment chunk, ct)

                        if received.MessageType = WebSocketMessageType.Close then
                            invalidate ()
                            outcome <- Some(Error TransportError.ConnectionClosed)
                        elif received.Count > 0 then
                            // CTR is a stream cipher, so decrypting arrival-ordered chunks is the
                            // same as decrypting the whole message: message boundaries carry no
                            // meaning and are deliberately ignored.
                            let decrypted = dec.Process(chunk[0 .. received.Count - 1])
                            pending <- Array.append pending decrypted

                return outcome.Value
            with ex ->
                // Bytes were consumed from the socket and the keystream moved with them; resuming
                // on this connection would decrypt the next read at the wrong offset.
                invalidate ()

                match ex with
                | :? OperationCanceledException -> return Error TransportError.Timeout
                | _ -> return Error(TransportError.ReadError ex.Message)
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
