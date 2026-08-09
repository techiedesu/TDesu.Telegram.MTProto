namespace TDesu.Transport

open System
open System.IO
open System.Net.WebSockets
open System.Threading
open System.Threading.Tasks
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
/// `endpoint` overrides where to dial. Left as `None`, the DC id resolves Telegram's web
/// gateway (wss://&lt;name&gt;.web.telegram.org/apiws, falling back to venus/DC2) — `dc.Address`
/// cannot serve here because a WebSocket needs a URL, not an IP and port, and a self-hosted
/// deployment picks its own scheme, host and path.
type WsTransport(dc: DataCenter, endpoint: Uri option) =

    let endpoint =
        match endpoint with
        | Some uri -> uri
        | None ->
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

    new(dc: DataCenter) = new WsTransport(dc, None)

    /// Where this transport dials — Telegram's gateway unless one was supplied.
    member _.Endpoint = endpoint

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

    /// One frame out of `pending`, or None while it still holds less than a whole frame.
    member private _.TakeFrame() =
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

    /// Reads until `pending` holds a whole frame, then yields it.
    ///
    /// Recursive rather than a `while` loop with a `let mutable` result. A mutable
    /// local carried across an `await` inside a loop stops F# generating a resumable
    /// state machine, and the fallback it silently lands on blocks the calling thread
    /// waiting on the awaited task. Blocking is merely wasteful on a desktop runtime
    /// and fatal in a browser, whose single thread cannot wait at all: `Monitor` raises
    /// "Cannot wait on monitors on this runtime" and the handshake dies mid-flight.
    ///
    /// The recursion does not grow the stack — each step returns into an awaited
    /// continuation rather than nesting a call.
    member private this.PumpAsync
        (client: ClientWebSocket, dec: Aes256Ctr, ct: CancellationToken)
        : Task<Result<byte[], TransportError>> =
        task {
            match this.TakeFrame() with
            | Error e ->
                // A length we cannot make sense of means the stream is off its rails.
                invalidate ()
                return Error e
            | Ok(Some frame) -> return Ok frame
            | Ok None ->
                let chunk = Array.zeroCreate<byte> (16 * 1024)
                let! received = client.ReceiveAsync(Memory chunk, ct)

                if received.MessageType = WebSocketMessageType.Close then
                    invalidate ()
                    return Error TransportError.ConnectionClosed
                else
                    if received.Count > 0 then
                        // CTR is a stream cipher, so decrypting arrival-ordered chunks is the
                        // same as decrypting the whole message: message boundaries carry no
                        // meaning and are deliberately ignored.
                        pending <- Array.append pending (dec.Process(chunk[0 .. received.Count - 1]))
                    else
                        // A read that delivered nothing and did not close is not progress. On a
                        // thread-pool runtime the next await would hand the thread back anyway;
                        // on a cooperative single-threaded one it would not, and recursing
                        // straight back into a synchronously-completing receive pegs the only
                        // thread there is — the socket then never gets a chance to deliver, so
                        // the loop spins until the connection times out.
                        do! Task.Yield()

                    return! this.PumpAsync(client, dec, ct)
        }

    member this.ReceiveAsync(ct: CancellationToken) =
        task {
            match getWs (), decryptor with
            | Error e, _ -> return Error e
            | _, None -> return Error TransportError.ConnectionClosed
            | Ok client, Some dec ->
                try
                    return! this.PumpAsync(client, dec, ct)
                with ex ->
                    // Bytes were consumed from the socket and the keystream moved with them;
                    // resuming on this connection would decrypt the next read at the wrong offset.
                    invalidate ()

                    match ex with
                    | :? OperationCanceledException -> return Error TransportError.Timeout
                    // The type name is carried too: a bare `Message` reads
                    // "Cannot wait on monitors on this runtime" with no clue that it is a
                    // platform limitation rather than a peer or protocol fault.
                    | _ -> return Error(TransportError.ReadError $"{ex.GetType().Name}: {ex.Message}")
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
