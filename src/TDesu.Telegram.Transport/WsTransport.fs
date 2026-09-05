namespace TDesu.Transport

open System
open System.IO
open System.Net.WebSockets
open System.Threading
open System.Threading.Tasks
open TDesu.FSharp
open TDesu.FSharp.Operators
open TDesu.FSharp.Tasks

/// Decrypted bytes that have arrived but not yet been handed out as frames: one array with a read
/// offset, compacted when a chunk would not fit and doubled only when it still would not.
///
/// This replaces `pending <- Array.append pending chunk`, which copied the whole accumulated
/// buffer on every 16 KiB read: a 512 KiB frame cost about 8.4 MB of memcpy in appends alone and a
/// 16 MiB frame about 8.4 GB, all of it churning the large object heap. Appending now copies the
/// chunk once, and taking a frame copies the frame once.
type internal ReceiveBuffer() =
    let mutable buffer = Array.zeroCreate<byte> (64 * 1024)
    let mutable start = 0
    let mutable count = 0

    member _.Count = count

    member _.Clear() =
        start <- 0
        count <- 0

    member _.Append(chunk: ReadOnlySpan<byte>) =
        if start + count + chunk.Length > buffer.Length then
            if start > 0 then
                Buffer.BlockCopy(buffer, start, buffer, 0, count)
                start <- 0

            if count + chunk.Length > buffer.Length then
                let bigger = Array.zeroCreate<byte> (max (buffer.Length * 2) (count + chunk.Length))
                Buffer.BlockCopy(buffer, 0, bigger, 0, count)
                buffer <- bigger

        chunk.CopyTo(Span(buffer, start + count, chunk.Length))
        count <- count + chunk.Length

    /// One frame, or None while the buffer holds less than a whole one.
    member _.TryTakeFrame() : Result<byte[] option, TransportError> =
        if count < 4 then
            Ok None
        else
            match FrameCodec.decodeFrameLengthAt buffer start with
            | Error e -> Error e
            | Ok length ->
                if count < 4 + length then
                    Ok None
                else
                    let frame = Array.zeroCreate<byte> length
                    Buffer.BlockCopy(buffer, start + 4, frame, 0, length)
                    start <- start + 4 + length
                    count <- count - 4 - length

                    if count = 0 then
                        start <- 0

                    Ok(Some frame)

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
    let pending = ReceiveBuffer()

    // One read's worth of ciphertext, reused across reads: decrypted in place and appended, so a
    // read allocates nothing but the frame it completes.
    let chunk = Array.zeroCreate<byte> (16 * 1024)

    let getWs () =
        match ws with
        | Some s when s.State = WebSocketState.Open -> Ok s
        | _ -> Error TransportError.ConnectionClosed

    /// A cancelled or failed read leaves the CTR keystream out of step with the peer, and a
    /// half-decrypted message would corrupt every frame after it. Retire the connection instead.
    let invalidate () =
        connected <- false
        pending.Clear()

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
            pending.Clear()
            connected <- true
            return Ok ()
        with ex ->
            // Nothing was published, so nothing else will ever dispose these: the socket and both
            // ciphers hold unmanaged handles and this path runs several times an hour.
            client.Dispose()
            (obf.Send :> IDisposable).Dispose()
            (obf.Recv :> IDisposable).Dispose()

            match ex with
            | :? OperationCanceledException -> return Error TransportError.Cancelled
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
                | :? OperationCanceledException -> return Error TransportError.Cancelled
                | _ -> return Error(TransportError.WriteError ex.Message)
    }

    /// Reads until `pending` holds a whole frame, then yields it.
    ///
    /// A `Task.loop` rather than a recursive `return! this.PumpAsync`: that recursion was not a
    /// tail call — every step stayed registered as the continuation of the next, one state
    /// machine per read for the life of the connection — and it allocated a fresh 16 KiB chunk
    /// per level. The loop's body is a plain task-returning function, awaited from one `while`
    /// inside the library, which is also what keeps it off the calling thread on a runtime whose
    /// single thread cannot block.
    member private _.PumpAsync
        (client: ClientWebSocket, dec: Aes256Ctr, ct: CancellationToken)
        : Task<Result<byte[], TransportError>> =
        Task.loop
            (fun () ->
                task {
                    match pending.TryTakeFrame() with
                    | Error e ->
                        // A length we cannot make sense of means the stream is off its rails.
                        invalidate ()
                        return Loop.Stop(Error e)
                    | Ok(Some frame) -> return Loop.Stop(Ok frame)
                    | Ok None ->
                        let! received = client.ReceiveAsync(Memory chunk, ct)

                        if received.MessageType = WebSocketMessageType.Close then
                            invalidate ()
                            return Loop.Stop(Error TransportError.ConnectionClosed)
                        else
                            if received.Count > 0 then
                                // CTR is a stream cipher, so decrypting arrival-ordered chunks is
                                // the same as decrypting the whole message: message boundaries
                                // carry no meaning and are deliberately ignored.
                                let span = Span(chunk, 0, received.Count)
                                dec.ProcessInPlace span
                                pending.Append(Span.op_Implicit span)
                            else
                                // A read that delivered nothing and did not close is not progress.
                                // On a thread-pool runtime the next await would hand the thread
                                // back anyway; on a cooperative single-threaded one it would not,
                                // and looping straight back into a synchronously-completing
                                // receive pegs the only thread there is — the socket then never
                                // gets a chance to deliver, so the loop spins until the connection
                                // times out.
                                do! Task.Yield()

                            return Loop.Continue()
                })
            ()

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
                    | :? OperationCanceledException -> return Error TransportError.Cancelled
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
