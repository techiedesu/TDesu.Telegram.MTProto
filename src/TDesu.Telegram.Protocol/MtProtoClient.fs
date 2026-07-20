namespace TDesu.MTProto

open System
open System.IO
open System.IO.Compression
open System.Threading
open System.Threading.Tasks
open Microsoft.Extensions.Logging
open TDesu.FSharp
open TDesu.FSharp.Operators
open TDesu.MTProto.Auth
open TDesu.Serialization
open TDesu.Transport

/// Core MTProto client handling session, encryption, and RPC dispatch.
type MtProtoClient(dc: DataCenter, ?logger: ILogger) =

    let mutable transport = new TcpTransport(dc)
    let dispatcher = RpcDispatcher()
    let updateEvent = Event<byte[]>()
    let reconnectedEvent = Event<unit>()
    let mutable authKey: AuthKey option = None
    let mutable session: SessionState option = None
    let mutable receiveLoopCts: CancellationTokenSource option = None
    let mutable isReconnecting = false
    let reconnectLock = obj ()

    let log =
        defaultArg
            logger
            (Microsoft.Extensions.Logging.Abstractions.NullLogger.Instance :> Microsoft.Extensions.Logging.ILogger)

    // Serializes every write to the socket together with msg_id/seqno generation. A single
    // MtProtoClient is one transport with one shared, mutating SessionState; without this lock
    // concurrent senders (an RPC, a bad_server_salt re-send, a ping, an ack flush) interleave
    // their frame bytes on the wire and race the counters, desyncing the stream — the exact
    // failure the higher layers were working around.
    let sendLock = new SemaphoreSlim(1, 1)

    // msg_ids of received content-related (odd-seqno) messages awaiting an msgs_ack. Telegram
    // retransmits anything we don't acknowledge and eventually drops an all-unacked session.
    let pendingAcks = System.Collections.Concurrent.ConcurrentQueue<int64>()
    let enqueueAck (msgId: int64) = pendingAcks.Enqueue(msgId)

    let ensureSession () =
        match session with
        | Some s -> s
        | None -> failwith "Session not initialized"

    let ensureAuthKey () =
        match authKey with
        | Some k -> k
        | None -> failwith "Auth key not established"

    /// Telegram wraps large results in gzip_packed#3072cfa1 packed_data:bytes — a gzip
    /// stream carrying the real TL object. Unwrap it so callers see the plain object.
    let ungzip (data: byte[]) : byte[] =
        if data.Length >= 4 && BitConverter.ToUInt32(data, 0) = 0x3072cfa1u then
            use reader = new TlReadBuffer(data)
            %reader.ReadConstructorId()
            let packed = reader.ReadBytes()
            use input = new MemoryStream(packed)
            use gz = new GZipStream(input, CompressionMode.Decompress)
            use output = new MemoryStream()
            gz.CopyTo(output)
            output.ToArray()
        else
            data

    // msgs_ack#62d6b459 msg_ids:Vector<long> = MsgsAck
    let buildMsgsAck (ids: int64[]) : byte[] =
        use w = new TlWriteBuffer()
        w.WriteConstructorId(0x62d6b459u)
        w.WriteConstructorId(0x1cb5c415u) // vector
        w.WriteInt32(ids.Length)
        for id in ids do
            w.WriteInt64(id)
        w.ToArray()

    // ping_delay_disconnect#f3427b8c ping_id:long disconnect_delay:int = Pong
    let buildPing (pingId: int64) (disconnectDelay: int) : byte[] =
        use w = new TlWriteBuffer()
        w.WriteConstructorId(0xf3427b8cu)
        w.WriteInt64(pingId)
        w.WriteInt32(disconnectDelay)
        w.ToArray()

    /// Send an already-built TL body as an encrypted message under the send lock; returns its
    /// msg_id. Does NOT register for a response — for fire-and-forget service messages (ack, ping).
    let sendServiceMessage (body: byte[]) (contentRelated: bool) (ct: CancellationToken) : Task<Result<int64, MtProtoError>> =
        task {
            match authKey, session with
            | Some key, Some sess ->
                do! sendLock.WaitAsync(ct)

                try
                    let msgId = Session.generateMsgId sess
                    let seqNo = Session.nextSeqNo sess contentRelated
                    let encrypted = MessageFraming.encrypt key sess msgId seqNo body

                    match! transport.SendAsync(encrypted, ct) with
                    | Ok() -> return Ok msgId
                    | Error e -> return Error(MtProtoError.TransportError e)
                finally
                    %sendLock.Release()
            | _ -> return Error(MtProtoError.InvalidResponse "not connected")
        }

    let processRpcResult (body: byte[]) (offset: int) (reqMsgId: int64) =
        let resultData = ungzip body[offset..]

        if not (dispatcher.CompleteRequest(reqMsgId, resultData)) then
            log.LogWarning("No pending request for msg_id {MsgId}", reqMsgId)

    /// Re-send a still-pending request under a fresh msg_id (e.g. after bad_server_salt corrected
    /// the salt). The response to the re-send completes the original caller's task via Rekey.
    /// Runs under the send lock so its write can't interleave with another sender's.
    let resendRequest (oldMsgId: int64) =
        task {
            match dispatcher.TryGetBody(oldMsgId), authKey, session with
            | Some body, Some key, Some sess ->
                do! sendLock.WaitAsync(CancellationToken.None)

                try
                    let newMsgId = Session.generateMsgId sess
                    let seqNo = Session.nextSeqNo sess true

                    if dispatcher.Rekey(oldMsgId, newMsgId) then
                        let encrypted = MessageFraming.encrypt key sess newMsgId seqNo body

                        match! transport.SendAsync(encrypted, CancellationToken.None) with
                        | Ok() -> ()
                        | Error e -> %dispatcher.FailRequest(newMsgId, MtProtoError.TransportError e)
                finally
                    %sendLock.Release()
            | _ -> ()
        }

    /// Process one decrypted message (or a message nested in a container), acking content-related
    /// ones and routing service messages (bad_server_salt, new_session_created, bad_msg) the same
    /// way whether they arrive bare or wrapped in a msg_container.
    let rec processInnerMessage (body: byte[]) (msgId: int64) (seqNo: int32) =
        if body.Length >= 4 then
            use reader = new TlReadBuffer(body)
            let constructor = reader.ReadConstructorId()

            match constructor with
            | 0x3072cfa1u ->
                // gzip_packed wrapping the whole message — decompress and re-dispatch (ack at leaf).
                processInnerMessage (ungzip body) msgId seqNo
            | 0x73f1f8dcu ->
                // msg_container — a non-content wrapper; don't ack it, recurse into each inner
                // message so their service constructors are handled and content ones get acked.
                let count = reader.ReadInt32()

                for _ in 1..count do
                    let innerMsgId = reader.ReadInt64()
                    let innerSeqNo = reader.ReadInt32()
                    let innerLength = reader.ReadInt32()
                    let innerBody = reader.ReadRawBytes(innerLength)
                    processInnerMessage innerBody innerMsgId innerSeqNo
            | _ ->
                // Content-related messages carry an odd seqno and MUST be acked.
                if seqNo &&& 1 = 1 then
                    enqueueAck msgId

                match constructor with
                | 0xf35c6d01u ->
                    // rpc_result: req_msg_id + result
                    let reqMsgId = reader.ReadInt64()
                    processRpcResult body 12 reqMsgId
                | 0xedab447bu ->
                    // bad_server_salt: bad_msg_id:long bad_msg_seqno:int error_code:int new_server_salt:long
                    let badMsgId = reader.ReadInt64()
                    %reader.ReadInt32()
                    %reader.ReadInt32()
                    let newSalt = reader.ReadInt64()
                    session |> Option.iter (fun s -> s.Salt <- newSalt)
                    log.LogWarning("bad_server_salt for msg_id {MsgId}; updated salt and re-sending", badMsgId)
                    %Task.Run(Func<Task>(fun () -> resendRequest badMsgId))
                | 0x9ec20908u ->
                    // new_session_created: first_msg_id:long unique_id:long server_salt:long
                    %reader.ReadInt64()
                    %reader.ReadInt64()
                    let newSalt = reader.ReadInt64()
                    session |> Option.iter (fun s -> s.Salt <- newSalt)
                    log.LogInformation("new_session_created; server salt updated")
                | 0xa7eff811u ->
                    // bad_msg_notification: bad_msg_id:long bad_msg_seqno:int error_code:int
                    let badMsgId = reader.ReadInt64()
                    %reader.ReadInt32()
                    let errCode = reader.ReadInt32()
                    log.LogWarning("bad_msg_notification {ErrCode} for msg_id {MsgId}", errCode, badMsgId)
                    %dispatcher.FailRequest(badMsgId, MtProtoError.InvalidResponse $"bad_msg_notification {errCode}")
                | 0x347773c5u ->
                    // pong (reply to our keepalive ping) — nothing to correlate.
                    log.LogTrace("pong")
                | 0x62d6b459u ->
                    // server-side msgs_ack — acknowledges our sends, nothing to do.
                    ()
                | _ ->
                    // Server push update (not RPC result, not a known service message).
                    log.LogDebug("Push update 0x{Constructor:x8}, msg_id={MsgId}", constructor, msgId)
                    updateEvent.Trigger(body)

    /// Periodically flush accumulated msgs_ack. Shares the receive loop's CT so it dies with a
    /// disconnect/reconnect and is restarted alongside the new receive loop.
    let ackLoop (ct: CancellationToken) =
        task {
            try
                while not ct.IsCancellationRequested do
                    do! Tasks.Task.Delay(10000, ct)
                    let ids = ResizeArray<int64>()
                    let mutable id = 0L

                    while pendingAcks.TryDequeue(&id) do
                        ids.Add id

                    if ids.Count > 0 then
                        match! sendServiceMessage (buildMsgsAck (ids.ToArray())) false ct with
                        | Ok _ -> ()
                        | Error e -> log.LogDebug("msgs_ack send failed: {Error}", e)
            with
            | :? OperationCanceledException -> ()
            | ex -> log.LogDebug(ex, "ack loop ended")
        }

    /// Keepalive: ping the server before it times out an idle connection (disconnect_delay = 75s,
    /// pinged every 60s). Shares the receive loop's CT.
    let pingLoop (ct: CancellationToken) =
        task {
            try
                while not ct.IsCancellationRequested do
                    do! Tasks.Task.Delay(60000, ct)
                    let pingId = Session.newSessionId ()

                    match! sendServiceMessage (buildPing pingId 75) false ct with
                    | Ok _ -> ()
                    | Error e -> log.LogDebug("ping send failed: {Error}", e)
            with
            | :? OperationCanceledException -> ()
            | ex -> log.LogDebug(ex, "ping loop ended")
        }

    let rec receiveLoop (ct: CancellationToken) =
        task {
            try
                while not ct.IsCancellationRequested && transport.IsConnected do
                    match! transport.ReceiveAsync(ct) with
                    | Error TransportError.ConnectionClosed ->
                        log.LogWarning("Connection closed by server; reconnecting")
                        dispatcher.FailAll(MtProtoError.TransportError TransportError.ConnectionClosed)
                        do! reconnectInternal ct
                        return ()
                    | Error TransportError.Timeout ->
                        // ReceiveAsync only yields Timeout when our own receive CT is cancelled
                        // (Disconnect / reconnect tearing this loop down). The while-guard exits on
                        // the next check — nothing to recover.
                        ()
                    | Error e ->
                        // A desynced byte stream (InvalidFrame) or broken socket (ReadError /
                        // ConnectionFailed) leaves the read position unrecoverable: looping would
                        // spin on garbage until every in-flight RPC times out. Treat it like a
                        // dropped connection — fail pending requests and reconnect.
                        log.LogWarning("Receive error ({Error}); stream unrecoverable, reconnecting", e)
                        dispatcher.FailAll(MtProtoError.TransportError e)
                        do! reconnectInternal ct
                        return ()
                    | Ok data ->
                        match authKey with
                        | Some key ->
                            match MessageFraming.decrypt key data with
                            | Ok(msgId, _sessionId, seqNo, body) -> processInnerMessage body msgId seqNo
                            | Error e -> log.LogError("Failed to decrypt message: {Error}", e)
                        | None ->
                            match UnencryptedMessage.deserialize data with
                            | Ok(msgId, body) -> %dispatcher.CompleteRequest(msgId, body)
                            | Error e -> log.LogError("Failed to parse unencrypted message: {Error}", e)
            with
            | :? OperationCanceledException -> ()
            | ex ->
                // An exception in message processing (malformed container, corrupt gzip, a throwing
                // update subscriber) would otherwise kill the reader while the socket stays
                // "connected" — every later RPC then times out forever. Reconnect instead.
                log.LogError(ex, "Receive loop error; reconnecting")
                dispatcher.FailAll(MtProtoError.TransportError TransportError.ConnectionClosed)
                do! reconnectInternal ct
        }

    and reconnectInternal (ct: CancellationToken) =
        task {
            let shouldReconnect =
                lock reconnectLock (fun () ->
                    if isReconnecting then
                        false
                    else
                        isReconnecting <- true
                        true)

            if not shouldReconnect then
                ()
            else

                let backoffs = [| 1000; 2000; 4000 |]
                let mutable reconnected = false

                for attempt in 0 .. backoffs.Length - 1 do
                    if not reconnected && not ct.IsCancellationRequested then
                        log.LogInformation("Reconnect attempt {Attempt} after {Delay}ms", attempt + 1, backoffs[attempt])
                        do! Tasks.Task.Delay(backoffs[attempt], ct)

                        try
                            transport.Disconnect()
                            transport <- new TcpTransport(dc)

                            match! transport.ConnectAsync(ct) with
                            | Error e -> log.LogWarning("Reconnect attempt {Attempt} failed: {Error}", attempt + 1, e)
                            | Ok() ->
                                match authKey with
                                | Some _ ->
                                    // The auth key is permanent per DC — reuse it instead of re-running
                                    // DH so a restored/persisted session keeps working.
                                    spawnReceiveAndKeepalive ()
                                    reconnected <- true
                                    log.LogInformation("Reconnected (reused auth key)")
                                    reconnectedEvent.Trigger()
                                | None ->
                                    match! AuthKeyExchange.performExchange transport ct with
                                    | Error e ->
                                        log.LogWarning("Auth key exchange failed on reconnect: {Error}", attempt + 1, e)
                                    | Ok(key, salt, timeOffset) ->
                                        authKey <- Some key
                                        let sess = Session.createSession ()
                                        sess.Salt <- salt
                                        sess.TimeOffset <- timeOffset
                                        session <- Some sess
                                        spawnReceiveAndKeepalive ()
                                        reconnected <- true
                                        log.LogInformation("Reconnected successfully")
                                        reconnectedEvent.Trigger()
                        with
                        | :? OperationCanceledException -> ()
                        | ex -> log.LogWarning(ex, "Reconnect attempt {Attempt} error", attempt + 1)

                lock reconnectLock (fun () -> isReconnecting <- false)

                if not reconnected then
                    log.LogError("All reconnect attempts failed")
        }

    /// Start a fresh receive loop plus the ack/ping keepalive loops on a new CT. Cancels the
    /// previous CT first so a reconnect doesn't leave the old keepalive loops running (they'd
    /// pile up across reconnects, all writing to the now-shared transport).
    and spawnReceiveAndKeepalive () =
        receiveLoopCts
        |> Option.iter (fun old ->
            old.Cancel()
            old.Dispose())

        let cts = new CancellationTokenSource()
        receiveLoopCts <- Some cts
        %Task.Run(Func<Task>(fun () -> receiveLoop cts.Token))
        %Task.Run(Func<Task>(fun () -> ackLoop cts.Token))
        %Task.Run(Func<Task>(fun () -> pingLoop cts.Token))

    /// Set the auth key + a fresh session (carrying the given salt/time offset) and start the
    /// receive + keepalive loops. Shared by the fresh-DH connect and persisted-session restore.
    let startSession (key: AuthKey) (salt: int64) (timeOffset: int32) =
        authKey <- Some key
        let sess = Session.createSession ()
        sess.Salt <- salt
        sess.TimeOffset <- timeOffset
        session <- Some sess
        spawnReceiveAndKeepalive ()

    /// Connect to the DC and perform auth key exchange
    member _.ConnectAsync(ct: CancellationToken) : Task<Result<unit, MtProtoError>> =
        task {
            log.LogInformation("Connecting to DC{DcId} at {Address}:{Port}", dc.Id, dc.Address, dc.Port)

            match! transport.ConnectAsync(ct) with
            | Error e -> return Error(MtProtoError.TransportError e)
            | Ok() ->

                log.LogInformation("Connected, performing auth key exchange")

                match! AuthKeyExchange.performExchange transport ct with
                | Error e -> return Error e
                | Ok(key, salt, timeOffset) ->

                    startSession key salt timeOffset
                    log.LogInformation("Auth key established, session created")
                    return Ok()
        }

    /// Connect to the DC reusing a previously established auth key (skips the DH exchange).
    /// Use after restoring a persisted session so you don't have to re-login.
    member _.ConnectWithAuthKeyAsync
        (key: AuthKey, salt: int64, timeOffset: int32, ct: CancellationToken)
        : Task<Result<unit, MtProtoError>> =
        task {
            log.LogInformation(
                "Connecting to DC{DcId} at {Address}:{Port} with persisted auth key",
                dc.Id,
                dc.Address,
                dc.Port
            )

            match! transport.ConnectAsync(ct) with
            | Error e -> return Error(MtProtoError.TransportError e)
            | Ok() ->

                startSession key salt timeOffset
                log.LogInformation("Connected with persisted auth key (no DH)")
                return Ok()
        }

    /// Export the established auth key + server salt + time offset for persistence.
    /// Returns None if not connected/authorized yet.
    member _.ExportSession() : (AuthKey * int64 * int32) option =
        match authKey, session with
        | Some key, Some sess -> Some(key, sess.Salt, sess.TimeOffset)
        | _ -> None

    /// Send an RPC request and await the response. The send (msg_id/seqno generation + the socket
    /// write) runs under the send lock so it can't interleave with another sender; the response is
    /// awaited outside the lock. Failures arrive as Error Results, never thrown.
    /// Send an RPC request and await the response. If the transport is disconnected or the send
    /// fails, waits for an in-progress reconnect to complete before returning the error — so the
    /// caller's retry hits a live connection instead of immediately failing again.
    member _.RpcAsync(requestBody: byte[], ct: CancellationToken) =
        task {
            // If a reconnect is in progress, wait for it before even trying to send.
            if isReconnecting then
                log.LogDebug("RpcAsync: reconnect in progress, waiting...")
                let tcs = TaskCompletionSource<unit>()
                let handler = Handler<unit>(fun _ () -> tcs.TrySetResult() |> ignore)
                reconnectedEvent.Publish.AddHandler(handler)
                try
                    use cts = CancellationTokenSource.CreateLinkedTokenSource(ct)
                    cts.CancelAfter(TimeSpan.FromSeconds(15.0))
                    do! tcs.Task.WaitAsync(cts.Token)
                with _ -> ()
                reconnectedEvent.Publish.RemoveHandler(handler)

            let sess = ensureSession ()
            let key = ensureAuthKey ()

            do! sendLock.WaitAsync(ct)

            let mutable sent: Result<int64 * Task<Result<byte[], MtProtoError>>, MtProtoError> option =
                None

            try
                let msgId = Session.generateMsgId sess
                let seqNo = Session.nextSeqNo sess true
                let encrypted = MessageFraming.encrypt key sess msgId seqNo requestBody
                let responseTask = dispatcher.RegisterRequest(msgId, requestBody)

                match! transport.SendAsync(encrypted, ct) with
                | Ok() -> sent <- Some(Ok(msgId, responseTask))
                | Error e ->
                    %dispatcher.FailRequest(msgId, MtProtoError.TransportError e)
                    sent <- Some(Error(MtProtoError.TransportError e))
            finally
                %sendLock.Release()

            match sent with
            | None -> return Error(MtProtoError.InvalidResponse "send aborted")
            | Some(Error(MtProtoError.TransportError TransportError.ConnectionClosed)) ->
                // Send failed because the socket died. The receive loop will trigger reconnectInternal.
                // Wait briefly for the reconnect to finish so the caller's retry has a live connection.
                if isReconnecting then
                    let tcs = TaskCompletionSource<unit>()
                    let handler = Handler<unit>(fun _ () -> tcs.TrySetResult() |> ignore)
                    reconnectedEvent.Publish.AddHandler(handler)
                    try
                        use cts = CancellationTokenSource.CreateLinkedTokenSource(ct)
                        cts.CancelAfter(TimeSpan.FromSeconds(15.0))
                        do! tcs.Task.WaitAsync(cts.Token)
                        log.LogInformation("RpcAsync: reconnect completed, caller should retry")
                    with _ ->
                        log.LogWarning("RpcAsync: reconnect wait timed out")
                    reconnectedEvent.Publish.RemoveHandler(handler)
                return Error(MtProtoError.TransportError TransportError.ConnectionClosed)
            | Some(Error e) -> return Error e
            | Some(Ok(msgId, responseTask)) ->
                try
                    use cts = CancellationTokenSource.CreateLinkedTokenSource(ct)
                    cts.CancelAfter(TimeSpan.FromSeconds(30.0))
                    return! responseTask.WaitAsync(cts.Token)
                with :? OperationCanceledException ->
                    %dispatcher.FailRequest(msgId, MtProtoError.Timeout)
                    return Error MtProtoError.Timeout
        }

    /// Send an unencrypted message (for pre-auth operations)
    member _.SendUnencryptedAsync(body: byte[], ct: CancellationToken) =
        task {
            let sess = ensureSession ()
            let msgId = Session.generateMsgId sess
            let unencrypted = UnencryptedMessage.serialize msgId body

            match! transport.SendAsync(unencrypted, ct) with
            | Error e -> return Error(MtProtoError.TransportError e)
            | Ok() -> return Ok msgId
        }

    /// Event fired when a server push update is received (not an RPC response).
    [<CLIEvent>]
    member _.UpdateReceived = updateEvent.Publish

    /// Event fired after a successful automatic reconnection.
    [<CLIEvent>]
    member _.Reconnected = reconnectedEvent.Publish

    /// Disconnect and clean up
    member _.Disconnect() =
        receiveLoopCts
        |> Option.iter (fun cts ->
            cts.Cancel()
            cts.Dispose())

        receiveLoopCts <- None
        dispatcher.FailAll(MtProtoError.TransportError TransportError.ConnectionClosed)
        transport.Disconnect()
        log.LogInformation("Disconnected")

    interface IDisposable with
        member this.Dispose() = this.Disconnect()
