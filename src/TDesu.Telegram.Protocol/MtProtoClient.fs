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
open TDesu.MTProto.Service
open TDesu.MTProto.Service.Requests
open TDesu.Serialization
open TDesu.Transport

/// Core MTProto client handling session, encryption, and RPC dispatch.
type MtProtoClient(dc: DataCenter, ?logger: ILogger, ?transportFactory: DataCenter -> ITransport) =

    // The carrier comes from the DataCenter the caller connected with (`dc.Transport`), so it is
    // chosen at connection setup and is rebuilt the same way on every reconnect. An explicit
    // factory still wins, for tests and for carriers this library does not know about.
    let createTransport = defaultArg transportFactory Transports.create

    let mutable transport = createTransport dc
    let dispatcher = RpcDispatcher()
    let updateEvent = Event<byte[]>()
    let reconnectedEvent = Event<unit>()
    let mutable authKey: AuthKey option = None
    let mutable session: SessionState option = None
    let mutable receiveLoopCts: CancellationTokenSource option = None
    let mutable isReconnecting = false
    let reconnectLock = obj ()
    // Set by Disconnect, cleared by a connect. A closed client answers RPCs with
    // ConnectionClosed at once: there is no reader left to complete them and no reconnect coming,
    // so waiting on either would stall the caller for nothing.
    let mutable closed = false

    // Cancelled only by Disconnect, and replaced by the next connect. A reconnect has to outlive
    // the receive loop that asked for it: stopping that loop is the reconnect's own first step, so
    // gating the attempts on the loop's token would abort every reconnect the reader itself
    // triggers — which is nearly all of them.
    let mutable lifetimeCts = new CancellationTokenSource()

    /// Stop the current receive loop and its keepalives, and forget them.
    ///
    /// Cancelled but deliberately not disposed: the loops being torn down still hold this token and
    /// hand it to the transport, and disposing it under them turns a clean cancellation into an
    /// ObjectDisposedException from inside someone's registration. A source with no timer and no
    /// live registrations is cheap to simply let go of.
    let stopReceiveLoops () =
        match receiveLoopCts with
        | Some old ->
            receiveLoopCts <- None
            old.Cancel()
        | None -> ()

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

    // Bounded replay guard: a replayed encrypted packet decrypts to the same msg_id, so each
    // inbound msg_id is processed at most once. The window is capped to bound memory.
    let seenMsgIds = System.Collections.Generic.HashSet<int64>()
    let seenMsgIdOrder = System.Collections.Generic.Queue<int64>()
    let seenMsgIdLock = obj ()

    let markSeen (msgId: int64) : bool =
        lock seenMsgIdLock (fun () ->
            if not (seenMsgIds.Add msgId) then
                false
            else
                seenMsgIdOrder.Enqueue msgId
                if seenMsgIdOrder.Count > 8192 then
                    seenMsgIds.Remove(seenMsgIdOrder.Dequeue()) |> ignore
                true)

    /// Whether this msg_id is still inside the replay window, i.e. we can say we received it.
    /// Outside the window the honest answer is "nothing known", not "never arrived".
    let wasSeen (msgId: int64) : bool =
        lock seenMsgIdLock (fun () -> seenMsgIds.Contains msgId)

    // The three constructors `mtproto.tl` comments out as "parsed manually", so td-tl-gen emits no
    // literal for them and there is nothing generated to match on. They are the frame's own
    // structure rather than payload types: the container and the gzip wrapper are unwrapped before
    // anything is dispatched, and rpc_result's `result:Object` has no static type to deserialize
    // into. Everything else in this dispatch matches a `GeneratedCid`.
    [<Literal>]
    let GzipPackedCid = 0x3072cfa1u

    [<Literal>]
    let MsgContainerCid = 0x73f1f8dcu

    [<Literal>]
    let RpcResultCid = 0xf35c6d01u

    /// Inflate the `packed_data:bytes` of a gzip_packed, with a ceiling.
    ///
    /// Split from `ungzip` so a caller holding a reader already positioned on the packed field can
    /// inflate without first slicing the enclosing frame out of the buffer — that slice is a
    /// full-size copy, and it is discarded the moment the packed bytes are read.
    let inflate (packed: byte[]) : byte[] =
        use input = new MemoryStream(packed)
        use gz = new GZipStream(input, CompressionMode.Decompress)

        // Grown once to a plausible size rather than doubled from nothing. A MemoryStream that
        // doubles into a multi-megabyte result allocates every intermediate size on the large object
        // heap, and this runs on a process that never restarts.
        use output = new MemoryStream(min (packed.Length * 4) FrameCodec.MaxFrameLength)

        // A frame is capped at 16 MiB on the wire, but gzip of structured TL data expands by
        // three orders of magnitude, and the unpacked body can itself be gzip_packed. Copying
        // without a ceiling turns one frame into gigabytes on a process that never restarts.
        let buffer = Array.zeroCreate<byte> 81920
        let mutable total = 0
        let mutable read = gz.Read(buffer, 0, buffer.Length)

        while read > 0 do
            total <- total + read

            if total > FrameCodec.MaxFrameLength then
                failwith "gzip_packed expands beyond the maximum frame size"

            output.Write(buffer, 0, read)
            read <- gz.Read(buffer, 0, buffer.Length)

        output.ToArray()

    /// Telegram wraps large results in gzip_packed#3072cfa1 packed_data:bytes — a gzip
    /// stream carrying the real TL object. Unwrap it so callers see the plain object.
    let ungzip (data: byte[]) : byte[] =
        if data.Length >= 4 && BitConverter.ToUInt32(data, 0) = GzipPackedCid then
            use reader = new TlReadBuffer(data)
            %reader.ReadConstructorId()
            inflate (reader.ReadBytes())
        else
            data

    /// Serialize a generated service type into a message body.
    ///
    /// These used to be hand-written: a literal constructor id, a literal vector id, a length and a
    /// loop, per message type. That is the byte-poking the generator exists to make unnecessary, and
    /// it is how a schema change becomes a silent wire bug instead of a compile error.
    let serializeService (write: TlWriteBuffer -> unit) : byte[] =
        use w = new TlWriteBuffer()
        write w
        w.ToArray()

    let buildMsgsAck (ids: int64[]) : byte[] =
        serializeService (fun w -> MsgsAck.Serialize(w, { MsgIds = ids }))

    let buildPing (pingId: int64) (disconnectDelay: int) : byte[] =
        serializeService (fun w ->
            PingDelayDisconnect.Serialize(
                w,
                { PingId = pingId
                  DisconnectDelay = disconnectDelay }
            ))

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


    /// Complete (or fail) the caller waiting on `reqMsgId` with the result that follows `offset`.
    ///
    /// `reader` is the caller's, already positioned at `offset` — on the gzip path the packed field
    /// is read straight from it. Slicing the tail out first is a full-size copy of the whole frame,
    /// thrown away as soon as the packed bytes are read, and packed is precisely what the large
    /// results arrive as. That copy is one of five the receive path already makes per message, on
    /// buffers big enough to land on the large object heap.
    let processRpcResult (reader: TlReadBuffer) (body: byte[]) (offset: int) (reqMsgId: int64) =
        let isPacked =
            body.Length - offset >= 4 && BitConverter.ToUInt32(body, offset) = GzipPackedCid

        let resultData =
            if isPacked then
                %reader.ReadConstructorId()
                inflate (reader.ReadBytes())
            else
                body[offset..]

        // An rpc_error arrives in the same slot as a successful result, so handing the
        // bytes straight to the caller reports failure as success. Every consumer then
        // fails deserializing an "unknown constructor id" instead of reading the error
        // the server actually sent, and no `Error(RpcError …)` match anywhere can fire —
        // which quietly disables flood-wait handling and 2FA detection alike.
        let isRpcError =
            resultData.Length >= 8
            && BitConverter.ToUInt32(resultData, 0) = Requests.RpcError.ConstructorId

        let completed =
            if isRpcError then
                use r = new TlReadBuffer(resultData)
                let err = Requests.RpcError.Deserialize r

                log.LogDebug(
                    "rpc_error {Code} {Message} for msg_id {MsgId}",
                    err.ErrorCode,
                    err.ErrorMessage,
                    reqMsgId
                )

                dispatcher.FailRequest(reqMsgId, MtProtoError.RpcError(err.ErrorCode, err.ErrorMessage))
            else
                dispatcher.CompleteRequest(reqMsgId, resultData)

        if not completed then
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

                    // The seqno is taken inside the guard, never before it. Rekey fails whenever the
                    // request is no longer pending — it already completed, timed out, or another
                    // service message re-sent it first — and a content seqno consumed by a message
                    // that is then never sent leaves a hole in the sequence. The server reads that
                    // hole as a lost message and stops accepting the session; nothing recovers it
                    // short of a new one. A skipped msg_id costs nothing by comparison: they only
                    // have to increase.
                    if dispatcher.Rekey(oldMsgId, newMsgId) then
                        let seqNo = Session.nextSeqNo sess true
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
            | GzipPackedCid ->
                // gzip_packed wrapping the whole message — decompress and re-dispatch (ack at leaf).
                processInnerMessage (ungzip body) msgId seqNo
            | MsgContainerCid ->
                // msg_container — a non-content wrapper; don't ack it, recurse into each inner
                // message so their service constructors are handled and content ones get acked.
                let count = reader.ReadInt32()

                // The spec caps a container at 1024 messages. Beyond that the count is garbage and
                // the reads below would walk off the buffer.
                if count < 0 || count > 1024 then
                    log.LogWarning("Dropping msg_container with implausible count {Count}", count)
                else
                    for _ in 1..count do
                        let innerMsgId = reader.ReadInt64()
                        let innerSeqNo = reader.ReadInt32()
                        let innerLength = reader.ReadInt32()

                        // The inner length is the container's own claim about itself; a bad one
                        // either allocates what it asks for or rewinds the cursor and re-reads the
                        // same bytes forever.
                        if innerLength < 0 || innerLength > body.Length then
                            failwith $"msg_container inner length {innerLength} does not fit the frame"

                        let innerBody = reader.ReadRawBytes(innerLength)

                        // The dedupe guard has to sit here, not only on the outer id: Telegram
                        // re-sends anything it has not seen acked, and a retransmission keeps its
                        // own msg_id but travels inside a *new* container with a new outer id. The
                        // outer check passes and the update would be applied a second time.
                        if markSeen innerMsgId then
                            processInnerMessage innerBody innerMsgId innerSeqNo
                        else
                            log.LogDebug("Dropping replayed inner msg_id {MsgId}", innerMsgId)
            | _ ->
                // Content-related messages carry an odd seqno and MUST be acked.
                if seqNo &&& 1 = 1 then
                    enqueueAck msgId

                match constructor with
                | RpcResultCid ->
                    // rpc_result: req_msg_id + result
                    let reqMsgId = reader.ReadInt64()
                    processRpcResult reader body 12 reqMsgId
                | GeneratedCid.BadServerSalt ->
                    use r = new TlReadBuffer(body)

                    match BadMsgNotification.Deserialize r with
                    | BadMsgNotification.BadServerSalt(badMsgId, _, _, newSalt) ->
                        session |> Option.iter (fun s -> s.Salt <- newSalt)
                        log.LogWarning("bad_server_salt for msg_id {MsgId}; updated salt and re-sending", badMsgId)
                        %Task.Run(Func<Task>(fun () -> resendRequest badMsgId))
                    | other -> log.LogWarning("bad_server_salt id decoded as {Other}", other)
                | GeneratedCid.NewSessionCreated ->
                    // The server threw the old session away. Everything we sent before first_msg_id
                    // was discarded with it, and any update it would have pushed in between is now
                    // a hole only the application can fill — so re-send the abandoned requests and
                    // tell listeners the stream has a gap. Treating this as "new salt" alone loses
                    // both, silently.
                    use r = new TlReadBuffer(body)
                    let created = NewSession.Deserialize r
                    let firstMsgId = created.FirstMsgId
                    session |> Option.iter (fun s -> s.Salt <- created.ServerSalt)

                    let abandoned = dispatcher.PendingIds |> List.filter (fun id -> id < firstMsgId)

                    log.LogWarning(
                        "new_session_created (first_msg_id={FirstMsgId}); re-sending {Count} abandoned request(s)",
                        firstMsgId,
                        abandoned.Length
                    )

                    for abandonedId in abandoned do
                        %Task.Run(Func<Task>(fun () -> resendRequest abandonedId))

                    // Same signal the reconnect path raises: whatever listens for "your view of the
                    // update stream may be incomplete" has to run now.
                    reconnectedEvent.Trigger()
                | GeneratedCid.BadMsgNotification ->
                    use r = new TlReadBuffer(body)

                    let badMsgId, errCode =
                        match BadMsgNotification.Deserialize r with
                        | BadMsgNotification.BadMsgNotification(badMsgId, _, errorCode) -> badMsgId, errorCode
                        | BadMsgNotification.BadServerSalt(badMsgId, _, errorCode, _) -> badMsgId, errorCode

                    match errCode with
                    | 16
                    | 17 ->
                        // Our clock disagrees with the server's beyond the accepted window, so
                        // every message we send is rejected until the offset is corrected. The
                        // notification's own msg_id carries the server's time in its high 32 bits.
                        let serverSeconds = int32 (msgId >>> 32)
                        let localSeconds = int32 (DateTimeOffset.UtcNow.ToUnixTimeSeconds())

                        session
                        |> Option.iter (fun s ->
                            s.TimeOffset <- serverSeconds - localSeconds
                            // The monotonic clamp would otherwise keep emitting ids from the old,
                            // wrong clock long after the offset is fixed.
                            s.LastMsgId <- 0L)

                        log.LogWarning(
                            "bad_msg_notification {ErrCode} for msg_id {MsgId}; time offset corrected to {Offset}s, re-sending",
                            errCode,
                            badMsgId,
                            serverSeconds - localSeconds
                        )

                        %Task.Run(Func<Task>(fun () -> resendRequest badMsgId))
                    | _ ->
                        log.LogWarning("bad_msg_notification {ErrCode} for msg_id {MsgId}", errCode, badMsgId)

                        %dispatcher.FailRequest(
                            badMsgId,
                            MtProtoError.InvalidResponse $"bad_msg_notification {errCode}"
                        )
                | GeneratedCid.Pong ->
                    // pong (reply to our keepalive ping) — nothing to correlate.
                    log.LogTrace("pong")
                | GeneratedCid.MsgsAck ->
                    // server-side msgs_ack — acknowledges our sends, nothing to do.
                    ()
                | GeneratedCid.MsgDetailedInfo
                | GeneratedCid.MsgNewDetailedInfo ->
                    // Both variants name an *answer* the server is holding for us, and both must be
                    // acked by that answer's id — not by their own. Until they are, the server keeps
                    // re-announcing the same answer: measured against a live account, 28 of these
                    // arrived in one hour, over and over for the same ids.
                    //
                    // Falling through to the `_` branch is worse than noise. These are transport
                    // bookkeeping, not API objects, so every one of them was handed to update
                    // subscribers, which then failed deserializing an `Updates` and logged a real
                    // error for a message that never carried an update — hiding actual parse
                    // failures among them.
                    use r = new TlReadBuffer(body)

                    let answerMsgId =
                        match MsgDetailedInfo.Deserialize r with
                        | MsgDetailedInfo.MsgDetailedInfo(_, answerMsgId, _, _) -> answerMsgId
                        | MsgDetailedInfo.MsgNewDetailedInfo(answerMsgId, _, _) -> answerMsgId

                    enqueueAck answerMsgId
                    log.LogTrace("msg_detailed_info: acking answer_msg_id {AnswerMsgId}", answerMsgId)
                // The rest of mtproto.tl. None of these is an API object, and until they were named
                // here every one of them fell through to `_` and was handed to update subscribers,
                // which then failed deserializing an `Updates` — a real logged error for a message
                // that never carried an update, drowning the genuine parse failures.
                //
                // Read with the generated types, matched on the generated ids. Both come from
                // `cached/mtproto.tl` through td-tl-gen, so a schema change is a compile error here
                // rather than a wire bug nobody sees.
                | GeneratedCid.MsgsStateReq ->
                    // The server is asking what became of messages it sent. Answering costs one
                    // service message and stops it re-announcing them.
                    use r = new TlReadBuffer(body)
                    let req = MsgsStateReq.Deserialize r

                    // One status byte per requested id, in the order asked: 4 is "received", which
                    // doubles as an acknowledgement, and 1 is "nothing known" — the honest answer
                    // for anything that has aged out of the replay window.
                    let info =
                        req.MsgIds
                        |> Array.map (fun id -> if wasSeen id then '\004' else '\001')
                        |> System.String

                    log.LogDebug("msgs_state_req for {Count} id(s); answering", req.MsgIds.Length)

                    %Task.Run(
                        Func<Task>(fun () ->
                            task {
                                let body =
                                    serializeService (fun w ->
                                        MsgsStateInfo.Serialize(w, { ReqMsgId = msgId; Info = info }))

                                match! sendServiceMessage body false CancellationToken.None with
                                | Ok _ -> ()
                                | Error e -> log.LogDebug("msgs_state_info send failed: {Error}", e)
                            })
                    )
                | GeneratedCid.MsgsStateInfo ->
                    // An answer to a msgs_state_req this client never sends.
                    log.LogTrace("msgs_state_info")
                | GeneratedCid.MsgsAllInfo -> log.LogTrace("msgs_all_info")
                | GeneratedCid.MsgResendReq ->
                    // The server never got these. Re-sending is the whole point of the message, and
                    // the dispatcher still holds the body of anything still pending.
                    use r = new TlReadBuffer(body)
                    let req = MsgResendReq.Deserialize r
                    log.LogWarning("msg_resend_req for {Count} message(s); re-sending", req.MsgIds.Length)

                    for id in req.MsgIds do
                        %Task.Run(Func<Task>(fun () -> resendRequest id))
                | GeneratedCid.RpcAnswerUnknown
                | GeneratedCid.RpcAnswerDroppedRunning
                | GeneratedCid.RpcAnswerDropped ->
                    // Replies to rpc_drop_answer, which this client never sends.
                    log.LogTrace("rpc_answer 0x{Constructor:x8}", constructor)
                | GeneratedCid.FutureSalts ->
                    // Only ever a reply to get_future_salts, unsent here.
                    log.LogTrace("future_salts")
                | GeneratedCid.DestroySessionOk
                | GeneratedCid.DestroySessionNone ->
                    log.LogDebug("destroy_session result 0x{Constructor:x8}", constructor)
                | GeneratedCid.HttpWait ->
                    // http_wait#9299359f — a client-to-server message; arriving here means a broken
                    // peer, but it is still transport bookkeeping and not an update.
                    log.LogDebug("Unexpected http_wait from the server")
                | _ ->
                    // Server push update (not RPC result, not a known service message).
                    log.LogDebug("Push update 0x{Constructor:x8}, msg_id={MsgId}", constructor, msgId)

                    // Subscribers run on the receive loop, so a throwing handler would otherwise
                    // reach the loop's catch-all, fail every in-flight RPC and force a reconnect.
                    try
                        updateEvent.Trigger(body)
                    with ex ->
                        log.LogError(ex, "Update subscriber threw for msg_id {MsgId}", msgId)

    /// Periodically flush accumulated msgs_ack. Shares the receive loop's CT so it dies with a
    /// disconnect/reconnect and is restarted alongside the new receive loop.
    let ackLoop (ct: CancellationToken) =
        task {
            try
                while not ct.IsCancellationRequested do
                    do! Tasks.Task.Delay(10000, ct)
                    let ids = ResizeArray<int64>()
                    let mutable id = 0L

                    // The spec caps msgs_ack at 8192 ids; a catch-up burst can exceed that inside
                    // one 10s window, so take at most a batch and leave the rest queued.
                    while ids.Count < 8192 && pendingAcks.TryDequeue(&id) do
                        ids.Add id

                    if ids.Count > 0 then
                        // Requeued on *any* failure to send, cancellation included. The ids are
                        // already out of the queue by this point, so a reconnect or a Disconnect
                        // landing mid-flush silently dropped a whole batch: the `Error` arm below
                        // covered a refused send, but a cancelled one throws straight past it to the
                        // handler at the bottom of the loop. Telegram retransmits anything it has
                        // not seen acked and eventually drops a session it judges unacked, so the
                        // batch has to survive the teardown that interrupted it.
                        let mutable delivered = false

                        try
                            match! sendServiceMessage (buildMsgsAck (ids.ToArray())) false ct with
                            | Ok _ -> delivered <- true
                            | Error e ->
                                log.LogDebug("msgs_ack send failed, {Count} ids requeued: {Error}", ids.Count, e)
                        finally
                            if not delivered then
                                for unsent in ids do
                                    enqueueAck unsent
            with
            | :? OperationCanceledException -> ()
            | ex -> log.LogDebug(ex, "ack loop ended")
        }

    /// How long the carrier tolerates silence. Telegram's WebSocket gateway closes a connection
    /// roughly 30s after the last frame — measured in production as a drop every 31-36s, each one
    /// costing a reconnect and a gap-recovery pass — so the 60s that suits raw TCP never arrives
    /// in time. HTTP is request/response and needs the same treatment for pushes to keep flowing.
    let pingIntervalMs =
        match dc.Transport with
        | TransportKind.WebSocket _
        | TransportKind.Http -> 20_000
        | _ -> 60_000

    /// Keepalive: ping the server before it — or anything between us — times out an idle
    /// connection. `disconnect_delay` tells the server to drop us if we go quiet for that long.
    /// Shares the receive loop's CT.
    let pingLoop (ct: CancellationToken) =
        task {
            try
                while not ct.IsCancellationRequested do
                    do! Tasks.Task.Delay(pingIntervalMs, ct)
                    let pingId = Session.newSessionId ()
                    let disconnectDelay = max 75 (pingIntervalMs / 1000 * 4)

                    match! sendServiceMessage (buildPing pingId disconnectDelay) false ct with
                    | Ok _ -> ()
                    | Error e -> log.LogDebug("ping send failed: {Error}", e)
            with
            | :? OperationCanceledException -> ()
            | ex -> log.LogDebug(ex, "ping loop ended")
        }

    /// One read, then a tail call for the next.
    ///
    /// Deliberately recursive rather than a `while` loop inside the handler in
    /// `receiveLoop`. An awaiting loop wrapped in `try/with` stops F# generating a
    /// resumable state machine, and the fallback it silently lands on blocks the
    /// calling thread on each await. That is merely wasteful on a thread pool and
    /// fatal in a browser, whose single thread cannot wait at all: `Monitor` raises
    /// "Cannot wait on monitors on this runtime" and the reader dies one message
    /// after the session is established.
    let rec receiveStep (ct: CancellationToken) : Task<unit> =
        task {
            if ct.IsCancellationRequested || not transport.IsConnected then
                return ()
            else
                match! transport.ReceiveAsync(ct) with
                | Error TransportError.ConnectionClosed ->
                    // A loop whose CT was already cancelled has been superseded by a newer
                    // generation; failing requests now would kill the ones belonging to the
                    // connection that replaced it.
                    if not ct.IsCancellationRequested then
                        log.LogWarning("Connection closed by server; reconnecting")
                        dispatcher.FailAll(MtProtoError.TransportError TransportError.ConnectionClosed)
                        do! reconnectInternal ()

                    return ()
                | Error TransportError.Timeout ->
                    // ReceiveAsync only yields Timeout when our own receive CT is cancelled
                    // (Disconnect / reconnect tearing this loop down). The guard at the top
                    // exits on the next step — nothing to recover.
                    return! receiveStep ct
                | Error e ->
                    // A desynced byte stream (InvalidFrame) or broken socket (ReadError /
                    // ConnectionFailed) leaves the read position unrecoverable: looping would
                    // spin on garbage until every in-flight RPC times out. Treat it like a
                    // dropped connection — fail pending requests and reconnect.
                    if not ct.IsCancellationRequested then
                        log.LogWarning("Receive error ({Error}); stream unrecoverable, reconnecting", e)
                        dispatcher.FailAll(MtProtoError.TransportError e)
                        do! reconnectInternal ()

                    return ()
                | Ok data ->
                    match authKey with
                    | Some key ->
                        match MessageFraming.decrypt key data with
                        | Ok(msgId, sessionId, seqNo, body) ->
                            match session with
                            | Some s when s.SessionId <> sessionId ->
                                log.LogWarning("Dropping message for foreign session_id {SessionId}", sessionId)
                            | _ ->
                                if markSeen msgId then
                                    // One unparseable message must cost that message, not the
                                    // connection: the handler in `receiveLoop` fails every
                                    // in-flight RPC and reconnects.
                                    try
                                        processInnerMessage body msgId seqNo
                                    with ex ->
                                        log.LogError(ex, "Failed to process msg_id {MsgId}", msgId)
                                else
                                    log.LogWarning("Dropping replayed msg_id {MsgId}", msgId)
                        | Error e -> log.LogError("Failed to decrypt message: {Error}", e)
                    | None ->
                        match UnencryptedMessage.deserialize data with
                        | Ok(msgId, body) -> %dispatcher.CompleteRequest(msgId, body)
                        | Error e -> log.LogError("Failed to parse unencrypted message: {Error}", e)

                    return! receiveStep ct
        }

    and receiveLoop (ct: CancellationToken) =
        task {
            try
                do! receiveStep ct
            with
            | :? OperationCanceledException -> ()
            | ex ->
                // An exception in message processing (malformed container, corrupt gzip, a throwing
                // update subscriber) would otherwise kill the reader while the socket stays
                // "connected" — every later RPC then times out forever. Reconnect instead.
                log.LogError(ex, "Receive loop error; reconnecting")
                dispatcher.FailAll(MtProtoError.TransportError TransportError.ConnectionClosed)
                do! reconnectInternal ()
        }

    and reconnectInternal () =
        task {
            let shouldReconnect =
                lock reconnectLock (fun () ->
                    if isReconnecting || closed then
                        false
                    else
                        isReconnecting <- true
                        true)

            if not shouldReconnect then
                ()
            else

                // Everything below runs under try/finally because the flag gates every future
                // reconnect *and* the wait inside RpcAsync. Leaving it set — a cancelled backoff
                // delay used to throw straight out of here — permanently convinces the client that
                // a reconnect is in flight: no attempt is ever made again and every later RPC
                // burns its reconnect wait before failing. That is a silent, unrecoverable death.
                try
                    // The reader that triggered this is still running, and it reads `transport`
                    // through the same mutable field this is about to overwrite. Stopping it first
                    // is the whole point: swapping underneath it pointed the old loop at the new
                    // socket, so for the rest of its life it raced the new reader for frames off a
                    // connection it knew nothing about — every frame it won was decrypted against
                    // the wrong generation's expectations and every one it lost was a reply the new
                    // reader never saw.
                    stopReceiveLoops ()

                    let ct = lifetimeCts.Token
                    let backoffs = [| 1000; 2000; 4000 |]
                    let mutable reconnected = false

                    for attempt in 0 .. backoffs.Length - 1 do
                        if not reconnected && not ct.IsCancellationRequested && not closed then
                            log.LogInformation(
                                "Reconnect attempt {Attempt} after {Delay}ms",
                                attempt + 1,
                                backoffs[attempt]
                            )

                            try
                                do! Tasks.Task.Delay(backoffs[attempt], ct)
                                transport.Disconnect()
                                transport <- createTransport dc

                                match! transport.ConnectAsync(ct) with
                                | Error e ->
                                    log.LogWarning("Reconnect attempt {Attempt} failed: {Error}", attempt + 1, e)
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
                                        match! AuthKeyExchange.performExchange transport dc.Id ct with
                                        | Error e ->
                                            log.LogWarning(
                                                "Auth key exchange failed on reconnect: {Error}",
                                                attempt + 1,
                                                e
                                            )
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

                    if not reconnected then
                        log.LogError("All reconnect attempts failed")
                finally
                    lock reconnectLock (fun () -> isReconnecting <- false)
        }

    /// Start a fresh receive loop plus the ack/ping keepalive loops on a new CT. Stops the previous
    /// generation first so a reconnect doesn't leave the old keepalive loops running (they'd pile up
    /// across reconnects, all writing to the now-shared transport).
    and spawnReceiveAndKeepalive () =
        stopReceiveLoops ()

        let cts = new CancellationTokenSource()
        receiveLoopCts <- Some cts
        %Task.Run(Func<Task>(fun () -> receiveLoop cts.Token))
        %Task.Run(Func<Task>(fun () -> ackLoop cts.Token))
        %Task.Run(Func<Task>(fun () -> pingLoop cts.Token))

    /// Set the auth key + a fresh session (carrying the given salt/time offset) and start the
    /// receive + keepalive loops. Shared by the fresh-DH connect and persisted-session restore,
    /// and the one place that revives a client an earlier Disconnect had closed.
    let startSession (key: AuthKey) (salt: int64) (timeOffset: int32) =
        closed <- false

        // A previous Disconnect cancelled the old one for good; without a replacement every
        // reconnect on the revived client would abort before its first backoff.
        if lifetimeCts.IsCancellationRequested then
            lifetimeCts <- new CancellationTokenSource()

        authKey <- Some key
        let sess = Session.createSession ()
        sess.Salt <- salt
        sess.TimeOffset <- timeOffset
        session <- Some sess

        // Both are per-session: acks name msg_ids the new session never saw, and the replay window
        // would judge new ids against ids from a session whose id no longer matches.
        pendingAcks.Clear()

        lock seenMsgIdLock (fun () ->
            seenMsgIds.Clear()
            seenMsgIdOrder.Clear())

        spawnReceiveAndKeepalive ()

    /// Connect to the DC and perform auth key exchange
    member _.ConnectAsync(ct: CancellationToken) : Task<Result<unit, MtProtoError>> =
        task {
            log.LogInformation("Connecting to DC{DcId} at {Address}:{Port}", dc.Id, dc.Address, dc.Port)

            match! transport.ConnectAsync(ct) with
            | Error e -> return Error(MtProtoError.TransportError e)
            | Ok() ->

                log.LogInformation("Connected, performing auth key exchange")

                match! AuthKeyExchange.performExchange transport dc.Id ct with
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
    ///
    /// If a reconnect is in flight the send waits for it, so the caller's retry hits a live
    /// connection. A client closed by Disconnect returns ConnectionClosed immediately instead:
    /// nothing will read a reply and no reconnect is coming, so every wait would be dead time.
    member _.RpcAsync(requestBody: byte[], ct: CancellationToken) =
        task {
            if closed then
                return Error(MtProtoError.TransportError TransportError.ConnectionClosed)
            else

            // Reconnects are otherwise only ever driven by the receive loop, and that loop is gone
            // once its attempts are exhausted (or once something cancelled them). An RPC arriving
            // on a client whose transport is down is the trigger that brings the connection back,
            // instead of every later call failing until the process is restarted. Deliberately not
            // tied to the caller's token: one caller's timeout must not abort a shared reconnect.
            if not transport.IsConnected && not isReconnecting then
                log.LogInformation("RpcAsync: transport is down, reconnecting before the send")
                do! reconnectInternal ()

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

            // Documented contract: failures arrive as Error Results. Reading the session and the
            // auth key through a helper that throws when either is missing would fault the task for
            // any caller that raced a Disconnect or issued an RPC before the first connect —
            // exactly the callers least likely to have a try around it. Two such helpers used to sit
            // at the top of this type for that job; this match is why they were never called.
            match session, authKey with
            | Option.None, _
            | _, Option.None -> return Error(MtProtoError.InvalidResponse "not connected")
            | Some sess, Some key ->

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
            match session with
            | Option.None -> return Error(MtProtoError.InvalidResponse "not connected")
            | Some sess ->
                // msg_id generation mutates shared session state and the write shares one socket:
                // without the lock two messages can take the same id and interleave on the wire.
                do! sendLock.WaitAsync(ct)

                try
                    let msgId = Session.generateMsgId sess
                    let unencrypted = UnencryptedMessage.serialize msgId body

                    match! transport.SendAsync(unencrypted, ct) with
                    | Error e -> return Error(MtProtoError.TransportError e)
                    | Ok() -> return Ok msgId
                finally
                    %sendLock.Release()
        }

    /// Event fired when a server push update is received (not an RPC response).
    [<CLIEvent>]
    member _.UpdateReceived = updateEvent.Publish

    /// Event fired after a successful automatic reconnection.
    [<CLIEvent>]
    member _.Reconnected = reconnectedEvent.Publish

    /// Disconnect and clean up. The client stays usable: a later connect revives it.
    member _.Disconnect() =
        closed <- true

        // Stops any reconnect already in flight as well as the reader: the attempts run on this
        // token precisely so that tearing the reader down cannot abort them, which means only this
        // says "stop for good". `startSession` issues a fresh one, so the client stays revivable.
        lifetimeCts.Cancel()
        stopReceiveLoops ()
        dispatcher.FailAll(MtProtoError.TransportError TransportError.ConnectionClosed)
        transport.Disconnect()
        log.LogInformation("Disconnected")

    interface IDisposable with
        member this.Dispose() = this.Disconnect()
