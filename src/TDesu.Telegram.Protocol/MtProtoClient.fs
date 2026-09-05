namespace TDesu.MTProto

open System
open System.Buffers.Binary
open System.IO
open System.IO.Compression
open System.Threading
open System.Threading.Tasks
open Microsoft.Extensions.Logging
open TDesu.FSharp
open TDesu.FSharp.Operators
open TDesu.FSharp.Tasks
open TDesu.MTProto.Auth
open TDesu.MTProto.Service
open TDesu.MTProto.Service.Requests
open TDesu.Serialization
open TDesu.Transport

/// Core MTProto client handling session, encryption, and RPC dispatch.
///
/// `responseTimeout` bounds how long `RpcAsync` waits for the server's answer once the request is
/// on the wire (default 30 s); `TimeSpan.Zero` or a negative value means "only the caller's
/// token". Until 0.13 the 30 s were hard-coded and silently overrode a caller that asked for more.
type MtProtoClient
    (
        dc: DataCenter,
        ?logger: ILogger,
        ?transportFactory: DataCenter -> ITransport,
        ?responseTimeout: TimeSpan
    ) =

    // The carrier comes from the DataCenter the caller connected with (`dc.Transport`), so it is
    // chosen at connection setup and is rebuilt the same way on every reconnect. An explicit
    // factory still wins, for tests and for carriers this library does not know about.
    let createTransport = defaultArg transportFactory Transports.create

    let responseTimeout = defaultArg responseTimeout (TimeSpan.FromSeconds 30.0)

    let mutable transport = createTransport dc
    let dispatcher = RpcDispatcher()
    let updateEvent = Event<byte[]>()
    let reconnectedEvent = Event<unit>()
    let connectionLostEvent = Event<MtProtoError>()
    let mutable authKey: AuthKey option = None
    let mutable session: SessionState option = None
    let mutable receiveLoopCts: CancellationTokenSource option = None
    let mutable isReconnecting = false
    let reconnectLock = obj ()
    // Set by Disconnect, cleared by a connect. A closed client answers RPCs with
    // ConnectionClosed at once: there is no reader left to complete them and no reconnect coming,
    // so waiting on either would stall the caller for nothing.
    let mutable closed = false

    // Why the client closed itself, when it did: a transport error code the server sent (-404 is
    // "this auth key is unknown here", which no reconnect can fix) or exhausted reconnects. An RPC
    // on a client in this state gets the reason rather than a generic ConnectionClosed.
    let mutable lostReason: MtProtoError option = None

    // Completed by the reconnect that is in flight: `true` once it has a live connection, `false`
    // when it gave up. RPCs that arrive meanwhile await this. It replaces adding and removing
    // temporary handlers on the public `Reconnected` event, whose `Event<'T>` is an unsynchronised
    // delegate combine — concurrent callers could lose each other's registration and each lost one
    // was a 15 s stall or a dead handler invoked on every future reconnect.
    let mutable reconnectDone: TaskCompletionSource<bool> option = None

    // Keepalive bookkeeping: when the last ping went out and when the last pong came back. A socket
    // the server has stopped serving but not closed leaves the reader blocked forever and every RPC
    // timing out; two missed pongs are the signal to drop it and let the reconnect path run.
    let mutable lastPingSentAt = 0L
    let mutable lastPongAt = 0L

    // Cancelled only by Disconnect, and replaced by the next connect. A reconnect has to outlive
    // the receive loop that asked for it: stopping that loop is the reconnect's own first step, so
    // gating the attempts on the loop's token would abort every reconnect the reader itself
    // triggers — which is nearly all of them.
    let mutable lifetimeCts = new CancellationTokenSource()

    /// Ceiling on a server-supplied service vector. The spec caps `msgs_ack` at 8192 ids and the
    /// same number is the only plausible bound for the two service messages that hand us a
    /// `Vector<long>`; nothing in the reader bounds a count off the wire, so a 16 MiB frame can
    /// otherwise name millions.
    [<Literal>]
    let MaxServiceVectorLength = 8192

    /// Stop the current receive loop and its keepalives, and forget them.
    ///
    /// Under `reconnectLock` because this is a read-modify-write reached from at least three
    /// threads — the application's on Disconnect and on a connect, and the receive loop's or an
    /// RpcAsync caller's on a reconnect. Unsynchronised, a Disconnect could read `None` while a
    /// reconnect was writing `Some`, cancel nothing, and leave a generation running that nothing
    /// owned. The flag it sits beside has always been locked; this field was not, and centralising
    /// its mutation here is what made that visible.
    ///
    /// Cancelled but deliberately not disposed: the loops being torn down still hold this token and
    /// hand it to the transport, and disposing it under them turns a clean cancellation into an
    /// ObjectDisposedException from inside someone's registration. A source with no timer and no
    /// live registrations is cheap to simply let go of.
    let stopReceiveLoops () =
        lock reconnectLock (fun () ->
            match receiveLoopCts with
            | Some old ->
                receiveLoopCts <- None
                old.Cancel()
            | None -> ())

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

                    // The write itself runs under the client's lifetime token, not the caller's:
                    // on the obfuscated carriers the keystream has already advanced by this frame,
                    // so a cancellation that lands mid-write desyncs the stream for every sender.
                    // The wait for the lock above is where a caller's token applies.
                    match! transport.SendAsync(encrypted, lifetimeCts.Token) with
                    | Ok() -> return Ok msgId
                    | Error e -> return Error(MtProtoError.TransportError e)
                finally
                    %sendLock.Release()
            | _ -> return Error MtProtoError.NotConnected
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
        // Read with a reader rather than poked out of the buffer. `rpc_error` is not one of the
        // three constructors the schema exempts as parsed manually, so byte-poking its id is the
        // thing the house rule forbids — and a second reader over the same array is free, since
        // TlReadBuffer holds a position and nothing else.
        let isRpcError =
            resultData.Length >= 8
            && (use peek = new TlReadBuffer(resultData)
                peek.ReadConstructorId() = Requests.RpcError.ConstructorId)

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

                dispatcher.FailRequest(reqMsgId, MtProtoError.ofRpcError err.ErrorCode err.ErrorMessage)
            else
                dispatcher.CompleteRequest(reqMsgId, resultData)

        if not completed then
            log.LogWarning("No pending request for msg_id {MsgId}", reqMsgId)

    /// Re-send a still-pending request under a fresh msg_id (e.g. after bad_server_salt corrected
    /// the salt). The response to the re-send completes the original caller's task via Rekey.
    /// Runs under the send lock so its write can't interleave with another sender's.
    ///
    /// Answers whether the request went back out. `msg_resend_req` needs to know: the spec requires
    /// that ids we cannot serve be answered with a `msgs_state_info` naming their state, and without
    /// that the server keeps asking — the same runaway the detailed-info branch exists to end.
    let resendRequest (oldMsgId: int64) : Task<bool> =
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
                    if not (dispatcher.Rekey(oldMsgId, newMsgId)) then
                        return false
                    else
                        let seqNo = Session.nextSeqNo sess true
                        dispatcher.SetSeqNo(newMsgId, seqNo)
                        let encrypted = MessageFraming.encrypt key sess newMsgId seqNo body

                        match! transport.SendAsync(encrypted, lifetimeCts.Token) with
                        | Ok() -> return true
                        | Error e ->
                            // The socket is gone; the reader will notice and reconnect, and the
                            // request stays pending for the retransmission that follows. Failing it
                            // here would make the caller repeat it under a new id.
                            log.LogDebug("Re-send of msg_id {MsgId} failed: {Error}", newMsgId, e)
                            return false
                finally
                    %sendLock.Release()
            | _ -> return false
        }

    /// Open a new session on the same connection and auth key, then re-send everything pending
    /// under fresh ids. For the two cases where nothing sent on the old session can be repaired: a
    /// seq_no complaint from the server, and a clock correction backwards past ids already issued.
    /// The server answers the first message of the new session with `new_session_created`, which
    /// is where subscribers learn that the update stream may have a gap.
    let renewSession () =
        match session with
        | Some sess ->
            Session.renew sess
            pendingAcks.Clear()

            lock seenMsgIdLock (fun () ->
                seenMsgIds.Clear()
                seenMsgIdOrder.Clear())

            let pending = dispatcher.PendingIds

            log.LogInformation(
                "New session {SessionId}; re-sending {Count} pending request(s)",
                sess.SessionId,
                pending.Length
            )

            for id in pending do
                %Task.Run(Func<Task>(fun () -> task { let! _ = resendRequest id in () }))
        | None -> ()

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
                            // A duplicate content-related message means the server never saw our
                            // ack for it and is retransmitting; dropping it silently here left that
                            // retransmission with nothing to ever stop it. Re-ack without recursing
                            // into processInnerMessage — the dedupe above already keeps it from
                            // being applied twice, and the ack itself is idempotent.
                            if innerSeqNo &&& 1 = 1 then
                                enqueueAck innerMsgId
                                log.LogDebug("Re-acking replayed inner msg_id {MsgId}", innerMsgId)
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
                        %Task.Run(Func<Task>(fun () -> task { let! _ = resendRequest badMsgId in () }))
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
                        %Task.Run(Func<Task>(fun () -> task { let! _ = resendRequest abandonedId in () }))

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
                        let offset = serverSeconds - localSeconds

                        // Session.resetClock takes session's own lock, shared with
                        // generateMsgId, so this write can't land inside that function's
                        // read-modify-write of LastMsgId and be silently overwritten by
                        // the stale value it was about to write back. It answers whether the
                        // session can continue: after a backward correction the ids already
                        // issued sit above the corrected clock, and only a new session can
                        // start below them.
                        let sessionSurvives =
                            match session with
                            | Some s -> Session.resetClock s offset
                            | None -> true

                        if sessionSurvives then
                            log.LogWarning(
                                "bad_msg_notification {ErrCode} for msg_id {MsgId}; time offset corrected to {Offset}s, re-sending",
                                errCode,
                                badMsgId,
                                offset
                            )

                            %Task.Run(Func<Task>(fun () -> task { let! _ = resendRequest badMsgId in () }))
                        else
                            log.LogWarning(
                                "bad_msg_notification {ErrCode} for msg_id {MsgId}; clock corrected {Offset}s backwards past ids already sent, opening a new session",
                                errCode,
                                badMsgId,
                                offset
                            )

                            renewSession ()
                    | 20 ->
                        // "Message too old, and it cannot be verified whether the server has or
                        // has not received a message with this msg_id": the remedy is to send it
                        // again, which is also what the caller would do by hand.
                        log.LogWarning("bad_msg_notification 20 for msg_id {MsgId}; re-sending", badMsgId)
                        %Task.Run(Func<Task>(fun () -> task { let! _ = resendRequest badMsgId in () }))
                    | 32
                    | 33
                    | 34
                    | 35 ->
                        // A seq_no complaint (too low, too high, even where odd was expected or the
                        // reverse) means our counter and the server's view of this session no
                        // longer agree, and every later message on it will be refused the same
                        // way. Until 0.13 this failed the one request and left the session as it
                        // was, so the consumer's retry earned the same answer; the only repair is a
                        // fresh session on the same connection and auth key.
                        log.LogWarning(
                            "bad_msg_notification {ErrCode} for msg_id {MsgId}; seq_no out of step, opening a new session",
                            errCode,
                            badMsgId
                        )

                        renewSession ()
                    | _ ->
                        log.LogWarning("bad_msg_notification {ErrCode} for msg_id {MsgId}", errCode, badMsgId)
                        %dispatcher.FailRequest(badMsgId, MtProtoError.BadMsgNotification errCode)
                | GeneratedCid.Pong ->
                    // The reply to our keepalive ping. Its time is what the ping loop compares
                    // against to notice a connection the server has stopped serving.
                    lastPongAt <- Environment.TickCount64
                    log.LogTrace("pong")
                | GeneratedCid.MsgsAck ->
                    // server-side msgs_ack — acknowledges our sends, nothing to do.
                    ()
                | GeneratedCid.MsgDetailedInfo
                | GeneratedCid.MsgNewDetailedInfo ->
                    // Both variants name an *answer* the server is holding for us, and neither may be
                    // acked by its own id. Until the answer is acked the server keeps re-announcing
                    // it: measured against a live account, 28 of these arrived in one hour, over and
                    // over for the same ids.
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

                    // Acked only if we actually have it. An ack is the server's cue to discard, so
                    // acking an answer that never arrived throws it away with no trace — and
                    // `msg_new_detailed_info` names a *server-initiated* message, which for this
                    // client means an update. Asking for it instead is the spec's own remedy and it
                    // still ends the re-announcement, by getting the message rather than by
                    // pretending we have it.
                    if wasSeen answerMsgId then
                        enqueueAck answerMsgId
                        log.LogTrace("msg_detailed_info: acking answer_msg_id {AnswerMsgId}", answerMsgId)
                    else
                        log.LogWarning(
                            "msg_detailed_info names answer_msg_id {AnswerMsgId} we never received; requesting it",
                            answerMsgId
                        )

                        %Task.Run(
                            Func<Task>(fun () ->
                                task {
                                    let body =
                                        serializeService (fun w ->
                                            MsgResendReq.Serialize(w, { MsgIds = [| answerMsgId |] }))

                                    match! sendServiceMessage body false CancellationToken.None with
                                    | Ok _ -> ()
                                    | Error e -> log.LogDebug("msg_resend_req send failed: {Error}", e)
                                })
                        )
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

                    // Truncated to the spec's ceiling on an ack batch. Nothing bounds a vector count
                    // off the wire, so a single 16 MiB frame can name ~2M ids — and this branch runs
                    // on the receive thread, taking the replay lock once per id and materialising a
                    // string that long before any other inbound message can be processed. The
                    // `msg_container` branch above rejects an implausible count for the same reason.
                    let asked = req.MsgIds |> Array.truncate MaxServiceVectorLength

                    if asked.Length < req.MsgIds.Length then
                        log.LogWarning(
                            "msgs_state_req named {Count} ids; answering the first {Kept}",
                            req.MsgIds.Length,
                            asked.Length
                        )

                    // One status byte per requested id, in the order asked: 4 is "received", which
                    // doubles as an acknowledgement, and 1 is "nothing known" — the honest answer
                    // for anything that has aged out of the replay window.
                    let info =
                        asked
                        |> Array.map (fun id -> if wasSeen id then '\004' else '\001')
                        |> System.String

                    log.LogDebug("msgs_state_req for {Count} id(s); answering", asked.Length)

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

                    // Same ceiling, and here it bounds scheduled work rather than one string: this
                    // used to queue one Task per id, so an implausible count became millions of
                    // state machines all contending for the send lock.
                    let ids = req.MsgIds |> Array.truncate MaxServiceVectorLength

                    log.LogWarning("msg_resend_req for {Count} message(s); re-sending", ids.Length)

                    // Sequential, in one task, because the reply depends on all the answers. The
                    // spec requires that if any requested id does not exist or has been forgotten,
                    // the whole request is answered with a `msgs_state_info` as though it had also
                    // been a `msgs_state_req`; drop them silently and the server keeps asking, which
                    // is the runaway the detailed-info branch exists to end.
                    %Task.Run(
                        Func<Task>(fun () ->
                            task {
                                let unservable = ResizeArray<int64>()

                                for id in ids do
                                    let! sent = resendRequest id

                                    if not sent then
                                        unservable.Add id

                                if unservable.Count > 0 then
                                    log.LogWarning(
                                        "msg_resend_req: {Count} of {Total} id(s) could not be re-sent; reporting their state",
                                        unservable.Count,
                                        ids.Length
                                    )

                                    let info =
                                        ids
                                        |> Array.map (fun id -> if wasSeen id then '\004' else '\001')
                                        |> System.String

                                    let reply =
                                        serializeService (fun w ->
                                            MsgsStateInfo.Serialize(w, { ReqMsgId = msgId; Info = info }))

                                    match! sendServiceMessage reply false CancellationToken.None with
                                    | Ok _ -> ()
                                    | Error e -> log.LogDebug("msgs_state_info send failed: {Error}", e)
                            })
                    )
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
                    // Once a second, not every ten: the server re-announces anything unacknowledged
                    // (`msg_detailed_info`, 28 an hour measured in 0.12.1, partly this), and a
                    // second of latency on an ack costs nothing.
                    do! Tasks.Task.Delay(1000, ct)
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
                lastPingSentAt <- 0L
                lastPongAt <- Environment.TickCount64

                while not ct.IsCancellationRequested do
                    do! Tasks.Task.Delay(pingIntervalMs, ct)

                    // Two intervals without a pong means the server has stopped serving this socket
                    // while keeping it open — the state a blocked reader and a 30 s RPC timeout can
                    // never tell apart from a slow network. Dropping the transport is what turns it
                    // into a read error the reconnect path already handles.
                    if lastPingSentAt > lastPongAt && Environment.TickCount64 - lastPongAt > int64 (2 * pingIntervalMs) then
                        log.LogWarning("No pong for {Seconds}s; dropping the connection to reconnect", (Environment.TickCount64 - lastPongAt) / 1000L)
                        transport.Disconnect()
                    else
                        let pingId = Session.newSessionId ()
                        let disconnectDelay = max 75 (pingIntervalMs / 1000 * 4)

                        match! sendServiceMessage (buildPing pingId disconnectDelay) false ct with
                        | Ok _ -> lastPingSentAt <- Environment.TickCount64
                        | Error e -> log.LogDebug("ping send failed: {Error}", e)
            with
            | :? OperationCanceledException -> ()
            | ex -> log.LogDebug(ex, "ping loop ended")
        }

    /// The default reconnect schedule: three attempts a second, two and four seconds apart.
    let reconnectBackoffs = [| 1000; 2000; 4000 |]

    /// What a -429 ("too many connections from this address") asks for: to be left alone for a
    /// while, not the fastest possible retry.
    let throttledBackoffs = [| 30_000; 60_000; 120_000 |]

    /// Re-send every pending request under the msg_id and seq_no it was first sent with. Runs after
    /// a reconnect that reused the auth key: the MTProto session survived the socket, and a server
    /// that did execute a request answers the same id once instead of running a second copy under
    /// a fresh id — which is what happened whenever the consumer retried a request the drop had
    /// failed (`messages.transcribeAudio` charges its quota per call).
    let retransmitPending () : Task<unit> =
        task {
            match authKey, session with
            | Some key, Some sess ->
                let pending = dispatcher.PendingRequests |> List.sortBy (fun (id, _, _) -> id)

                if not pending.IsEmpty then
                    log.LogInformation("Re-sending {Count} request(s) left pending by the drop", pending.Length)

                    do! sendLock.WaitAsync(lifetimeCts.Token)

                    try
                        let mutable ok = true

                        for (msgId, seqNo, body) in pending do
                            if ok then
                                let encrypted = MessageFraming.encrypt key sess msgId seqNo body

                                match! transport.SendAsync(encrypted, lifetimeCts.Token) with
                                | Ok() -> ()
                                | Error e ->
                                    // The new socket is already gone; the reader notices and the
                                    // next reconnect re-sends what is still pending.
                                    log.LogWarning("Re-send of msg_id {MsgId} failed: {Error}", msgId, e)
                                    ok <- false
                    finally
                        %sendLock.Release()
            | _ -> ()
        }

    /// Wait for the reconnect in flight, if any. `Ok` once the connection is back, `ReconnectFailed`
    /// when it gave up, `Cancelled` when the caller's token fired first.
    let awaitReconnect (ct: CancellationToken) : Task<Result<unit, MtProtoError>> =
        task {
            let inFlight =
                lock reconnectLock (fun () -> if isReconnecting then reconnectDone else None)

            match inFlight with
            | None -> return Ok()
            | Some finished ->
                try
                    let! ok = finished.Task.WaitAsync(ct)
                    return if ok then Ok() else Error MtProtoError.ReconnectFailed
                with :? OperationCanceledException ->
                    return Error MtProtoError.Cancelled
        }

    /// One read and everything that follows from it. `Loop.Continue` reads again; `Loop.Stop` ends
    /// this generation of the reader, after starting a reconnect where one is due.
    ///
    /// Driven by `Task.loop` rather than recursing with `return! receiveStep ct`. That recursion was
    /// not a tail call: every step stayed registered as the continuation of the next, so a
    /// connection retained one state machine per frame for as long as it lived — measured at 568
    /// bytes per frame, whatever the frame's size — and when reads completed synchronously, as a
    /// socket read does with data already buffered, the steps nested real stack frames and a 1 MB
    /// thread-pool stack overflowed between 1,000 and 1,500 of them.
    let rec receiveStep (ct: CancellationToken) : Task<Loop<CancellationToken, unit>> =
        task {
            if ct.IsCancellationRequested || not transport.IsConnected then
                return Loop.Stop()
            else
                match! transport.ReceiveAsync(ct) with
                | Error TransportError.ConnectionClosed ->
                    // A loop whose CT was already cancelled has been superseded by a newer
                    // generation, and must not start a reconnect on behalf of the connection that
                    // replaced it. Pending requests are kept: the session survives the socket and
                    // they are re-sent once it is back.
                    if not ct.IsCancellationRequested then
                        log.LogWarning("Connection closed by server; reconnecting")
                        do! reconnectInternal reconnectBackoffs

                    return Loop.Stop()
                | Error TransportError.Cancelled
                | Error TransportError.Timeout ->
                    // Cancelled: our own receive CT (Disconnect / reconnect tearing this loop
                    // down), and the guard at the top exits on the next step. Timeout: a carrier
                    // deadline with nothing consumed, which none of the shipped carriers produce —
                    // reading again is the only thing that is not an over-reaction.
                    return Loop.Continue ct
                | Error e ->
                    // A desynced byte stream (InvalidFrame) or broken socket (ReadError /
                    // ConnectionFailed) leaves the read position unrecoverable: looping would
                    // spin on garbage until every in-flight RPC times out. Treat it like a
                    // dropped connection and reconnect.
                    if not ct.IsCancellationRequested then
                        log.LogWarning("Receive error ({Error}); stream unrecoverable, reconnecting", e)
                        do! reconnectInternal reconnectBackoffs

                    return Loop.Stop()
                | Ok data when data.Length = 4 ->
                    // Not a message: a transport-level error code, sent bare and followed by the
                    // server closing the socket. Until 0.13 this reached `decrypt`, failed as an
                    // unreadable frame, and the close that followed looked like any other drop —
                    // so a revoked auth key was retried with 1/2/4 s backoffs forever and nobody
                    // learned that the session file had to be discarded.
                    let code = BinaryPrimitives.ReadInt32LittleEndian(ReadOnlySpan data)
                    do! onTransportErrorCode code
                    return Loop.Stop()
                | Ok data ->
                    match authKey with
                    | None ->
                        // Only reachable between a Disconnect and a connect; nothing here can read
                        // an encrypted frame.
                        log.LogWarning("Frame received with no auth key; stopping the reader")
                        return Loop.Stop()
                    | Some key ->
                        match MessageFraming.decrypt key data with
                        | Ok(msgId, sessionId, seqNo, body) ->
                            match session with
                            | Some s when s.SessionId <> sessionId ->
                                log.LogWarning("Dropping message for foreign session_id {SessionId}", sessionId)
                            | _ ->
                                // A server msg_id is 1 or 3 mod 4; anything else did not come from
                                // a server following the protocol. The time window the spec also
                                // asks for is checked at warning level only: it depends on our own
                                // clock offset, and dropping the bad_msg_notification that would
                                // correct that offset is how a client locks itself out.
                                if msgId &&& 3L <> 1L && msgId &&& 3L <> 3L then
                                    log.LogWarning("Dropping msg_id {MsgId}: not a server message id", msgId)
                                else
                                    let serverSeconds = msgId >>> 32

                                    let expected =
                                        DateTimeOffset.UtcNow.ToUnixTimeSeconds()
                                        + int64 (session |> Option.map _.TimeOffset |> Option.defaultValue 0)

                                    if serverSeconds > expected + 30L || serverSeconds < expected - 300L then
                                        log.LogWarning(
                                            "msg_id {MsgId} is {Delta}s from the expected time; the time offset may be stale",
                                            msgId,
                                            serverSeconds - expected
                                        )

                                    if markSeen msgId then
                                        // One unparseable message must cost that message, not the
                                        // connection: the handler in `receiveLoop` fails every
                                        // in-flight RPC and reconnects.
                                        try
                                            processInnerMessage body msgId seqNo
                                        with ex ->
                                            log.LogError(ex, "Failed to process msg_id {MsgId}", msgId)
                                    else
                                        // Our own ack for this msg_id may have been lost, which is
                                        // exactly why the server retransmitted it — dropping the
                                        // retry silently leaves nothing to ever stop the
                                        // retransmission loop. Re-ack without calling
                                        // processInnerMessage: the replay guard above still keeps
                                        // it from being applied a second time, and the ack itself
                                        // is idempotent and cheap.
                                        if seqNo &&& 1 = 1 then
                                            enqueueAck msgId
                                            log.LogDebug("Re-acking replayed msg_id {MsgId}", msgId)
                                        else
                                            log.LogWarning("Dropping replayed msg_id {MsgId}", msgId)
                        | Error e -> log.LogError("Failed to decrypt message: {Error}", e)

                        return Loop.Continue ct
        }

    and receiveLoop (ct: CancellationToken) =
        task {
            try
                do! Task.loop receiveStep ct
            with
            | :? OperationCanceledException -> ()
            | ex ->
                // An exception in message processing (malformed container, corrupt gzip, a throwing
                // update subscriber) would otherwise kill the reader while the socket stays
                // "connected" — every later RPC then times out forever. Reconnect instead.
                log.LogError(ex, "Receive loop error; reconnecting")
                do! reconnectInternal reconnectBackoffs
        }

    /// A transport error code has arrived; the server closes the socket right after it.
    and onTransportErrorCode (code: int) : Task<unit> =
        task {
            match code with
            | -429 ->
                // Too many connections from this address. Reconnecting at once is what it asks us
                // not to do; the slow schedule leaves the other connections time to go away.
                log.LogWarning("Transport error -429 (too many connections); reconnecting on the slow schedule")
                do! reconnectInternal throttledBackoffs
            | _ ->
                // -404 is an auth key this DC does not know — revoked from another device, reset
                // with auth.resetAuthorizations, or presented to the wrong DC — and -444 an invalid
                // DC. No reconnect can fix either: the client closes itself, everything pending
                // fails with the code, and the consumer's `ConnectionLost` handler is where the
                // session store gets cleared.
                let reason = MtProtoError.TransportErrorCode code
                log.LogError("Transport error {Code}; closing the client, the auth key is unusable here", code)

                lock reconnectLock (fun () ->
                    lostReason <- Some reason
                    closed <- true)

                stopReceiveLoops ()
                authKey <- None
                session <- None
                transport.Disconnect()
                dispatcher.FailAll reason

                try
                    connectionLostEvent.Trigger reason
                with ex ->
                    log.LogError(ex, "ConnectionLost subscriber threw")
        }

    and reconnectInternal (backoffs: int[]) =
        task {
            // The generation this attempt belongs to, captured before the flag is taken so the
            // `finally` can see it. A Disconnect followed by a connect replaces `lifetimeCts`, and
            // the attempt parked in a transport call at that moment no longer owns anything: it must
            // not clear `isReconnecting` on the way out, or it releases a flag the revived client's
            // own reconnect is holding. Without this, a stale attempt against a black-holed peer left
            // every RPC on the revived client waiting out two 15s windows while nothing repaired it.
            let generation = lifetimeCts
            let finished = TaskCompletionSource<bool>(TaskCreationOptions.RunContinuationsAsynchronously)

            let shouldReconnect =
                lock reconnectLock (fun () ->
                    if isReconnecting || closed then
                        false
                    else
                        isReconnecting <- true
                        reconnectDone <- Some finished
                        true)

            if not shouldReconnect then
                ()
            else

                // Everything below runs under try/finally because the flag gates every future
                // reconnect *and* the wait inside RpcAsync. Leaving it set — a cancelled backoff
                // delay used to throw straight out of here — permanently convinces the client that
                // a reconnect is in flight: no attempt is ever made again and every later RPC
                // burns its reconnect wait before failing. That is a silent, unrecoverable death.
                let mutable reconnected = false

                try
                    // The reader that triggered this is still running, and it reads `transport`
                    // through the same mutable field this is about to overwrite. Stopping it first
                    // is the whole point: swapping underneath it pointed the old loop at the new
                    // socket, so for the rest of its life it raced the new reader for frames off a
                    // connection it knew nothing about — every frame it won was decrypted against
                    // the wrong generation's expectations and every one it lost was a reply the new
                    // reader never saw.
                    stopReceiveLoops ()

                    let ct = generation.Token

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
                                        //
                                        // `spawnReceiveAndKeepalive` answers whether it actually
                                        // started anything: a Disconnect that landed while the
                                        // connect was in flight makes it refuse, and then this
                                        // attempt owns a live socket nobody is reading. Hand it back
                                        // rather than announcing a reconnect that is not one.
                                        if spawnReceiveAndKeepalive () then
                                            reconnected <- true
                                            log.LogInformation("Reconnected (reused auth key)")
                                            do! retransmitPending ()
                                        else
                                            log.LogInformation("Reconnect abandoned: the client was closed while connecting")
                                            transport.Disconnect()
                                    | None ->
                                        match! AuthKeyExchange.performExchange transport dc.Id ct with
                                        | Error e ->
                                            log.LogWarning(
                                                "Auth key exchange failed on reconnect attempt {Attempt}: {Error}",
                                                attempt + 1,
                                                e
                                            )
                                        | Ok(key, salt, timeOffset) ->
                                            authKey <- Some key
                                            let sess = Session.createSession ()
                                            sess.Salt <- salt
                                            sess.TimeOffset <- timeOffset
                                            session <- Some sess

                                            if spawnReceiveAndKeepalive () then
                                                reconnected <- true
                                                log.LogInformation("Reconnected successfully")
                                            else
                                                log.LogInformation(
                                                    "Reconnect abandoned: the client was closed during the key exchange"
                                                )

                                                transport.Disconnect()
                            with
                            | :? OperationCanceledException -> ()
                            | ex -> log.LogWarning(ex, "Reconnect attempt {Attempt} error", attempt + 1)

                    if not reconnected then
                        // The requests kept pending through the drop have nowhere to go now. The
                        // client stays open: the next RpcAsync tries again from scratch.
                        log.LogError("All reconnect attempts failed")
                        dispatcher.FailAll MtProtoError.ReconnectFailed

                        try
                            connectionLostEvent.Trigger MtProtoError.ReconnectFailed
                        with ex ->
                            log.LogError(ex, "ConnectionLost subscriber threw")
                finally
                    // Only the attempt that still owns the current generation releases the flag. A
                    // superseded one clearing it would hand a revived client's in-flight reconnect
                    // away to a second concurrent attempt, and both would swap `transport`.
                    lock reconnectLock (fun () ->
                        if obj.ReferenceEquals(lifetimeCts, generation) then
                            isReconnecting <- false
                            reconnectDone <- None)

                    // Completed after the flag is released, so a waiter that wakes and checks the
                    // flag does not find a reconnect still "in flight".
                    %finished.TrySetResult reconnected

                // Raised last, off the reconnect's own critical section: subscribers run gap
                // recovery here, and a slow one used to hold up the attempt that had just succeeded.
                if reconnected then
                    reconnectedEvent.Trigger()
        }

    /// Start a fresh receive loop plus the ack/ping keepalive loops on a new CT. Stops the previous
    /// generation first so a reconnect doesn't leave the old keepalive loops running (they'd pile up
    /// across reconnects, all writing to the now-shared transport).
    ///
    /// Refuses to start anything on a closed client, and decides that under the same lock that
    /// publishes the generation. A reconnect can be parked in `ConnectAsync` — or, on the fresh-key
    /// path, in a whole DH exchange — when `Disconnect` runs: the connect then completes anyway and
    /// used to install three loops nothing could ever cancel, because a closed client refuses every
    /// later reconnect and `RpcAsync` returns before it reaches one. The symptom was a torn-down
    /// client that kept pinging and flushing acks for the life of the process.
    and spawnReceiveAndKeepalive () : bool =
        stopReceiveLoops ()

        let cts = new CancellationTokenSource()

        let started =
            lock reconnectLock (fun () ->
                if closed || lifetimeCts.IsCancellationRequested then
                    false
                else
                    receiveLoopCts <- Some cts
                    true)

        if started then
            %Task.Run(Func<Task>(fun () -> receiveLoop cts.Token))
            %Task.Run(Func<Task>(fun () -> ackLoop cts.Token))
            %Task.Run(Func<Task>(fun () -> pingLoop cts.Token))

        started

    /// Set the auth key + a fresh session (carrying the given salt/time offset) and start the
    /// receive + keepalive loops. Shared by the fresh-DH connect and persisted-session restore,
    /// and the one place that revives a client an earlier Disconnect had closed.
    let startSession (key: AuthKey) (salt: int64) (timeOffset: int32) =
        // Replacing the lifetime generation and disowning any attempt still parked in a transport
        // call, both under the lock that publishes them. Without the replacement every reconnect on
        // the revived client aborts before its first backoff; without clearing the flag, a stale
        // attempt against an unreachable peer keeps it set for as long as its connect takes, and
        // every RPC in the meantime waits out two 15s reconnect windows for a repair that the
        // stale attempt will never perform and a fresh one is refused from starting.
        lock reconnectLock (fun () ->
            closed <- false
            lostReason <- None

            if lifetimeCts.IsCancellationRequested then
                lifetimeCts <- new CancellationTokenSource()
                isReconnecting <- false
                reconnectDone <- None)

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

        // A fresh session is not a reconnect: nothing else is racing the spawn here, and a

        // caller that just set `closed <- false` under the lock cannot be refused.

        spawnReceiveAndKeepalive () |> ignore

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
    /// awaited outside the lock. Failures arrive as Error Results, never thrown — a cancelled token
    /// included, which is `Cancelled`.
    ///
    /// A drop while the request is in flight does not fail it: the session outlives the socket, so
    /// the request is re-sent under its original ids once the connection is back, and only
    /// exhausted reconnects (`ReconnectFailed`) or a transport error code end it early. A client
    /// closed by Disconnect answers ConnectionClosed at once, one closed by a -404 answers with
    /// that code: nothing will read a reply and no reconnect is coming.
    member _.RpcAsync(requestBody: byte[], ct: CancellationToken) : Task<Result<byte[], MtProtoError>> =
        task {
            match lock reconnectLock (fun () -> closed, lostReason) with
            | true, Some reason -> return Error reason
            | true, None -> return Error(MtProtoError.TransportError TransportError.ConnectionClosed)
            | false, _ ->

            match session, authKey with
            | None, _
            | _, None -> return Error MtProtoError.NotConnected
            | Some sess, Some key ->

            // Reconnects are otherwise only ever driven by the receive loop, and that loop is gone
            // once its attempts are exhausted (or once something cancelled them). An RPC arriving
            // on a client whose transport is down is the trigger that brings the connection back,
            // instead of every later call failing until the process is restarted. Deliberately not
            // tied to the caller's token: one caller's timeout must not abort a shared reconnect.
            if not transport.IsConnected && not isReconnecting then
                log.LogInformation("RpcAsync: transport is down, reconnecting before the send")
                do! reconnectInternal reconnectBackoffs

            match! awaitReconnect ct with
            | Error e -> return Error e
            | Ok() ->

            // The lock wait is where the caller's token applies; a token that fires here is a
            // cancellation, not a transport failure. The write itself runs on the lifetime token
            // (see sendServiceMessage).
            let mutable acquired = false

            try
                do! sendLock.WaitAsync(ct)
                acquired <- true
            with :? OperationCanceledException ->
                ()

            if not acquired then
                return Error MtProtoError.Cancelled
            else

            let mutable sent: Result<int64 * Task<Result<byte[], MtProtoError>>, MtProtoError> =
                Error MtProtoError.NotConnected

            try
                let msgId = Session.generateMsgId sess
                let seqNo = Session.nextSeqNo sess true
                let encrypted = MessageFraming.encrypt key sess msgId seqNo requestBody
                let responseTask = dispatcher.RegisterRequest(msgId, seqNo, requestBody)

                match! transport.SendAsync(encrypted, lifetimeCts.Token) with
                | Ok() -> sent <- Ok(msgId, responseTask)
                | Error TransportError.ConnectionClosed
                | Error(TransportError.WriteError _)
                | Error TransportError.Cancelled ->
                    // The socket died under the write. The request stays registered: a reconnect
                    // re-sends it under these ids, so the caller waits for the answer like any
                    // other instead of retrying under a new id.
                    log.LogInformation("Send of msg_id {MsgId} failed on a dead socket; it will be re-sent after the reconnect", msgId)
                    sent <- Ok(msgId, responseTask)
                | Error e ->
                    %dispatcher.FailRequest(msgId, MtProtoError.TransportError e)
                    sent <- Error(MtProtoError.TransportError e)
            finally
                %sendLock.Release()

            match sent with
            | Error e -> return Error e
            | Ok(msgId, responseTask) ->
                // A send that found the socket dead is what brings the connection back when no
                // reader is around to do it.
                if not transport.IsConnected && not isReconnecting then
                    do! reconnectInternal reconnectBackoffs

                try
                    if responseTimeout > TimeSpan.Zero then
                        use cts = CancellationTokenSource.CreateLinkedTokenSource(ct)
                        cts.CancelAfter(responseTimeout)
                        return! responseTask.WaitAsync(cts.Token)
                    else
                        return! responseTask.WaitAsync(ct)
                with :? OperationCanceledException ->
                    let reason = if ct.IsCancellationRequested then MtProtoError.Cancelled else MtProtoError.Timeout
                    %dispatcher.FailRequest(msgId, reason)
                    return Error reason
        }

    /// Push updates from the server (raw bytes of the update message)
    member _.UpdateReceived = updateEvent.Publish

    /// Raised after a successful automatic reconnect, and on `new_session_created`: either way the
    /// update stream may have a hole that only the application can fill.
    member _.Reconnected = reconnectedEvent.Publish

    /// Raised once when the client gives up on its connection by itself: every reconnect attempt
    /// failed (`ReconnectFailed` — the next RpcAsync tries again), or the server sent a transport
    /// error code that no reconnect can fix (`TransportErrorCode` — the client is closed, and a
    /// -404 means the persisted session must be discarded and a login redone).
    member _.ConnectionLost = connectionLostEvent.Publish

    member _.Disconnect() =
        lock reconnectLock (fun () ->
            closed <- true
            lostReason <- None)

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
