namespace TDesu.MTProto

open System
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
    let reconnectLock = obj()
    let log = defaultArg logger (Microsoft.Extensions.Logging.Abstractions.NullLogger.Instance :> Microsoft.Extensions.Logging.ILogger)

    let ensureSession () =
        match session with
        | Some s -> s
        | None -> failwith "Session not initialized"

    let ensureAuthKey () =
        match authKey with
        | Some k -> k
        | None -> failwith "Auth key not established"

    let processRpcResult (body: byte[]) (offset: int) (reqMsgId: int64) =
        let resultData = body[offset..]
        if not (dispatcher.CompleteRequest(reqMsgId, resultData)) then
            log.LogWarning("No pending request for msg_id {MsgId}", reqMsgId)

    let processInnerMessage (body: byte[]) (msgId: int64) =
        if body.Length >= 4 then
            use reader = new TlReadBuffer(body)
            let constructor = reader.ReadConstructorId()
            if constructor = 0xf35c6d01u then
                // rpc_result: req_msg_id + result
                let reqMsgId = reader.ReadInt64()
                processRpcResult body 12 reqMsgId
            elif constructor = 0x73f1f8dcu then
                // msg_container: process each message
                let count = reader.ReadInt32()
                for _ in 1..count do
                    let innerMsgId = reader.ReadInt64()
                    let _innerSeqNo = reader.ReadInt32()
                    let innerLength = reader.ReadInt32()
                    let innerBody = reader.ReadRawBytes(innerLength)
                    if innerBody.Length >= 12 then
                        use innerReader = new TlReadBuffer(innerBody)
                        let innerConstructor = innerReader.ReadConstructorId()
                        if innerConstructor = 0xf35c6d01u then
                            let reqMsgId = innerReader.ReadInt64()
                            processRpcResult innerBody 12 reqMsgId
                        else
                            // Server push update inside container
                            log.LogDebug("Container update 0x{Constructor:x8}, msg_id={MsgId}", innerConstructor, innerMsgId)
                            updateEvent.Trigger(innerBody)
            else
                // Server push update (not RPC result, not container)
                log.LogDebug("Push update 0x{Constructor:x8}, msg_id={MsgId}", constructor, msgId)
                updateEvent.Trigger(body)

    let rec receiveLoop (ct: CancellationToken) = task {
        try
            while not ct.IsCancellationRequested && transport.IsConnected do
                match! transport.ReceiveAsync(ct) with
                | Error TransportError.ConnectionClosed ->
                    log.LogWarning("Connection closed by server")
                    dispatcher.FailAll(MtProtoError.TransportError TransportError.ConnectionClosed)
                    // Auto-reconnect
                    do! reconnectInternal ct
                    return ()
                | Error e ->
                    log.LogError("Receive error: {Error}", e)
                | Ok data ->
                    match authKey with
                    | Some key ->
                        match MessageFraming.decrypt key data with
                        | Ok (msgId, _sessionId, _seqNo, body) ->
                            processInnerMessage body msgId
                        | Error e ->
                            log.LogError("Failed to decrypt message: {Error}", e)
                    | None ->
                        match UnencryptedMessage.deserialize data with
                        | Ok (msgId, body) ->
                            %dispatcher.CompleteRequest(msgId, body)
                        | Error e ->
                            log.LogError("Failed to parse unencrypted message: {Error}", e)
        with
        | :? OperationCanceledException -> ()
        | ex -> log.LogError(ex, "Receive loop error")
    }

    and reconnectInternal (ct: CancellationToken) = task {
        let shouldReconnect =
            lock reconnectLock (fun () ->
                if isReconnecting then false
                else isReconnecting <- true; true)
        if not shouldReconnect then () else

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
                    | Error e ->
                        log.LogWarning("Reconnect attempt {Attempt} failed: {Error}", attempt + 1, e)
                    | Ok () ->
                        match! AuthKeyExchange.performExchange transport ct with
                        | Error e ->
                            log.LogWarning("Auth key exchange failed on reconnect: {Error}", attempt + 1, e)
                        | Ok (key, salt, timeOffset) ->
                            authKey <- Some key
                            let sess = Session.createSession ()
                            sess.Salt <- salt
                            sess.TimeOffset <- timeOffset
                            session <- Some sess
                            // Restart receive loop
                            let cts = new CancellationTokenSource()
                            receiveLoopCts <- Some cts
                            %Tasks.Task.Run(Func<Tasks.Task>(fun () -> receiveLoop cts.Token))
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

    /// Connect to the DC and perform auth key exchange
    member _.ConnectAsync(ct: CancellationToken) : Task<Result<unit, MtProtoError>> = task {
        log.LogInformation("Connecting to DC{DcId} at {Address}:{Port}", dc.Id, dc.Address, dc.Port)
        match! transport.ConnectAsync(ct) with
        | Error e -> return Error (MtProtoError.TransportError e)
        | Ok () ->

        log.LogInformation("Connected, performing auth key exchange")
        match! AuthKeyExchange.performExchange transport ct with
        | Error e -> return Error e
        | Ok (key, salt, timeOffset) ->

        authKey <- Some key
        let sess = Session.createSession ()
        sess.Salt <- salt
        sess.TimeOffset <- timeOffset
        session <- Some sess

        log.LogInformation("Auth key established, session created")

        // Start receive loop
        let cts = new CancellationTokenSource()
        receiveLoopCts <- Some cts
        %Task.Run(Func<Task>(fun () -> receiveLoop cts.Token))

        return Ok ()
    }

    /// Send an RPC request and await the response
    member _.RpcAsync(requestBody: byte[], ct: CancellationToken) = task {
        let sess = ensureSession ()
        let key = ensureAuthKey ()

        let msgId = Session.generateMsgId sess
        let seqNo = Session.nextSeqNo sess true

        let encrypted = MessageFraming.encrypt key sess msgId seqNo requestBody
        let responseTask = dispatcher.RegisterRequest(msgId)

        match! transport.SendAsync(encrypted, ct) with
        | Error e -> return Error (MtProtoError.TransportError e)
        | Ok () ->
            try
                use cts = CancellationTokenSource.CreateLinkedTokenSource(ct)
                cts.CancelAfter(TimeSpan.FromSeconds(30.0))
                let! response = responseTask.WaitAsync(cts.Token)
                return Ok response
            with
            | :? OperationCanceledException ->
                %dispatcher.FailRequest(msgId, MtProtoError.Timeout)
                return Error MtProtoError.Timeout
    }

    /// Send an unencrypted message (for pre-auth operations)
    member _.SendUnencryptedAsync(body: byte[], ct: CancellationToken) = task {
        let sess = ensureSession ()
        let msgId = Session.generateMsgId sess
        let unencrypted = UnencryptedMessage.serialize msgId body
        match! transport.SendAsync(unencrypted, ct) with
        | Error e -> return Error (MtProtoError.TransportError e)
        | Ok () -> return Ok msgId
    }

    /// Event fired when a server push update is received (not an RPC response).
    [<CLIEvent>]
    member _.UpdateReceived = updateEvent.Publish

    /// Event fired after a successful automatic reconnection.
    [<CLIEvent>]
    member _.Reconnected = reconnectedEvent.Publish

    /// Disconnect and clean up
    member _.Disconnect() =
        receiveLoopCts |> Option.iter (fun cts -> cts.Cancel(); cts.Dispose())
        receiveLoopCts <- None
        dispatcher.FailAll(MtProtoError.TransportError TransportError.ConnectionClosed)
        transport.Disconnect()
        log.LogInformation("Disconnected")

    interface IDisposable with
        member this.Dispose() = this.Disconnect()
