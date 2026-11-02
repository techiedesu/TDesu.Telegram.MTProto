namespace TDesu.Telegram.Protocol.Tests

open System
open System.Collections.Concurrent
open System.Threading
open System.Threading.Tasks
open NUnit.Framework
open TDesu.Crypto
open TDesu.MTProto
open TDesu.MTProto.Service
open TDesu.Serialization
open TDesu.Transport

/// Loopback that records the salt of every frame whose body is the test's request, and answers each
/// such frame through `buildReply` (the count of request frames so far, and the frame's msg_id).
/// Acks and keepalive traffic pass unrecorded and unanswered.
type internal SaltRecordingTransport(authKey: AuthKey, requestCid: uint32, buildReply: int * int64 -> byte[]) =
    let mutable connected = false
    let replies = ConcurrentQueue<byte[]>()
    let ready = new SemaphoreSlim(0)
    let salts = ConcurrentQueue<int64>()
    let mutable requests = 0

    /// The salt each request frame carried, re-sends included, in the order they were sent.
    member _.RequestSalts = salts |> List.ofSeq

    interface ITransport with
        member _.IsConnected = connected

        member _.ConnectAsync(_) =
            connected <- true
            Task.FromResult(Ok())

        member _.SendAsync(payload, _) =
            let reader = TlReadBuffer(payload)
            reader.ReadInt64() |> ignore // auth_key_id
            let msgKey = reader.ReadRawBytes(16)
            let encrypted = reader.ReadRawBytes(payload.Length - 24)
            let aes = KeyDerivation.deriveAesKeyIv authKey.Data msgKey 0
            let inner = TlReadBuffer(AesIge.decrypt encrypted aes.Key aes.Iv)
            let salt = inner.ReadInt64()
            let sessionId = inner.ReadInt64()
            let msgId = inner.ReadInt64()
            inner.ReadInt32() |> ignore // seq_no
            inner.ReadInt32() |> ignore // length

            if inner.ReadConstructorId() = requestCid then
                salts.Enqueue salt
                let n = Interlocked.Increment(&requests)
                let sess = Session.createSession ()
                sess.SessionId <- sessionId
                replies.Enqueue(MessageFramingTests.serverEncrypt authKey sess (msgId + 1L) 1 (buildReply (n, msgId)))
                ready.Release() |> ignore

            Task.FromResult(Ok())

        member _.ReceiveAsync(ct) =
            task {
                do! ready.WaitAsync(ct)
                let mutable frame = Array.empty

                return
                    if replies.TryDequeue(&frame) then
                        Ok frame
                    else
                        Error TransportError.Timeout
            }

        member _.Disconnect() = connected <- false

    interface IDisposable with
        member _.Dispose() = ready.Dispose()

/// The server rotates its salt about once an hour and refuses a message carrying an expired one
/// with `bad_server_salt`. The client asks for the coming salts ahead of time and switches by the
/// server's clock; a salt the server names outright replaces whatever it had announced.
[<TestFixture>]
module ServerSaltTests =

    let private key: AuthKey = {
        Data = Array.init 256 (fun i -> byte ((i * 11 + 5) % 256))
        Id = 0x1020304050607080L
        AuxHash = 0L
    }

    [<Literal>]
    let private RestoredSalt = 0x0BADL

    let private request = [| 0xEDuy; 0x5Euy; 0xEDuy; 0x5Euy |]

    let private window since until salt : SaltWindow = {
        ValidSince = since
        ValidUntil = until
        Salt = salt
    }

    // rpc_result and msg_container are read by hand on the client and have no generated writer.
    let private rpcResult (reqMsgId: int64) (result: byte[]) =
        use w = new TlWriteBuffer()
        w.WriteConstructorId(0xf35c6d01u)
        w.WriteInt64(reqMsgId)
        w.WriteRawBytes(result)
        w.ToArray()

    let private container (msgId: int64) (bodies: byte[] list) =
        use w = new TlWriteBuffer()
        w.WriteConstructorId(0x73f1f8dcu)
        w.WriteInt32(bodies.Length)

        bodies
        |> List.iteri (fun i body ->
            w.WriteInt64(msgId + 2L * int64 (i + 1))
            w.WriteInt32(i + 2)
            w.WriteInt32(body.Length)
            w.WriteRawBytes(body))

        w.ToArray()

    let private answer (msgId: int64) = rpcResult msgId [| 9uy; 9uy; 9uy; 9uy |]

    let private futureSalts (salts: (int64 * int64 * int64) list) =
        Tl.bytesOf (
            {
                ReqMsgId = 0L
                Now = int32 (DateTimeOffset.UtcNow.ToUnixTimeSeconds())
                Salts = [|
                    for since, until, salt in salts ->
                        ({
                            ValidSince = int32 since
                            ValidUntil = int32 until
                            Salt = salt
                         }
                        : Requests.FutureSalt)
                |]
            }
            : Requests.FutureSalts
        )

    let private badServerSalt (badMsgId: int64) (salt: int64) =
        Tl.bytesOf (Requests.BadMsgNotification.BadServerSalt(badMsgId, 1, 48, salt))

    let private newSessionCreated (firstMsgId: int64) (salt: int64) =
        Tl.bytesOf (
            {
                FirstMsgId = firstMsgId
                UniqueId = 42L
                ServerSalt = salt
            }
            : Requests.NewSession
        )

    let private connect (transport: SaltRecordingTransport) =
        let client =
            new MtProtoClient(DataCenters.defaultDc, transportFactory = fun _ -> transport :> ITransport)

        match client.ConnectWithAuthKeyAsync(key, RestoredSalt, 0, CancellationToken.None).GetAwaiter().GetResult() with
        | Error e -> Assert.Fail($"stub connect failed: %A{e}")
        | Ok() -> ()

        client

    let private call (client: MtProtoClient) =
        match client.RpcAsync(request, CancellationToken.None).GetAwaiter().GetResult() with
        | Ok _ -> ()
        | Error e -> Assert.Fail($"the call failed: %A{e}")

    let private run (reply: int * int64 -> byte[]) (calls: int) =
        use transport = new SaltRecordingTransport(key, BitConverter.ToUInt32(request, 0), reply)
        use client = connect transport

        for _ in 1..calls do
            call client

        transport.RequestSalts

    // ─── choosing a salt ─────────────────────────────────────────────────

    [<Test>]
    let ``the next salt is taken only once it has been valid for the margin`` () =
        let overlapping = [ window 1000L 4600L 11L; window 4000L 7600L 22L ]
        Assert.That(Session.selectSalt overlapping 3000L, Is.EqualTo(Some 11L))
        Assert.That(Session.selectSalt overlapping 4030L, Is.EqualTo(Some 11L), NUnitString "the next opened 30 s ago")
        Assert.That(Session.selectSalt overlapping 4100L, Is.EqualTo(Some 22L))
        Assert.That(Session.selectSalt overlapping 8000L, Is.EqualTo(None: int64 option))

    [<Test>]
    let ``back-to-back salts switch at the boundary`` () =
        let adjacent = [ window 1000L 4000L 11L; window 4000L 7600L 22L ]
        Assert.That(Session.selectSalt adjacent 3990L, Is.EqualTo(Some 11L))
        Assert.That(Session.selectSalt adjacent 4010L, Is.EqualTo(Some 22L))

    [<Test>]
    let ``more salts are asked for once the announced ones cover less than an hour`` () =
        Assert.That(Session.needsFutureSalts [] 1000L, Is.True)
        Assert.That(Session.needsFutureSalts [ window 0L 10_000L 1L ] 5000L, Is.False)
        Assert.That(Session.needsFutureSalts [ window 0L 8000L 1L ] 5000L, Is.True)

    [<Test>]
    let ``expired salts are dropped and the current one taken`` () =
        let s = Session.createSession ()
        let now = Session.serverNow s
        s.Salt <- 5L
        s.FutureSalts <- [ window (now - 7200L) (now - 3600L) 6L; window (now - 600L) (now + 3000L) 7L ]
        Session.refreshSalt s
        Assert.That(s.Salt, Is.EqualTo 7L)
        Assert.That(s.FutureSalts |> List.map _.Salt, Is.EqualTo<int64 list> [ 7L ])

    // ─── through the client ──────────────────────────────────────────────

    /// The server answers `get_future_salts` bare, with its own req_msg_id; one wrapped in
    /// rpc_result answers a request nobody awaits. Either way the salts must not be dropped.
    [<TestCase(false)>]
    [<TestCase(true)>]
    let ``announced salts are used from the next message on`` (wrapped: bool) =
        let now = DateTimeOffset.UtcNow.ToUnixTimeSeconds()
        let announced = futureSalts [ now - 600L, now + 3000L, 0x5A11L; now + 3000L, now + 6600L, 0x5A12L ]

        let reply (n, msgId) =
            match n with
            | 1 -> container msgId [ (if wrapped then rpcResult 0x4000L announced else announced); answer msgId ]
            | _ -> answer msgId

        Assert.That(run reply 2, Is.EqualTo<int64 list> [ RestoredSalt; 0x5A11L ])

    /// One answer covers many hours, so the salt has to be re-chosen by the clock before each
    /// message, not only when an answer arrives.
    [<Test>]
    let ``the salt switches by the clock between answers`` () =
        let now = DateTimeOffset.UtcNow.ToUnixTimeSeconds()
        // 0x5A12 has been valid for 57 s: it is passed over until the margin is reached, 3 s on.
        let announced = futureSalts [ now - 600L, now + 3000L, 0x5A11L; now - 57L, now + 3600L, 0x5A12L ]

        let reply (n, msgId) =
            match n with
            | 1 -> container msgId [ announced; answer msgId ]
            | _ -> answer msgId

        use transport = new SaltRecordingTransport(key, BitConverter.ToUInt32(request, 0), reply)
        use client = connect transport
        call client
        call client

        while DateTimeOffset.UtcNow.ToUnixTimeSeconds() < now + 3L do
            Thread.Sleep 100

        call client
        Assert.That(transport.RequestSalts, Is.EqualTo<int64 list> [ RestoredSalt; 0x5A11L; 0x5A12L ])

    /// A refused salt was announced as valid; switching back to it before the re-send would be
    /// refused again, every time.
    [<Test>]
    let ``a salt the server refuses is replaced by its own, not by an announced one`` () =
        let now = DateTimeOffset.UtcNow.ToUnixTimeSeconds()

        let reply (n, msgId) =
            match n with
            | 1 -> container msgId [ futureSalts [ now - 600L, now + 3000L, 0x5A11L ]; answer msgId ]
            | 2 -> badServerSalt msgId 0x5A17L
            | _ -> answer msgId

        Assert.That(
            run reply 3,
            Is.EqualTo<int64 list> [ RestoredSalt; 0x5A11L; 0x5A17L; 0x5A17L ],
            NUnitString "the re-send and every later message must carry the salt the server named"
        )

    [<Test>]
    let ``a new session's salt replaces the announced ones`` () =
        let now = DateTimeOffset.UtcNow.ToUnixTimeSeconds()

        let reply (n, msgId) =
            match n with
            | 1 -> container msgId [ futureSalts [ now - 600L, now + 3000L, 0x5A11L ]; answer msgId ]
            | 2 -> container msgId [ newSessionCreated msgId 0x7701L; answer msgId ]
            | _ -> answer msgId

        Assert.That(run reply 3, Is.EqualTo<int64 list> [ RestoredSalt; 0x5A11L; 0x7701L ])
