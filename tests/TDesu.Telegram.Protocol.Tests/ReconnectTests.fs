namespace TDesu.Telegram.Protocol.Tests

open System
open System.Collections.Concurrent
open System.Threading
open System.Threading.Tasks
open TDesu.Crypto
open TDesu.MTProto
open TDesu.Serialization
open TDesu.Transport

/// A scriptable transport: the test decides what each read answers and watches every send.
///
/// Reads complete on a semaphore the test releases, so a "drop" is observable exactly when the
/// test says; sends are decrypted so the test can see the msg_id the client used and mint a reply
/// for it under the client's own session id.
type internal ScriptedTransport(authKey: AuthKey) =
    let mutable connected = false
    let mutable connects = 0
    let reads = ConcurrentQueue<Result<byte[], TransportError>>()
    let ready = new SemaphoreSlim(0)
    let sends = ConcurrentQueue<int64 * int32 * byte[]>()
    let mutable sessionId = 0L

    let clientDecrypt (data: byte[]) =
        let reader = TlReadBuffer(data)
        reader.ReadInt64() |> ignore
        let msgKey = reader.ReadRawBytes(16)
        let encrypted = reader.ReadRawBytes(data.Length - 24)
        let aes = KeyDerivation.deriveAesKeyIv authKey.Data msgKey 0
        let plain = AesIge.decrypt encrypted aes.Key aes.Iv
        let inner = TlReadBuffer(plain)
        inner.ReadInt64() |> ignore
        let sid = inner.ReadInt64()
        let msgId = inner.ReadInt64()
        let seqNo = inner.ReadInt32()
        let len = inner.ReadInt32()
        sid, msgId, seqNo, inner.ReadRawBytes(len)

    /// Queue what the next read will answer.
    member _.Push(read: Result<byte[], TransportError>) =
        reads.Enqueue read
        ready.Release() |> ignore

    /// Queue an encrypted server message under the session the client last sent on.
    member _.PushMessage(msgId: int64, seqNo: int, body: byte[]) =
        let sess = Session.createSession ()
        sess.SessionId <- sessionId
        reads.Enqueue(Ok(MessageFramingTests.serverEncrypt authKey sess msgId seqNo body))
        ready.Release() |> ignore

    /// Every request the client sent: msg_id, seq_no, body — service messages included.
    member _.Sends = sends |> List.ofSeq

    member _.ConnectCount = Volatile.Read &connects

    /// Reads queued and not yet taken by the client's loop.
    member _.PendingReads = reads.Count

    /// Wait until the client has sent `count` frames whose body satisfies `pick`.
    member _.WaitForSends(count: int, pick: byte[] -> bool, timeoutMs: int) =
        SpinWait.SpinUntil(
            (fun () -> sends |> Seq.filter (fun (_, _, body) -> pick body) |> Seq.length >= count),
            timeoutMs
        )

    interface ITransport with
        member _.IsConnected = connected

        member _.ConnectAsync(_) =
            Interlocked.Increment &connects |> ignore
            connected <- true
            Task.FromResult(Ok())

        member _.SendAsync(payload, _) =
            if not connected then
                Task.FromResult(Error TransportError.ConnectionClosed)
            else
                let sid, msgId, seqNo, body = clientDecrypt payload
                sessionId <- sid
                sends.Enqueue(msgId, seqNo, body)
                Task.FromResult(Ok())

        member _.ReceiveAsync(ct) =
            task {
                do! ready.WaitAsync(ct)
                let mutable read = Error TransportError.Cancelled

                if reads.TryDequeue(&read) then
                    match read with
                    | Error TransportError.ConnectionClosed -> connected <- false
                    | _ -> ()

                    return read
                else
                    return Error TransportError.Cancelled
            }

        member _.Disconnect() = connected <- false

    interface IDisposable with
        member _.Dispose() = ready.Dispose()

namespace TDesu.Telegram.Protocol.Tests

open System
open System.Threading
open System.Threading.Tasks
open NUnit.Framework
open TDesu.MTProto
open TDesu.Serialization
open TDesu.Transport

[<TestFixture>]
module ReconnectTests =

    let private testAuthKey: AuthKey = {
        Data = Array.init 256 (fun i -> byte ((i * 7 + 3) % 256))
        Id = 0x1122334455667788L
        AuxHash = 0L
    }

    // rpc_result#f35c6d01 req_msg_id:long result:Object
    let private rpcResult (reqMsgId: int64) (payload: byte[]) : byte[] =
        use w = new TlWriteBuffer()
        w.WriteConstructorId(0xf35c6d01u)
        w.WriteInt64(reqMsgId)
        w.WriteRawBytes(payload)
        w.ToArray()

    // pong#347773c5 msg_id:long ping_id:long
    let private pong (msgId: int64) : byte[] =
        use w = new TlWriteBuffer()
        w.WriteConstructorId(0x347773c5u)
        w.WriteInt64(msgId)
        w.WriteInt64(1L)
        w.ToArray()

    let private isRpc (body: byte[]) =
        body.Length = 4 && body[0] = 0xDEuy

    let private connect (transport: ScriptedTransport) =
        let client =
            new MtProtoClient(DataCenters.defaultDc, transportFactory = fun _ -> transport :> ITransport)

        match client.ConnectWithAuthKeyAsync(testAuthKey, 1L, 0, CancellationToken.None).GetAwaiter().GetResult() with
        | Error e -> Assert.Fail($"stub connect failed: %A{e}")
        | Ok() -> ()

        client

    /// The receive loop used to recurse with `return! receiveStep ct`, which is not a tail call:
    /// one state machine per frame stayed reachable until the connection dropped — measured at
    /// 568 bytes per frame, for frames of any size. Twenty thousand frames at that rate is over
    /// 11 MB; the bound here allows a tenth of it.
    [<Test>]
    [<CancelAfter(120_000)>]
    let ``the receive loop retains nothing per frame`` () =
        use transport = new ScriptedTransport(testAuthKey)
        use client = connect transport

        let frames = 20_000
        let pongs = Array.init frames (fun i -> int64 i * 4L + 3L)

        let heapBefore =
            GC.Collect()
            GC.WaitForPendingFinalizers()
            GC.GetTotalMemory(true)

        for id in pongs do
            // seq_no 0: not content-related, so no ack is queued for it.
            transport.PushMessage(id, 0, pong id)

        // Reads are consumed by the loop as fast as they are pushed; wait for the last one.
        Assert.That(
            SpinWait.SpinUntil((fun () -> transport.PendingReads = 0), 60_000),
            Is.True,
            NUnitString "the loop did not consume every frame"
        )

        Thread.Sleep 500
        GC.Collect()
        GC.WaitForPendingFinalizers()
        let heapAfter = GC.GetTotalMemory(true)
        let perFrame = float (heapAfter - heapBefore) / float frames

        Assert.That(perFrame, Is.LessThan 60.0, NUnitString $"retained %.1f{perFrame} bytes per frame")
        GC.KeepAlive client

    /// A 4-byte frame is a transport error code, not a message. -404 says the auth key is unknown
    /// here: the pending call fails with the code, `ConnectionLost` names it, no reconnect is
    /// attempted, and later calls answer with the same code instead of ConnectionClosed.
    [<Test>]
    [<CancelAfter(30_000)>]
    let ``a -404 frame fails the pending call, closes the client and does not reconnect`` () =
        use transport = new ScriptedTransport(testAuthKey)
        use client = connect transport
        let lost = TaskCompletionSource<MtProtoError>()
        client.ConnectionLost.Add(fun e -> lost.TrySetResult e |> ignore)

        let call = client.RpcAsync([| 0xDEuy; 0xADuy; 0xBEuy; 0xEFuy |], CancellationToken.None)
        Assert.That(transport.WaitForSends(1, isRpc, 5_000), Is.True, NUnitString "the request was never sent")

        transport.Push(Ok(BitConverter.GetBytes(-404)))

        match call.GetAwaiter().GetResult() with
        | Error(MtProtoError.TransportErrorCode -404) -> ()
        | other -> Assert.Fail($"expected TransportErrorCode -404, got %A{other}")

        Assert.That(lost.Task.Wait(5_000), Is.True, NUnitString "ConnectionLost was not raised")
        Assert.That(lost.Task.Result, Is.EqualTo(MtProtoError.TransportErrorCode -404))

        match client.RpcAsync([| 0xDEuy; 0xADuy; 0xBEuy; 0xEFuy |], CancellationToken.None).GetAwaiter().GetResult() with
        | Error(MtProtoError.TransportErrorCode -404) -> ()
        | other -> Assert.Fail($"a call on the closed client should answer -404, got %A{other}")

        Assert.That(transport.ConnectCount, Is.EqualTo 1, NUnitString "the client must not reconnect after -404")

    /// The session outlives the socket. A request in flight when the connection drops is re-sent
    /// under its original msg_id and seq_no once the connection is back, and the reply to that
    /// re-send completes the caller — it is not failed, and the caller does not retry under a new
    /// id, which is how a request the server had already executed used to run twice.
    [<Test>]
    [<CancelAfter(60_000)>]
    let ``a request in flight across a drop is re-sent under its original ids and completed`` () =
        use transport = new ScriptedTransport(testAuthKey)
        use client = connect transport

        let call = client.RpcAsync([| 0xDEuy; 0xADuy; 0xBEuy; 0xEFuy |], CancellationToken.None)
        Assert.That(transport.WaitForSends(1, isRpc, 5_000), Is.True, NUnitString "the request was never sent")
        let (msgId, seqNo, _) = transport.Sends |> List.find (fun (_, _, b) -> isRpc b)

        // The socket dies before any answer. The reconnect (1 s backoff) reuses the auth key and
        // must re-send the request as it was.
        transport.Push(Error TransportError.ConnectionClosed)
        Assert.That(transport.WaitForSends(2, isRpc, 15_000), Is.True, NUnitString "the request was not re-sent")

        let resent = transport.Sends |> List.filter (fun (_, _, b) -> isRpc b) |> List.last
        let (resentId, resentSeqNo, _) = resent
        Assert.That(resentId, Is.EqualTo msgId, NUnitString "re-sent under a different msg_id")
        Assert.That(resentSeqNo, Is.EqualTo seqNo, NUnitString "re-sent under a different seq_no")
        Assert.That(transport.ConnectCount, Is.EqualTo 2)

        transport.PushMessage(msgId + 1L, 1, rpcResult msgId [| 0x01uy; 0x02uy; 0x03uy; 0x04uy |])

        match call.GetAwaiter().GetResult() with
        | Ok [| 0x01uy; 0x02uy; 0x03uy; 0x04uy |] -> ()
        | other -> Assert.Fail($"expected the re-sent request's answer, got %A{other}")

    /// A migration is its own case: nobody should have to parse `FILE_MIGRATE_4` out of a string.
    [<Test>]
    let ``a 303 migrate error is split into its kind and DC`` () =
        Assert.That(MtProtoError.ofRpcError 303 "FILE_MIGRATE_4", Is.EqualTo(MtProtoError.Migrate("FILE", 4)))
        Assert.That(MtProtoError.ofRpcError 303 "PHONE_MIGRATE_2", Is.EqualTo(MtProtoError.Migrate("PHONE", 2)))
        Assert.That(MtProtoError.ofRpcError 303 "SEE_OTHER", Is.EqualTo(MtProtoError.RpcError(303, "SEE_OTHER")))

    /// A flood wait is its own case, the same way a migrate is: `FLOOD_WAIT_n` and
    /// `FLOOD_PREMIUM_WAIT_n` both mean the account must not call for `n` seconds.
    /// `SLOWMODE_WAIT_n` carries the same code and the same `_WAIT_` marker but is a per-chat
    /// limit, not an account-wide one, so it must stay a plain `RpcError`.
    [<Test>]
    let ``a 420 flood wait error is split into its seconds`` () =
        Assert.That(MtProtoError.ofRpcError 420 "FLOOD_WAIT_30", Is.EqualTo(MtProtoError.FloodWait 30))
        Assert.That(MtProtoError.ofRpcError 420 "FLOOD_PREMIUM_WAIT_7", Is.EqualTo(MtProtoError.FloodWait 7))
        Assert.That(MtProtoError.ofRpcError 420 "SLOWMODE_WAIT_5", Is.EqualTo(MtProtoError.RpcError(420, "SLOWMODE_WAIT_5")))
        Assert.That(MtProtoError.ofRpcError 400 "FLOOD_WAIT_30", Is.EqualTo(MtProtoError.RpcError(400, "FLOOD_WAIT_30")))
        Assert.That(MtProtoError.ofRpcError 420 "FLOOD_WAIT_x", Is.EqualTo(MtProtoError.RpcError(420, "FLOOD_WAIT_x")))
