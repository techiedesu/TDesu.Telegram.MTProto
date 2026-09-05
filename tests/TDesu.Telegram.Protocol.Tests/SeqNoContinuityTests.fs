namespace TDesu.Telegram.Protocol.Tests

open System
open System.Collections.Concurrent
open System.Threading
open System.Threading.Tasks
open TDesu.Crypto
open TDesu.MTProto
open TDesu.Serialization
open TDesu.Transport

/// Loopback that records the seqno of every frame the client sends.
///
/// The client's own frames are encrypted with x=0 and `MessageFraming` only exposes the x=8
/// (server→client) half, so reading back what we just sent needs the other direction spelled out
/// here — the same gap `RpcResultTests.LoopbackTransport` fills to recover a msg_id.
type internal SeqNoRecordingTransport(authKey: AuthKey, buildReply: int * int64 -> byte[] option) =
    let mutable connected = false
    let replies = ConcurrentQueue<byte[]>()
    let ready = new SemaphoreSlim(0)
    let sent = ConcurrentQueue<int32>()
    let mutable count = 0

    member _.SentSeqNos = sent |> List.ofSeq

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
            let plain = AesIge.decrypt encrypted aes.Key aes.Iv

            let inner = TlReadBuffer(plain)
            inner.ReadInt64() |> ignore // salt
            let sessionId = inner.ReadInt64()
            let msgId = inner.ReadInt64()
            let seqNo = inner.ReadInt32()

            sent.Enqueue seqNo
            let n = Interlocked.Increment(&count)

            match buildReply (n, msgId) with
            | Some body ->
                let sess = Session.createSession ()
                sess.SessionId <- sessionId
                replies.Enqueue(MessageFramingTests.serverEncrypt authKey sess (msgId + 1L) 1 body)
                ready.Release() |> ignore
            | None -> ()

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

namespace TDesu.Telegram.Protocol.Tests

open System.Threading
open NUnit.Framework
open TDesu.MTProto
open TDesu.MTProto.Service
open TDesu.Serialization
open TDesu.Transport

/// Content-related messages carry a strictly increasing, gapless seqno. A number handed out and
/// then never put on the wire is a hole the server reads as a message it lost — it stops accepting
/// the session and nothing short of a new one recovers it.
///
/// `resendRequest` used to take its seqno before deciding whether it had anything to re-send.
/// `Rekey` fails whenever the request has already moved, and `TryGetBody` resolves through the
/// redirect chain while `Rekey` does not — so a second `bad_server_salt` naming the same original
/// msg_id finds a body, fails to re-key, and returns having consumed a number it never used.
[<TestFixture>]
module SeqNoContinuityTests =

    let private testAuthKey: AuthKey = {
        Data = Array.init 256 (fun i -> byte ((i * 13 + 9) % 256))
        Id = 0x778899AABBCCDDEEL
        AuxHash = 0L
    }

    // bad_server_salt#edab447b bad_msg_id:long bad_msg_seqno:int error_code:int new_server_salt:long
    let private buildBadServerSalt (badMsgId: int64) : byte[] =
        use w = new TlWriteBuffer()

        Requests.BadMsgNotification.Serialize(
            w,
            Requests.BadMsgNotification.BadServerSalt(badMsgId, 1, 48, 0x5A17L)
        )

        w.ToArray()

    // rpc_result and msg_container are the two the schema marks parsed-manually, so there is no
    // generated writer for them; everything else here is built with one.
    let private buildRpcResultBody (reqMsgId: int64) (result: byte[]) : byte[] =
        use w = new TlWriteBuffer()
        w.WriteConstructorId(0xf35c6d01u)
        w.WriteInt64(reqMsgId)
        w.WriteRawBytes(result)
        w.ToArray()

    let private buildContainer (parts: (int64 * int * byte[]) list) : byte[] =
        use w = new TlWriteBuffer()
        w.WriteConstructorId(0x73f1f8dcu)
        w.WriteInt32(parts.Length)

        for msgId, seqNo, body in parts do
            w.WriteInt64(msgId)
            w.WriteInt32(seqNo)
            w.WriteInt32(body.Length)
            w.WriteRawBytes(body)

        w.ToArray()

    [<Test>]
    let ``a re-send that cannot re-key does not consume a content seqno`` () =
        let payload = [| 5uy; 6uy; 7uy; 8uy |]

        // The first request is answered by a container holding two bad_server_salts for its own
        // msg_id plus the real result. The first salt re-keys the request; the second finds it
        // already moved, which is the case that used to burn a number.
        let reply (n: int, msgId: int64) =
            if n = 1 then
                Some(
                    buildContainer [
                        msgId + 2L, 2, buildBadServerSalt msgId
                        msgId + 4L, 4, buildBadServerSalt msgId
                        msgId + 6L, 5, buildRpcResultBody msgId payload
                    ]
                )
            else
                // Re-sends and the second call all get a plain result for whatever they asked.
                Some(buildRpcResultBody msgId payload)

        use transport = new SeqNoRecordingTransport(testAuthKey, reply)

        use client =
            new MtProtoClient(DataCenters.defaultDc, transportFactory = fun _ -> transport :> ITransport)

        match client.ConnectWithAuthKeyAsync(testAuthKey, 1L, 0, CancellationToken.None).GetAwaiter().GetResult() with
        | Error e -> Assert.Fail($"stub connect failed: %A{e}")
        | Ok() -> ()

        client.RpcAsync([| 1uy; 2uy; 3uy; 4uy |], CancellationToken.None).GetAwaiter().GetResult()
        |> ignore

        // Let both re-send tasks finish; they run off the receive loop via Task.Run.
        Thread.Sleep 750

        client.RpcAsync([| 1uy; 2uy; 3uy; 4uy |], CancellationToken.None).GetAwaiter().GetResult()
        |> ignore

        // Content seqnos are the odd ones; acks and pings take the even ones and are not due yet.
        let content = transport.SentSeqNos |> List.filter (fun s -> s % 2 = 1) |> List.sort

        let gaps =
            content
            |> List.pairwise
            |> List.filter (fun (a, b) -> b - a <> 2)

        Assert.That(
            gaps,
            Is.Empty,
            NUnitString $"content seqnos must be gapless, got %A{content}"
        )
