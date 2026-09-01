namespace TDesu.Telegram.Protocol.Tests

open System
open System.Collections.Concurrent
open System.Threading
open System.Threading.Tasks
open TDesu.Crypto
open TDesu.MTProto
open TDesu.MTProto.Service
open TDesu.Serialization
open TDesu.Transport

/// Loopback that records every `msgs_ack` the client sends, stalls the first one, and can drop the
/// connection on demand.
///
/// The stall is the whole point. The flush takes its ids out of the queue before it writes, so a
/// teardown landing inside that write is what used to lose them — holding the write open is how the
/// test cancels it deterministically instead of racing a ten-second timer.
type internal AckStallingTransport(authKey: AuthKey, stall: SemaphoreSlim) =
    let mutable connected = false
    let ackedIds = ConcurrentQueue<int64>()
    let mutable ackSends = 0
    let frames = ConcurrentQueue<byte[]>()
    let ready = new SemaphoreSlim(0)
    let mutable dropNext = false
    let mutable contentMsgId = 0L

    /// A body no service branch claims, so the dispatch takes the update arm — and an odd seqno,
    /// which is what makes a message content-related and therefore owed an acknowledgement.
    let contentBody = [| 0xEFuy; 0xBEuy; 0xADuy; 0xDEuy |]

    let push (frame: byte[]) =
        frames.Enqueue frame
        ready.Release() |> ignore

    /// Every msg_id this client has acknowledged, across every flush.
    member _.AckedIds = ackedIds |> List.ofSeq

    member _.AckSendCount = Volatile.Read &ackSends

    /// The id of the content message the client owes an ack for.
    member _.ContentMsgId = Volatile.Read &contentMsgId

    /// Make the reader observe a dead connection, which is what drives `reconnectInternal`.
    member _.DropConnection() =
        dropNext <- true
        ready.Release() |> ignore

    interface ITransport with
        member _.IsConnected = connected

        member _.ConnectAsync(_) =
            connected <- true
            Task.FromResult(Ok())

        member _.SendAsync(payload, ct) =
            task {
                use reader = new TlReadBuffer(payload)
                reader.ReadInt64() |> ignore // auth_key_id
                let msgKey = reader.ReadRawBytes(16)
                let encrypted = reader.ReadRawBytes(payload.Length - 24)
                let aes = KeyDerivation.deriveAesKeyIv authKey.Data msgKey 0
                let plain = AesIge.decrypt encrypted aes.Key aes.Iv

                use inner = new TlReadBuffer(plain)
                inner.ReadInt64() |> ignore // salt
                let sessionId = inner.ReadInt64()
                let msgId = inner.ReadInt64()
                inner.ReadInt32() |> ignore // seq_no
                let bodyLength = inner.ReadInt32()
                let body = inner.ReadRawBytes(bodyLength)

                let isAck =
                    body.Length >= 4 && BitConverter.ToUInt32(body, 0) = GeneratedCid.MsgsAck

                if isAck then
                    let n = Interlocked.Increment &ackSends

                    // Only the first flush stalls; the one after the reconnect has to get through so
                    // the test can see what it carried.
                    if n = 1 then
                        do! stall.WaitAsync(ct)

                    use r = new TlReadBuffer(body)

                    for id in (Requests.MsgsAck.Deserialize r).MsgIds do
                        ackedIds.Enqueue id
                else
                    // The client's session id is only knowable from what it sends, which is why the
                    // content frame is minted here rather than by the test.
                    let sess = Session.createSession ()
                    sess.SessionId <- sessionId

                    if Volatile.Read &contentMsgId = 0L then
                        let contentId = msgId + 2L
                        Volatile.Write(&contentMsgId, contentId)
                        push (MessageFramingTests.serverEncrypt authKey sess contentId 3 contentBody)

                return Ok()
            }

        member _.ReceiveAsync(ct) =
            task {
                do! ready.WaitAsync(ct)

                if dropNext then
                    dropNext <- false
                    connected <- false
                    return Error TransportError.ConnectionClosed
                else
                    let mutable frame = Array.empty

                    return
                        if frames.TryDequeue(&frame) then
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
open TDesu.Transport

/// Telegram retransmits every content message it has not seen acknowledged, and eventually drops a
/// session it judges unacked altogether. The flush takes its batch out of the queue before writing,
/// so whatever interrupts that write owes the queue those ids back.
///
/// A *refused* send already requeued. A *cancelled* one threw straight past that arm to the loop's
/// own handler and the batch went with it — which is exactly what a reconnect does to a flush in
/// flight, and a reconnect is the most common event in this client's life.
///
/// Slow by nature: the flush runs on a ten-second timer with nothing to inject, so this waits for
/// two of them rather than pretending it can hurry the clock.
[<TestFixture>]
module AckRequeueTests =

    let private testAuthKey: AuthKey = {
        Data = Array.init 256 (fun i -> byte ((i * 17 + 11) % 256))
        Id = 0x0A0B0C0D0E0F1011L
        AuxHash = 0L
    }

    [<Test>]
    [<CancelAfter(120_000)>]
    let ``a flush cancelled mid-send gives its ids back to the queue`` () =
        use stall = new SemaphoreSlim(0)
        use transport = new AckStallingTransport(testAuthKey, stall)

        use client =
            new MtProtoClient(DataCenters.defaultDc, transportFactory = fun _ -> transport :> ITransport)

        match client.ConnectWithAuthKeyAsync(testAuthKey, 1L, 0, CancellationToken.None).GetAwaiter().GetResult() with
        | Error e -> Assert.Fail($"stub connect failed: %A{e}")
        | Ok() -> ()

        // One RPC, purely to teach the transport the client's session id; the RPC itself is never
        // answered and is expected to time out, which the test does not wait for.
        client.RpcAsync([| 1uy; 2uy; 3uy; 4uy |], CancellationToken.None) |> ignore

        // The content frame the transport mints in response is what the client now owes an ack for.
        Assert.That(
            SpinWait.SpinUntil((fun () -> transport.ContentMsgId <> 0L), 10_000),
            Is.True,
            NUnitString "the transport never got a send to learn the session id from"
        )

        let owed = transport.ContentMsgId

        // First flush: the ten-second timer fires and the write stalls inside the transport.
        Assert.That(
            SpinWait.SpinUntil((fun () -> transport.AckSendCount > 0), 30_000),
            Is.True,
            NUnitString "the ack loop never attempted its first flush"
        )

        // Nothing has been acknowledged yet — the write is still parked.
        Assert.That(transport.AckedIds, Is.Empty)

        // Drop the connection. The reconnect stops the ack loop, which cancels the parked write:
        // the ids are already out of the queue, so only the requeue puts them back.
        transport.DropConnection()

        // The replacement loop flushes on its own timer. Without the requeue there is nothing left
        // to flush and this never becomes true.
        let acknowledged =
            SpinWait.SpinUntil((fun () -> transport.AckedIds |> List.contains owed), 60_000)

        Assert.That(
            acknowledged,
            Is.True,
            NUnitString
                $"msg_id %d{owed} was dequeued by the cancelled flush and never acknowledged; \
                  acks seen: %A{transport.AckedIds}"
        )
