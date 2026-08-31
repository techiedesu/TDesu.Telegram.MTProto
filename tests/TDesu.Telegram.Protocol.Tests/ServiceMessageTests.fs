namespace TDesu.Telegram.Protocol.Tests

open System.Threading
open NUnit.Framework
open NUnit.Framework.Legacy
open TDesu.MTProto
open TDesu.Serialization
open TDesu.Transport

/// `msg_detailed_info` and `msg_new_detailed_info` are transport bookkeeping from mtproto.tl, not
/// API objects. They used to fall through the service-message dispatch into the same branch as a
/// server push, so every one of them was handed to update subscribers — which then failed
/// deserializing an `Updates` and logged a real error for a message that never carried an update.
/// Measured against a live account, 28 arrived in one hour and each produced one such error, which
/// is also how they hid genuine parse failures.
///
/// Both name an *answer* the server is holding, and both must be acked by that answer's id rather
/// than their own; until they are, the server keeps re-announcing the same answer, which is why
/// there were 28 rather than one.
[<TestFixture>]
module ServiceMessageTests =

    let private testAuthKey: AuthKey = {
        Data = Array.init 256 (fun i -> byte ((i * 7 + 3) % 256))
        Id = 0x1122334455667788L
        AuxHash = 0L
    }

    // msg_new_detailed_info#809db6df answer_msg_id:long bytes:int status:int
    let private buildNewDetailedInfo (answerMsgId: int64) : byte[] =
        use w = new TlWriteBuffer()
        w.WriteConstructorId(0x809db6dfu)
        w.WriteInt64(answerMsgId)
        w.WriteInt32(64)
        w.WriteInt32(1)
        w.ToArray()

    // msg_detailed_info#276d3ec6 msg_id:long answer_msg_id:long bytes:int status:int
    let private buildDetailedInfo (msgId: int64) (answerMsgId: int64) : byte[] =
        use w = new TlWriteBuffer()
        w.WriteConstructorId(0x276d3ec6u)
        w.WriteInt64(msgId)
        w.WriteInt64(answerMsgId)
        w.WriteInt32(64)
        w.WriteInt32(1)
        w.ToArray()

    // rpc_result#f35c6d01 req_msg_id:long result:Object = RpcResult
    let private buildRpcResultBody (reqMsgId: int64) (result: byte[]) : byte[] =
        use w = new TlWriteBuffer()
        w.WriteConstructorId(0xf35c6d01u)
        w.WriteInt64(reqMsgId)
        w.WriteRawBytes(result)
        w.ToArray()

    // msg_container#73f1f8dc count:int { msg_id:long seqno:int bytes:int body }
    //
    // The service message travels in a container beside the RPC result the call is waiting for.
    // That gives the test a deterministic finish line: when the call returns, the container — and
    // therefore the service message — has already been processed, so an assertion about what the
    // subscriber saw cannot race the receive loop.
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

    let private connectedClient (transport: LoopbackTransport) =
        let client =
            new MtProtoClient(DataCenters.defaultDc, transportFactory = fun _ -> transport :> ITransport)

        match client.ConnectWithAuthKeyAsync(testAuthKey, 1L, 0, CancellationToken.None).GetAwaiter().GetResult() with
        | Error e -> Assert.Fail($"stub connect failed: %A{e}")
        | Ok() -> ()

        client

    /// The payload is deliberately four bytes that are not a valid `Updates`: if the client ever
    /// forwards this to subscribers again, the consumer's deserializer throws exactly as it did in
    /// production. The assertion is on the count rather than the content so the failure message says
    /// what happened rather than what could not be parsed.
    let private runWithService (serviceBody: byte[]) =
        let payload = [| 9uy; 8uy; 7uy; 6uy |]
        let mutable updates = 0

        use transport =
            new LoopbackTransport(
                testAuthKey,
                fun reqMsgId ->
                    buildContainer [
                        reqMsgId + 2L, 2, serviceBody
                        reqMsgId + 4L, 3, buildRpcResultBody reqMsgId payload
                    ]
            )

        use client = connectedClient transport
        client.UpdateReceived.Add(fun _ -> Interlocked.Increment(&updates) |> ignore)

        let result = client.RpcAsync([| 1uy; 2uy; 3uy; 4uy |], CancellationToken.None).GetAwaiter().GetResult()

        match result with
        | Ok bytes -> CollectionAssert.AreEqual(payload, bytes)
        | Error e -> Assert.Fail($"expected the call to complete, got %A{e}")

        updates

    [<Test>]
    let ``msg_new_detailed_info is consumed by the transport, not published as an update`` () =
        Assert.That(runWithService (buildNewDetailedInfo 0x51A0L), Is.EqualTo 0)

    [<Test>]
    let ``msg_detailed_info is consumed by the transport, not published as an update`` () =
        Assert.That(runWithService (buildDetailedInfo 0x4200L 0x51A0L), Is.EqualTo 0)
