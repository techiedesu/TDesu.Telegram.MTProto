namespace TDesu.Telegram.Protocol.Tests

open System.IO
open System.IO.Compression
open System.Threading
open NUnit.Framework
open TDesu.MTProto
open TDesu.Serialization
open TDesu.Transport

/// `msg_container` and `gzip_packed` are flat, single-level wrappers in the schema -- neither is
/// modelled as wrapping itself. `processInnerMessage` caps how many layers of either it unwraps in
/// a row, so a message wrapped past that point is dropped at a fixed, shallow point instead of
/// being unwrapped for as many layers as the bytes keep describing.
[<TestFixture>]
module MessageNestingTests =

    let private testAuthKey: AuthKey = {
        Data = Array.init 256 (fun i -> byte ((i * 13 + 7) % 256))
        Id = 0x2244668800AACCEEL
        AuxHash = 0L
    }

    // gzip_packed#3072cfa1 and msg_container#73f1f8dc are read by hand on the client and have no
    // generated writer -- same literals every other test building one of these redefines locally.
    let private gzipPack (payload: byte array) : byte array =
        use compressed = new MemoryStream()

        do
            use gz = new GZipStream(compressed, CompressionMode.Compress, true)
            gz.Write(payload, 0, payload.Length)

        use w = new TlWriteBuffer()
        w.WriteConstructorId(0x3072cfa1u)
        w.WriteBytes(compressed.ToArray())
        w.ToArray()

    let rec private nestGzip (depth: int) (leaf: byte array) : byte array =
        if depth <= 0 then leaf else nestGzip (depth - 1) (gzipPack leaf)

    let private container (entries: (int64 * int32 * byte array) list) =
        use w = new TlWriteBuffer()
        w.WriteConstructorId(0x73f1f8dcu)
        w.WriteInt32 entries.Length

        for msgId, seqNo, body in entries do
            w.WriteInt64 msgId
            w.WriteInt32 seqNo
            w.WriteInt32 body.Length
            w.WriteRawBytes body

        w.ToArray()

    let rec private nestContainer (depth: int) (msgId: int64) (leaf: byte array) : byte array =
        if depth <= 0 then
            leaf
        else
            nestContainer (depth - 1) (msgId + 1L) (container [ (msgId, 2 * depth + 1, leaf) ])

    let private rpcResult (reqMsgId: int64) (result: byte array) =
        use w = new TlWriteBuffer()
        w.WriteConstructorId(0xf35c6d01u)
        w.WriteInt64 reqMsgId
        w.WriteRawBytes result
        w.ToArray()

    let private connectedClient (transport: LoopbackTransport) =
        let client =
            new MtProtoClient(DataCenters.defaultDc, transportFactory = fun _ -> transport :> ITransport)

        match client.ConnectWithAuthKeyAsync(testAuthKey, 1L, 0, CancellationToken.None).GetAwaiter().GetResult() with
        | Error e -> Assert.Fail($"stub connect failed: %A{e}")
        | Ok() -> ()

        client

    /// The nested entry travels beside an ordinary `rpc_result` in one container, so the call
    /// completing is a deterministic finish line: by the time it returns, both entries have
    /// already been dispatched, so asserting on what the nested one did cannot race the receive
    /// loop. Not a recognized constructor at the core, so if it were unwrapped all the way down it
    /// would be published as an update -- the one observable side effect this test checks for.
    let private runNested (wrapped: byte array) =
        let unwrappedLeafPayload = [| 0xDEuy; 0xADuy; 0xBEuy; 0xEFuy |]
        let mutable updates = 0

        use transport =
            new LoopbackTransport(
                testAuthKey,
                fun reqMsgId ->
                    container [
                        reqMsgId + 2L, 2, wrapped
                        reqMsgId + 4L, 3, rpcResult reqMsgId [| 9uy; 9uy; 9uy; 9uy |]
                    ]
            )

        use client = connectedClient transport
        client.UpdateReceived.Add(fun bytes -> if bytes = unwrappedLeafPayload then updates <- updates + 1)

        match client.RpcAsync([| 1uy; 2uy; 3uy; 4uy |], CancellationToken.None).GetAwaiter().GetResult() with
        | Ok _ -> ()
        | Error e -> Assert.Fail($"expected the call to complete, got %A{e}")

        updates

    [<Test>]
    let ``a gzip_packed wrapped past the depth cap is dropped without unpacking the innermost layer`` () =
        let leaf = [| 0xDEuy; 0xADuy; 0xBEuy; 0xEFuy |]
        Assert.That(runNested (nestGzip 10 leaf), Is.EqualTo 0)

    [<Test>]
    let ``a msg_container nested past the depth cap is dropped without reaching the innermost layer`` () =
        let leaf = [| 0xDEuy; 0xADuy; 0xBEuy; 0xEFuy |]
        Assert.That(runNested (nestContainer 10 900L leaf), Is.EqualTo 0)
