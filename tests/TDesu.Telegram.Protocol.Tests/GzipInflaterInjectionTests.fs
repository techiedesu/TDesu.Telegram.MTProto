namespace TDesu.Telegram.Protocol.Tests

open System
open System.IO
open System.IO.Compression
open System.Threading
open NUnit.Framework
open NUnit.Framework.Legacy
open TDesu.MTProto
open TDesu.Serialization
open TDesu.Transport

/// A fake `IGzipInflater` that counts every call and defers its actual answer to `behavior`, so a
/// test can tell "`MtProtoClient` consulted the inflater it was given" apart from "decompression
/// happened to produce the right bytes" -- the conformance suite already covers the latter.
type private RecordingInflater(behavior: byte[] -> int -> Result<byte[], InflateError>) =
    let mutable callCount = 0

    member _.CallCount = Volatile.Read(&callCount)

    interface IGzipInflater with
        member _.Inflate(packed: ReadOnlySpan<byte>, maxOutputBytes: int) =
            Interlocked.Increment(&callCount) |> ignore
            behavior (packed.ToArray()) maxOutputBytes

/// `GzipInflater.platform`, the default, is exercised throughout the rest of this suite
/// (`ReceivePathTests`, `MessageNestingTests`, ...). These pin that `MtProtoClient` actually calls
/// whatever `IGzipInflater` its constructor was given -- at both of its call sites, not just one --
/// and that an `Error` from it is dropped instead of thrown.
[<TestFixture>]
module GzipInflaterInjectionTests =

    let private testAuthKey: AuthKey = {
        Data = Array.init 256 (fun i -> byte ((i * 17 + 11) % 256))
        Id = 0x1133557799BBDDFFL
        AuxHash = 0L
    }

    // rpc_result#f35c6d01 and gzip_packed#3072cfa1 are read by hand on the client and have no
    // generated writer -- same literals every other test building one of these redefines locally.
    let private buildRpcResultBody (reqMsgId: int64) (result: byte[]) : byte[] =
        use w = new TlWriteBuffer()
        w.WriteConstructorId(0xf35c6d01u)
        w.WriteInt64(reqMsgId)
        w.WriteRawBytes(result)
        w.ToArray()

    let private gzipPack (payload: byte array) : byte array =
        use compressed = new MemoryStream()

        do
            use gz = new GZipStream(compressed, CompressionMode.Compress, true)
            gz.Write(payload, 0, payload.Length)

        use w = new TlWriteBuffer()
        w.WriteConstructorId(0x3072cfa1u)
        w.WriteBytes(compressed.ToArray())
        w.ToArray()

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

    let private connectedClient (transport: LoopbackTransport) (inflater: IGzipInflater) (responseTimeout: TimeSpan) =
        let client =
            new MtProtoClient(
                DataCenters.defaultDc,
                transportFactory = (fun _ -> transport :> ITransport),
                inflater = inflater,
                responseTimeout = responseTimeout
            )

        match client.ConnectWithAuthKeyAsync(testAuthKey, 1L, 0, CancellationToken.None).GetAwaiter().GetResult() with
        | Error e -> Assert.Fail($"stub connect failed: %A{e}")
        | Ok() -> ()

        client

    /// The real decode still has to come out right: a recording wrapper that silently swallowed
    /// bytes, or that a future refactor stopped calling, would pass a bare call-count assertion
    /// while breaking traffic -- so every test here also checks the decompressed bytes themselves.
    let private realInflater () =
        RecordingInflater(fun packed limit -> GzipInflater.platform.Inflate(ReadOnlySpan packed, limit))

    [<Test>]
    let ``processRpcResult's gzip_packed branch calls the injected inflater`` () =
        let payload = Array.init 4096 (fun i -> byte (i % 7))
        let recorder = realInflater ()

        use transport =
            new LoopbackTransport(testAuthKey, fun reqMsgId -> buildRpcResultBody reqMsgId (gzipPack payload))

        use client = connectedClient transport recorder (TimeSpan.FromSeconds 30.0)

        match client.RpcAsync([| 1uy; 2uy; 3uy; 4uy |], CancellationToken.None).GetAwaiter().GetResult() with
        | Ok bytes ->
            CollectionAssert.AreEqual(payload, bytes)
            Assert.That(recorder.CallCount, Is.EqualTo 1)
        | Error e -> Assert.Fail($"expected the inflated payload, got %A{e}")

    [<Test>]
    let ``processInnerMessage's gzip_packed branch calls the injected inflater`` () =
        let updatePayload = [| 0xDEuy; 0xADuy; 0xBEuy; 0xEFuy |]
        let anchorPayload = [| 9uy; 9uy; 9uy; 9uy |]
        let recorder = realInflater ()

        // The nested gzip_packed travels beside an ordinary rpc_result in one container -- same
        // technique `MessageNestingTests` uses -- so the RPC call completing is a deterministic
        // finish line: both entries have already been dispatched by the time it returns.
        use transport =
            new LoopbackTransport(
                testAuthKey,
                fun reqMsgId ->
                    container [
                        reqMsgId + 2L, 2, gzipPack updatePayload
                        reqMsgId + 4L, 3, buildRpcResultBody reqMsgId anchorPayload
                    ]
            )

        use client = connectedClient transport recorder (TimeSpan.FromSeconds 30.0)
        let mutable receivedUpdate: byte[] = [||]
        client.UpdateReceived.Add(fun bytes -> receivedUpdate <- bytes)

        match client.RpcAsync([| 1uy; 2uy; 3uy; 4uy |], CancellationToken.None).GetAwaiter().GetResult() with
        | Ok bytes ->
            CollectionAssert.AreEqual(anchorPayload, bytes)
            CollectionAssert.AreEqual(updatePayload, receivedUpdate)
            // Only the update's gzip_packed is compressed; the anchor rpc_result is not, so this
            // isolates processInnerMessage's own branch from processRpcResult's.
            Assert.That(recorder.CallCount, Is.EqualTo 1)
        | Error e -> Assert.Fail($"expected the anchor result, got %A{e}")

    [<Test>]
    let ``an Error from the inflater drops the message, without throwing, and the connection keeps serving requests`` () =
        let payload = Array.init 4096 (fun i -> byte (i % 7))
        let erroringInflater = RecordingInflater(fun _ _ -> Error(InflateError.Malformed "dropped for test"))
        let mutable callNumber = 0

        // The first reply's gzip_packed always fails to decompress; every later reply is the same
        // payload sent uncompressed, so a second RPC on the same client proves the dropped message
        // didn't take the connection down with it.
        use transport =
            new LoopbackTransport(
                testAuthKey,
                fun reqMsgId ->
                    callNumber <- callNumber + 1

                    if callNumber = 1 then
                        buildRpcResultBody reqMsgId (gzipPack payload)
                    else
                        buildRpcResultBody reqMsgId payload
            )

        use client = connectedClient transport erroringInflater (TimeSpan.FromMilliseconds 200.0)

        match client.RpcAsync([| 1uy; 2uy; 3uy; 4uy |], CancellationToken.None).GetAwaiter().GetResult() with
        | Error MtProtoError.Timeout -> ()
        | other -> Assert.Fail($"expected the dropped message to time out, not %A{other}")

        match client.RpcAsync([| 5uy; 6uy; 7uy; 8uy |], CancellationToken.None).GetAwaiter().GetResult() with
        | Ok bytes -> CollectionAssert.AreEqual(payload, bytes)
        | Error e -> Assert.Fail($"expected the connection to keep serving requests, got %A{e}")

        Assert.That(erroringInflater.CallCount, Is.EqualTo 1)
