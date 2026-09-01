namespace TDesu.Telegram.Protocol.Tests

open System
open System.Collections.Concurrent
open System.Threading
open System.Threading.Tasks
open TDesu.Transport

/// A transport that holds every read open until the test lets it go, records the high-water mark of
/// readers inside it, and can stall its own connect so a test can hold the reconnect window open.
///
/// Read *counts* prove nothing here — a loop that gets `Timeout` legitimately reads again. What
/// matters is who is inside the carrier and when: a read that lands on a replacement before that
/// replacement has even finished connecting cannot be the new generation's, because the new
/// generation does not exist yet.
type internal TrackingTransport(id: int, gate: SemaphoreSlim, connectGate: SemaphoreSlim option) =
    let mutable connected = false
    let mutable inFlight = 0
    let mutable peakInFlight = 0

    let mutable connectEntered = false

    member _.Id = id
    member _.PeakConcurrentReaders = Volatile.Read(&peakInFlight)
    member _.ConnectEntered = Volatile.Read(&connectEntered)

    interface ITransport with
        member _.IsConnected = connected

        member _.ConnectAsync(_) =
            task {
                Volatile.Write(&connectEntered, true)

                // Reported connected *before* the stall on purpose. That is the shape of the real
                // window: the socket comes up, `IsConnected` flips, and the reconnect then spends a
                // whole DH exchange in `performExchange` before it starts the new receive loop. A
                // stub that stayed unconnected would let the old loop exit on its `IsConnected`
                // guard and hide the very race this is here to catch.
                connected <- true

                match connectGate with
                | Some g -> do! g.WaitAsync()
                | None -> ()

                return Ok()
            }

        member _.SendAsync(_, _) = Task.FromResult(Ok())

        member _.ReceiveAsync(ct) =
            task {
                let now = Interlocked.Increment(&inFlight)

                let mutable seen = Volatile.Read(&peakInFlight)

                while now > seen
                      && Interlocked.CompareExchange(&peakInFlight, now, seen) <> seen do
                    seen <- Volatile.Read(&peakInFlight)

                try
                    do! gate.WaitAsync(ct)
                    // Timeout is the one receive error the loop treats as "nothing happened, read
                    // again" — the step that walks it onto whatever `transport` now points at.
                    return Error TransportError.Timeout
                finally
                    Interlocked.Decrement(&inFlight) |> ignore
            }

        member _.Disconnect() = connected <- false

    interface IDisposable with
        member _.Dispose() = ()

namespace TDesu.Telegram.Protocol.Tests

open System.Collections.Concurrent
open System.IO
open System.IO.Compression
open System.Threading
open NUnit.Framework
open NUnit.Framework.Legacy
open TDesu.MTProto
open TDesu.Serialization
open TDesu.Transport

/// The receive path's two structural hazards: what it does with a packed result, and what it does
/// with the previous connection's reader when the carrier is replaced underneath it.
[<TestFixture>]
module ReceivePathTests =

    let private testAuthKey: AuthKey = {
        Data = Array.init 256 (fun i -> byte ((i * 7 + 3) % 256))
        Id = 0x33445566778899AAL
        AuxHash = 0L
    }

    // rpc_result#f35c6d01 and gzip_packed#3072cfa1 are the two constructors `mtproto.tl` comments
    // out as parsed manually, so there is no generated writer to build them with — unlike every
    // other service message, which the test suite and the client both get from td-tl-gen.
    let private buildRpcResultBody (reqMsgId: int64) (result: byte[]) : byte[] =
        use w = new TlWriteBuffer()
        w.WriteConstructorId(0xf35c6d01u)
        w.WriteInt64(reqMsgId)
        w.WriteRawBytes(result)
        w.ToArray()

    let private gzipPack (payload: byte[]) : byte[] =
        use compressed = new MemoryStream()

        do
            use gz = new GZipStream(compressed, CompressionMode.Compress, true)
            gz.Write(payload, 0, payload.Length)

        use w = new TlWriteBuffer()
        w.WriteConstructorId(0x3072cfa1u)
        w.WriteBytes(compressed.ToArray())
        w.ToArray()

    /// The packed result is read straight off the reader that is already positioned on it, instead
    /// of slicing the frame's tail into a fresh array first. That slice was a full-size copy thrown
    /// away one line later, on exactly the large results that arrive packed — but the cheaper path
    /// is only worth anything if it still decodes, which is what this pins.
    [<Test>]
    let ``a gzip_packed result is inflated and handed to the caller`` () =
        // Long and repetitive so gzip actually compresses, i.e. the packed branch is the one taken.
        let payload = Array.init 4096 (fun i -> byte (i % 7))

        use transport =
            new LoopbackTransport(testAuthKey, fun reqMsgId -> buildRpcResultBody reqMsgId (gzipPack payload))

        use client =
            new MtProtoClient(DataCenters.defaultDc, transportFactory = fun _ -> transport :> ITransport)

        match client.ConnectWithAuthKeyAsync(testAuthKey, 1L, 0, CancellationToken.None).GetAwaiter().GetResult() with
        | Error e -> Assert.Fail($"stub connect failed: %A{e}")
        | Ok() -> ()

        match client.RpcAsync([| 1uy; 2uy; 3uy; 4uy |], CancellationToken.None).GetAwaiter().GetResult() with
        | Ok bytes -> CollectionAssert.AreEqual(payload, bytes)
        | Error e -> Assert.Fail($"expected the inflated payload, got %A{e}")

    /// A reconnect swaps a mutable field that the live reader dereferences on every iteration, and
    /// the old reader used to be stopped only *after* the replacement was connected. In between, a
    /// reader that woke up followed the swapped field onto a carrier it knew nothing about, racing
    /// the generation that was about to own it for frames belonging to a session it never sent on.
    ///
    /// The window is a real one — on the wire it spans a TCP connect, and a full DH exchange when
    /// the key has to be renegotiated — so the test holds the replacement's connect open to observe
    /// it deterministically rather than hoping to hit the race.
    [<Test>]
    let ``a superseded reader never reads the transport that replaced it`` () =
        use gate = new SemaphoreSlim(0)
        use connectGate = new SemaphoreSlim(0)
        let created = ResizeArray<TrackingTransport>()

        let factory _ =
            // Only the replacement stalls; the initial connect must go straight through.
            let stall = if created.Count = 0 then None else Some connectGate
            let t = new TrackingTransport(created.Count + 1, gate, stall)
            created.Add t
            t :> ITransport

        use client =
            new MtProtoClient(DataCenters.defaultDc, transportFactory = factory)

        match client.ConnectWithAuthKeyAsync(testAuthKey, 1L, 0, CancellationToken.None).GetAwaiter().GetResult() with
        | Error e -> Assert.Fail($"stub connect failed: %A{e}")
        | Ok() -> ()

        // Let the first reader reach its blocking read before anything is swapped.
        SpinWait.SpinUntil((fun () -> created[0].PeakConcurrentReaders > 0), 5000) |> ignore

        // Taking the carrier down is what makes RpcAsync drive the reconnect itself, with the
        // original reader still parked inside the old instance. It blocks on the stalled connect,
        // so it runs off-thread.
        (created[0] :> ITransport).Disconnect()

        let rpc =
            client.RpcAsync([| 1uy; 2uy; 3uy; 4uy |], CancellationToken.None)

        // Wait until the replacement exists and is mid-connect: `transport` now points at it and
        // no new receive loop has been started yet. This is the whole window.
        Assert.That(
            SpinWait.SpinUntil((fun () -> created.Count > 1 && created[1].ConnectEntered), 15000),
            Is.True,
            NUnitString "the reconnect never reached the replacement's connect"
        )

        // Wake the original reader inside the window. Uncancelled, it returns from the old
        // instance, recurses, and reads whatever `transport` points at — the replacement.
        gate.Release 4 |> ignore
        Thread.Sleep 500

        let readDuringWindow = created[1].PeakConcurrentReaders

        // Let the reconnect finish so the client is not left half-built.
        connectGate.Release() |> ignore
        rpc.GetAwaiter().GetResult() |> ignore

        Assert.That(
            readDuringWindow,
            Is.EqualTo 0,
            NUnitString "nothing may read the replacement before its own receive loop exists"
        )
