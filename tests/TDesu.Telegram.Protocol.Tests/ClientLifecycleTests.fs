namespace TDesu.Telegram.Protocol.Tests

open System.Diagnostics
open System.Threading
open NUnit.Framework
open TDesu.MTProto
open TDesu.Transport

/// A client that has been disconnected has no reader left to complete a reply and no reconnect on
/// the way, so an RPC on it must fail at once. When it stalled instead, a single forced disconnect
/// turned into hours of every call burning its reconnect wait before failing.
[<TestFixture>]
module ClientLifecycleTests =

    [<Test>]
    let ``an RPC on a disconnected client fails at once`` () =
        use client = new MtProtoClient(DataCenters.defaultDc)
        client.Disconnect()

        let sw = Stopwatch.StartNew()

        let result =
            client.RpcAsync([| 1uy; 2uy; 3uy; 4uy |], CancellationToken.None).GetAwaiter().GetResult()

        sw.Stop()

        match result with
        | Error(MtProtoError.TransportError TransportError.ConnectionClosed) -> ()
        | other -> Assert.Fail($"expected ConnectionClosed, got %A{other}")

        Assert.That(sw.ElapsedMilliseconds, Is.LessThan 1000L)
