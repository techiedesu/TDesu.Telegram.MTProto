namespace TDesu.Telegram.Protocol.Tests

open System
open System.Threading
open System.Threading.Tasks
open TDesu.Transport

/// Transport that is never connected: it counts connect attempts and refuses everything else, so a
/// test can watch what the client does when its carrier is down.
type internal DownTransport(attempts: int ref, succeed: int -> bool) =
    interface ITransport with
        member _.IsConnected = false

        member _.ConnectAsync(_) =
            task {
                attempts.Value <- attempts.Value + 1

                return
                    if succeed attempts.Value then
                        Ok()
                    else
                        Error TransportError.ConnectionClosed
            }

        member _.SendAsync(_, _) =
            Task.FromResult(Error TransportError.ConnectionClosed)

        member _.ReceiveAsync(_) =
            Task.FromResult(Error TransportError.ConnectionClosed)

        member _.Disconnect() = ()

    interface IDisposable with
        member _.Dispose() = ()

namespace TDesu.Telegram.Protocol.Tests

open System.Diagnostics
open System.Threading
open NUnit.Framework
open TDesu.MTProto
open TDesu.Transport

/// How the client behaves when its carrier is gone. Both cases here are the same lesson: a client
/// that neither recovers nor reports itself dead costs hours before anyone notices.
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

    /// Reconnects used to be driven only by the receive loop, so once its attempts were exhausted
    /// or cancelled, every later RPC failed until the process was restarted.
    [<Test>]
    let ``an RPC on a downed transport drives a reconnect`` () =
        let attempts = ref 0
        // Only the initial connect succeeds, so the client has a session and a dead carrier.
        let factory _ =
            new DownTransport(attempts, fun n -> n = 1) :> ITransport

        let key: AuthKey = {
            Data = Array.init 256 byte
            Id = 0x1122334455667788L
            AuxHash = 0x0102030405060708L
        }

        use client =
            new MtProtoClient(DataCenters.defaultDc, transportFactory = factory)

        match client.ConnectWithAuthKeyAsync(key, 1L, 0, CancellationToken.None).GetAwaiter().GetResult() with
        | Error e -> Assert.Fail($"stub connect failed: %A{e}")
        | Ok() -> ()

        let before = attempts.Value

        let result =
            client.RpcAsync([| 1uy; 2uy; 3uy; 4uy |], CancellationToken.None).GetAwaiter().GetResult()

        // A reconnect was attempted: the RPC, not the (long gone) receive loop, asked for it.
        Assert.That(attempts.Value - before, Is.GreaterThan 0)

        match result with
        | Error _ -> ()
        | Ok _ -> Assert.Fail "a downed transport cannot answer an RPC"
