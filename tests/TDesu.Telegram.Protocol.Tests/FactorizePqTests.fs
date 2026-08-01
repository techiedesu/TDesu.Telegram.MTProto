namespace TDesu.Telegram.Protocol.Tests

open System.Diagnostics
open System.Threading
open System.Threading.Tasks
open NUnit.Framework
open TDesu.MTProto
open TDesu.MTProto.Auth

/// `pq` is parsed off an unauthenticated, pre-handshake resPQ, so the server chooses the work
/// Pollard's rho does. Before this was bounded, a resPQ carrying a 63-bit PRIME — eight bytes and
/// >= 3, so past every guard performExchange applies — burned ~24 minutes of uninterruptible CPU
/// on this machine and then returned (1, pq), which is not a factorisation at all. ConnectAsync's
/// token could not stop it, which is what made a client dialling an untrusted server a remote
/// CPU-exhaustion primitive reachable before any key exists.
[<TestFixture>]
module FactorizePqTests =

    /// A real Telegram pq: the product of two 31-bit primes. This is the control ON THE DETECTOR —
    /// without it, a cap of zero would pass every hostile case below.
    let private realisticPq = 2147483647UL * 2147483629UL

    /// The largest prime below 2^63. Eight bytes, >= 3, and no factorisation to find, so rho has
    /// to run its whole cycle: ~3.0e9 iterations, measured at ~1453 s unbounded.
    let private hostilePrime = 9223372036854775783UL

    /// Bounds this fixture's own runs. The cap is ~0.7 s of work on this machine; anything an
    /// order of magnitude past that means the loop is unbounded again, not that CI was slow.
    [<Literal>]
    let private PromptMs = 10000L

    let private message =
        function
        | MtProtoError.AuthKeyExchangeFailed m -> m
        | other -> failwithf "expected AuthKeyExchangeFailed, got %A" other

    [<Test>]
    let ``a real 63-bit semiprime still factors`` () =
        let sw = Stopwatch.StartNew()
        let result = AuthKeyExchange.factorizePQ realisticPq CancellationToken.None
        sw.Stop()

        match result with
        | Ok(p, q) ->
            Assert.That(p, Is.EqualTo 2147483629UL)
            Assert.That(q, Is.EqualTo 2147483647UL)
            Assert.That(p * q, Is.EqualTo realisticPq)
        | Error e -> Assert.Fail($"the cap rejected a legitimate pq: {message e}")

        Assert.That(sw.ElapsedMilliseconds, Is.LessThan PromptMs)

    /// The hostile case, and the reason for the cap. Asserting the elapsed time is the whole
    /// point: the old code returned a value too, roughly 24 minutes later.
    [<Test>]
    let ``the largest prime below 2 pow 63 fails promptly instead of burning minutes`` () =
        let sw = Stopwatch.StartNew()
        let result = AuthKeyExchange.factorizePQ hostilePrime CancellationToken.None
        sw.Stop()

        match result with
        | Ok(p, q) -> Assert.Fail($"a prime pq reported a factorisation: %d{p} * %d{q}")
        | Error e -> Assert.That(message e, Does.Contain "did not factor within")

        Assert.That(sw.ElapsedMilliseconds, Is.LessThan PromptMs)

    /// Inside budget and still not a factorisation: rho settles on the trivial divisor for a small
    /// prime within a few hundred iterations. (1, pq) multiplies back to pq, so only the
    /// both-factors-> 1 half of the check catches this one.
    [<Test>]
    let ``a small prime pq is a failure, not one times pq`` () =
        match AuthKeyExchange.factorizePQ 7919UL CancellationToken.None with
        | Ok(p, q) -> Assert.Fail($"prime 7919 reported a factorisation: %d{p} * %d{q}")
        | Error e -> Assert.That(message e, Does.Contain "7919")

    [<Test>]
    let ``degenerate pq values are failures, not factorisations`` () =
        for pq in [ 0UL; 1UL; 2UL; 3UL ] do
            match AuthKeyExchange.factorizePQ pq CancellationToken.None with
            | Ok(p, q) -> Assert.Fail($"pq %d{pq} reported a factorisation: %d{p} * %d{q}")
            | Error e -> Assert.That(message e, Does.Contain "two factors")

    /// MtProtoClient's own doc comment promises "Failures arrive as Error Results, never thrown".
    /// A cancelled token must therefore not surface as OperationCanceledException from a call this
    /// deep inside the handshake — performExchange would map it to an opaque "Handshake failed".
    [<Test>]
    let ``an already-cancelled token returns Error rather than throwing`` () =
        use cts = new CancellationTokenSource()
        cts.Cancel()

        let sw = Stopwatch.StartNew()
        let result = AuthKeyExchange.factorizePQ hostilePrime cts.Token
        sw.Stop()

        match result with
        | Ok _ -> Assert.Fail "a cancelled factorisation reported a result"
        | Error e -> Assert.That(message e, Does.Contain "cancelled")

        // Nothing was computed, so this is not a "prompt enough" bound — it is immediate.
        Assert.That(sw.ElapsedMilliseconds, Is.LessThan 1000L)

    [<Test>]
    let ``a token cancelled mid-factorisation returns Error rather than throwing`` () =
        use cts = new CancellationTokenSource()
        let sw = Stopwatch.StartNew()
        let run = Task.Run(fun () -> AuthKeyExchange.factorizePQ hostilePrime cts.Token)

        // Long enough that rho is well inside its loop, short enough that cancellation, not the
        // iteration cap, is what ends the run.
        Thread.Sleep 100
        cts.Cancel()

        if not (run.Wait(int PromptMs)) then
            Assert.Fail "a cancelled factorisation never returned"

        sw.Stop()

        // The contract is a Result, not an exception: OperationCanceledException here would fault
        // the task and, through performExchange, surface as an opaque "Handshake failed".
        if run.IsFaulted then
            Assert.Fail($"cancellation was thrown instead of returned: %A{run.Exception}")

        match run.Result with
        | Ok _ -> Assert.Fail "a cancelled factorisation reported a result"
        | Error e -> Assert.That(message e, Does.Contain "cancelled")

        Assert.That(sw.ElapsedMilliseconds, Is.LessThan PromptMs)
