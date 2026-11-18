namespace TDesu.Telegram.Protocol.Tests

open System
open System.Diagnostics
open System.IO
open System.IO.Compression
open NUnit.Framework
open NUnit.Framework.Legacy
open TDesu.MTProto

/// Conformance suite for any `IGzipInflater`: whatever `MtProtoClient` is given -- `platform` or a
/// consumer's own managed inflater -- must behave the same way for these inputs. A consumer
/// validating their own `IGzipInflater` can copy this file into their test project, add a
/// `[<TestFixture>]` subclass overriding `Inflater`, and run it unchanged.
///
/// Class-based rather than the module-per-fixture style the rest of this project uses: NUnit runs
/// `[<Test>]` members inherited from an `[<AbstractClass>]` base against every concrete
/// `[<TestFixture>]` subclass, which is the only way to parameterise a whole suite by
/// implementation; a module has no subtype to override.
[<AbstractClass>]
type GzipInflaterConformanceTests() =

    static let gzip (payload: byte[]) : byte[] =
        use compressed = new MemoryStream()

        do
            use gz = new GZipStream(compressed, CompressionMode.Compress, true)
            gz.Write(payload, 0, payload.Length)

        compressed.ToArray()

    // Built from a reused chunk instead of one big source array -- the point is a small `packed`
    // input that claims a large decompressed size, not actually holding that size twice over
    // (once as plaintext, once as its compressed form) for the duration of the test.
    static let gzipZeros (totalBytes: int64) : byte[] =
        use compressed = new MemoryStream()
        let chunk = Array.zeroCreate<byte> (1 <<< 20)

        do
            use gz = new GZipStream(compressed, CompressionLevel.Optimal, true)
            let mutable remaining = totalBytes

            while remaining > 0L do
                let n = int (min remaining (int64 chunk.Length))
                gz.Write(chunk, 0, n)
                remaining <- remaining - int64 n

        compressed.ToArray()

    /// The inflater under test.
    abstract Inflater: IGzipInflater

    [<Test>]
    member this.``valid round trip returns the original payload`` () =
        let payload = Array.init 8192 (fun i -> byte (i % 251))

        match this.Inflater.Inflate(ReadOnlySpan(gzip payload), 1 <<< 20) with
        | Ok bytes -> CollectionAssert.AreEqual(payload, bytes)
        | Error e -> Assert.Fail($"expected a valid round trip, got %A{e}")

    [<Test>]
    member this.``empty packed data decompresses to nothing`` () =
        match this.Inflater.Inflate(ReadOnlySpan<byte>.Empty, 1 <<< 20) with
        | Ok bytes -> Assert.That(bytes, Is.Empty)
        | Error e -> Assert.Fail($"expected empty input to decompress to nothing, got %A{e}")

    [<Test>]
    member this.``a truncated member never comes back as the untruncated payload`` () =
        let payload = Array.init 8192 (fun i -> byte (i % 251))
        let packed = gzip payload
        let truncated = packed[.. packed.Length / 2]

        // `GZipStream` -- `platform`'s own decoder -- does not reliably detect a stream that
        // simply ends early; see its doc comment. A lenient inflater may legitimately return `Ok`
        // with fewer bytes than the original instead of `Error`. What no inflater may do is claim
        // the truncated input decoded to the *same* bytes as the untruncated original.
        match this.Inflater.Inflate(ReadOnlySpan truncated, 1 <<< 20) with
        | Error _ -> ()
        | Ok bytes -> Assert.That(bytes, Is.Not.EqualTo(payload))

    [<Test>]
    member this.``a corrupted header is Malformed`` () =
        let packed = gzip (Array.init 64 (fun i -> byte i))
        packed[0] <- 0x00uy
        packed[1] <- 0x00uy

        match this.Inflater.Inflate(ReadOnlySpan packed, 1 <<< 20) with
        | Error(InflateError.Malformed _) -> ()
        | other -> Assert.Fail($"expected Malformed, got %A{other}")

    [<Test>]
    member this.``a CRC mismatch is Malformed`` () =
        let packed = gzip (Array.init 4096 (fun i -> byte ((i * 31 + i * i) % 256)))

        // Flip a trailer byte (the last 8 bytes are CRC32 + ISIZE) without changing the length --
        // cutting the stream short instead is the truncation case above, and a decoder that
        // doesn't fully receive a trailer is not guaranteed to validate one at all.
        packed[packed.Length - 5] <- packed[packed.Length - 5] ^^^ 0xFFuy

        match this.Inflater.Inflate(ReadOnlySpan packed, 1 <<< 20) with
        | Error(InflateError.Malformed _) -> ()
        | other -> Assert.Fail($"expected Malformed, got %A{other}")

    [<Test>]
    member this.``concatenated members decode at least the first, per RFC 1952`` () =
        let payloadA = Array.init 64 (fun i -> byte i)
        let payloadB = Array.init 64 (fun i -> byte (255 - i))
        let concatenated = Array.append (gzip payloadA) (gzip payloadB)

        // RFC 1952 section 2.2: a compliant decompressor MUST decompress at least the first
        // member; decompressing every concatenated member as one stream is allowed, not required.
        // `GzipInflater.platform` does the latter -- see its doc comment -- but this suite only
        // asserts the former, so an inflater that stops after the first member still passes.
        match this.Inflater.Inflate(ReadOnlySpan concatenated, 1 <<< 20) with
        | Ok bytes -> CollectionAssert.AreEqual(payloadA, bytes[.. payloadA.Length - 1])
        | Error e -> Assert.Fail($"expected at least the first member, got %A{e}")

    [<Test>]
    member this.``output exactly at the limit succeeds`` () =
        let payload = Array.init 4096 (fun i -> byte (i % 251))

        match this.Inflater.Inflate(ReadOnlySpan(gzip payload), payload.Length) with
        | Ok bytes -> CollectionAssert.AreEqual(payload, bytes)
        | Error e -> Assert.Fail($"expected success exactly at the limit, got %A{e}")

    [<Test>]
    member this.``output one byte over the limit is rejected`` () =
        let payload = Array.init 4097 (fun i -> byte (i % 251))
        let limit = payload.Length - 1

        match this.Inflater.Inflate(ReadOnlySpan(gzip payload), limit) with
        | Error(InflateError.ExceedsLimit l) -> Assert.That(l, Is.EqualTo limit)
        | other -> Assert.Fail($"expected ExceedsLimit {limit}, got %A{other}")

    [<Test>]
    member this.``highly compressible input over the limit is rejected without decompressing it first`` () =
        // 512 MiB of zeros compresses to about half a kilobyte. Measured locally: a loop that
        // checks the running total against the limit after every read rejects this in under a
        // millisecond; decompressing the same input in full with `CopyTo` before measuring its
        // length takes on the order of 200 ms. The two are two orders of magnitude apart, so a
        // generous bound well below the unbounded figure still catches a regression to
        // "decompress first, check the length after" without being sensitive to ordinary
        // machine noise.
        let packed = gzipZeros (512L * 1024L * 1024L)
        let limit = 4096
        let sw = Stopwatch.StartNew()
        let result = this.Inflater.Inflate(ReadOnlySpan packed, limit)
        sw.Stop()

        match result with
        | Error(InflateError.ExceedsLimit l) -> Assert.That(l, Is.EqualTo limit)
        | other -> Assert.Fail($"expected ExceedsLimit {limit}, got %A{other}")

        Assert.That(
            sw.ElapsedMilliseconds,
            Is.LessThan 2000L,
            NUnitString "rejecting a highly compressible input over the limit should not require decompressing it first"
        )

/// `GzipInflater.platform` through the shared conformance suite above.
[<TestFixture>]
type PlatformGzipInflaterConformanceTests() =
    inherit GzipInflaterConformanceTests()
    override _.Inflater = GzipInflater.platform
