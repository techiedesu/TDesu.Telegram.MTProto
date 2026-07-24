namespace TDesu.Telegram.Protocol.Tests

open NUnit.Framework
open NUnit.Framework.Legacy
open TDesu.Transport

[<TestFixture>]
module TransportFramingTests =

    [<Test>]
    let ``intermediate frame round-trips`` () =
        let payload = [| for i in 0..99 -> byte i |]
        let frame = FrameCodec.encodeFrame payload

        match FrameCodec.decodeFrameLength frame with
        | Ok len ->
            Assert.That(len, Is.EqualTo payload.Length)
            CollectionAssert.AreEqual(payload, frame[4 .. 4 + len - 1])
        | Error e -> Assert.Fail($"%A{e}")

    [<Test>]
    let ``abridged uses 1-byte length below 0x7f`` () =
        let payload = Array.zeroCreate<byte> 40 // 10 words
        let frame = FrameCodec.Abridged.encodeFrame payload
        Assert.That(int frame[0], Is.EqualTo 10)
        Assert.That(FrameCodec.Abridged.isExtended frame[0], Is.False)

        match FrameCodec.Abridged.shortLength frame[0] with
        | Ok len -> Assert.That(len, Is.EqualTo 40)
        | Error e -> Assert.Fail($"%A{e}")

    [<Test>]
    let ``abridged uses 4-byte extended length at or above 0x7f`` () =
        let payload = Array.zeroCreate<byte> 600 // 150 words >= 127
        let frame = FrameCodec.Abridged.encodeFrame payload
        Assert.That(frame[0], Is.EqualTo 0x7fuy)
        Assert.That(FrameCodec.Abridged.isExtended frame[0], Is.True)

        match FrameCodec.Abridged.extendedLength frame[1..3] with
        | Ok len -> Assert.That(len, Is.EqualTo 600)
        | Error e -> Assert.Fail($"%A{e}")

    [<Test>]
    let ``randomized intermediate strips padding back to the original payload`` () =
        // Run several times because padding length (0..3) is random per call.
        for _ in 1..20 do
            let payload = [| for i in 0..95 -> byte i |]
            let frame = FrameCodec.RandomizedIntermediate.encodeFrame payload

            match FrameCodec.decodeFrameLength frame with
            | Ok len ->
                let body = frame[4 .. 4 + len - 1]
                CollectionAssert.AreEqual(payload, FrameCodec.RandomizedIntermediate.stripPadding body)
            | Error e -> Assert.Fail($"%A{e}")

    [<Test>]
    let ``Aes256Ctr decrypt inverts encrypt`` () =
        let key = Array.init 32 byte
        let iv = Array.init 16 (fun i -> byte (i + 1))
        use enc = new Aes256Ctr(key, iv)
        use dec = new Aes256Ctr(key, iv)
        let data = [| for i in 0..199 -> byte (i * 3) |]
        let cipher = enc.Process data
        CollectionAssert.AreNotEqual(data, cipher)
        CollectionAssert.AreEqual(data, dec.Process cipher)

    [<Test>]
    let ``Obfuscation.create yields a 64-byte init and a usable cipher pair`` () =
        let st = Obfuscation.create Obfuscation.IntermediateTag 2
        Assert.That(st.InitPacket.Length, Is.EqualTo 64)
        // The send cipher has already consumed the 64-byte init, so it produces ciphertext.
        let sample = [| for i in 0..63 -> byte i |]
        let out = st.Send.Process sample
        CollectionAssert.AreNotEqual(sample, out)
