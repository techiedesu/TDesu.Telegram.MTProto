namespace TDesu.Telegram.Protocol.Tests

open NUnit.Framework
open NUnit.Framework.Legacy
open TDesu.MTProto
open TDesu.Serialization
open TDesu.Crypto

[<TestFixture>]
module MessageFramingTests =

    let private authKey: AuthKey =
        { Data = Array.init 256 (fun i -> byte ((i * 7 + 3) % 256))
          Id = 0x1122334455667788L
          AuxHash = 0L }

    let private newSession () =
        let s = Session.createSession ()
        s.Salt <- 0x0102030405060708L
        s

    /// Server-side (x=8) encryption — the inverse of MessageFraming.decrypt, which is what the
    /// real server does. MessageFraming.encrypt is client->server (x=0), so we replicate the
    /// server direction here to exercise (and round-trip against) the decrypt path.
    let private serverEncrypt (key: AuthKey) (sess: SessionState) (msgId: int64) (seqNo: int) (body: byte[]) : byte[] =
        use w = new TlWriteBuffer()
        w.WriteInt64 sess.Salt
        w.WriteInt64 sess.SessionId
        w.WriteInt64 msgId
        w.WriteInt32 seqNo
        w.WriteInt32 body.Length
        w.WriteRawBytes body
        let padded = Padding.addPadding (w.ToArray())
        let msgKey = KeyDerivation.computeMsgKey key.Data padded 8
        let aes = KeyDerivation.deriveAesKeyIv key.Data msgKey 8
        let enc = AesIge.encrypt padded aes.Key aes.Iv
        use r = new TlWriteBuffer()
        r.WriteInt64 key.Id
        r.WriteRawBytes msgKey
        r.WriteRawBytes enc
        r.ToArray()

    [<Test>]
    let ``decrypt round-trips a server-encrypted message`` () =
        let sess = newSession ()
        let body = [| for i in 0..63 -> byte i |]
        let bytes = serverEncrypt authKey sess 0x5555555500000000L 3 body

        match MessageFraming.decrypt authKey bytes with
        | Ok(msgId, sessionId, seqNo, decoded) ->
            Assert.That(msgId, Is.EqualTo 0x5555555500000000L)
            Assert.That(sessionId, Is.EqualTo sess.SessionId)
            Assert.That(seqNo, Is.EqualTo 3)
            CollectionAssert.AreEqual(body, decoded)
        | Error e -> Assert.Fail($"expected Ok, got %A{e}")

    [<Test>]
    let ``decrypt rejects a tampered msg_key`` () =
        // A forged/replayed packet whose msg_key does not match the SHA-256 of the plaintext
        // must be rejected; this guards the constant-time msg_key verification.
        let sess = newSession ()
        let bytes = serverEncrypt authKey sess 0x100L 1 [| 1uy; 2uy; 3uy; 4uy |]
        bytes[8] <- bytes[8] ^^^ 0xFFuy // flip a byte inside msg_key (offset 8..23)

        let rejected =
            match MessageFraming.decrypt authKey bytes with
            | Error(MtProtoError.CryptoError _) -> true
            | _ -> false

        Assert.That(rejected, Is.True)

    [<Test>]
    let ``decrypt rejects a wrong auth_key_id`` () =
        let sess = newSession ()
        let bytes = serverEncrypt authKey sess 0x100L 1 [| 1uy; 2uy; 3uy; 4uy |]
        bytes[0] <- bytes[0] ^^^ 0xFFuy // corrupt auth_key_id

        let rejected =
            match MessageFraming.decrypt authKey bytes with
            | Error(MtProtoError.InvalidResponse _) -> true
            | _ -> false

        Assert.That(rejected, Is.True)
