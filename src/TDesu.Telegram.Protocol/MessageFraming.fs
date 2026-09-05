namespace TDesu.MTProto

open System
open System.Buffers.Binary
open TDesu.Serialization
open TDesu.Crypto

/// Encrypted message framing for MTProto 2.0.
/// Plaintext: salt (8) + session_id (8) + msg_id (8) + seq_no (4) + length (4) + body + padding (12-1024)
/// Encrypted: auth_key_id (8) + msg_key (16) + AES-IGE encrypted plaintext
module MessageFraming =

    /// auth_key_id (8) + msg_key (16).
    [<Literal>]
    let private HeaderLength = 24

    /// salt (8) + session_id (8) + msg_id (8) + seq_no (4) + length (4).
    [<Literal>]
    let private PlaintextHeaderLength = 32

    /// Constant-time byte-array equality. Used for msg_key verification so a
    /// forged packet cannot be probed byte-by-byte via response-timing differences.
    let private ctEquals (a: byte[]) (b: byte[]) : bool =
        if a.Length <> b.Length then
            false
        else
            let mutable diff = 0
            for i in 0 .. a.Length - 1 do
                diff <- diff ||| int (a[i] ^^^ b[i])
            diff = 0

    /// Frame `body` as an encrypted MTProto 2.0 message.
    ///
    /// Three arrays per message: the writer's plaintext, its padded copy, and the frame the
    /// ciphertext is written into. 0.12 made five — `AesIge.encrypt` returned the ciphertext as its
    /// own array and a second writer copied it into the frame behind the header; `encryptTo` writes
    /// it straight into the frame instead. The padded copy stays because the 12..27-byte rule
    /// belongs to `Padding.addPadding`, and reproducing it here to save one copy would be a second
    /// place for the rule to drift.
    let encrypt (authKey: AuthKey) (session: SessionState) (msgId: int64) (seqNo: int32) (body: byte[]) : byte[] =
        // Build plaintext: salt + session_id + msg_id + seq_no + length + body
        use innerWriter = new TlWriteBuffer()
        innerWriter.WriteInt64(session.Salt)
        innerWriter.WriteInt64(session.SessionId)
        innerWriter.WriteInt64(msgId)
        innerWriter.WriteInt32(seqNo)
        innerWriter.WriteInt32(body.Length)
        innerWriter.WriteRawBytes(body)

        // Add padding (12-1024 bytes, total divisible by 16)
        let padded = Padding.addPadding (innerWriter.ToArray())

        // Compute msg_key (x=0 for client->server)
        let msgKey = KeyDerivation.computeMsgKey authKey.Data padded 0

        // Derive AES key/iv
        let aesParams = KeyDerivation.deriveAesKeyIv authKey.Data msgKey 0

        // Final message: auth_key_id + msg_key + AES-IGE ciphertext, encrypted in place.
        let frame = Array.zeroCreate<byte> (HeaderLength + padded.Length)
        BinaryPrimitives.WriteInt64LittleEndian(Span(frame, 0, 8), authKey.Id)
        Buffer.BlockCopy(msgKey, 0, frame, 8, 16)
        AesIge.encryptTo (ReadOnlySpan(padded)) aesParams.Key aesParams.Iv (Span(frame, HeaderLength, padded.Length))
        frame

    /// Decrypt one frame and hand back its body as a reader over the decrypted plaintext.
    ///
    /// The body is a view, not a copy: the decrypted plaintext is the one array this allocates,
    /// and the caller reads the body out of it in place — a container's inner messages by `Slice`,
    /// an rpc_result's payload by the single copy the awaiting caller owns. 0.12 copied the
    /// ciphertext out of the frame before decrypting and the body out of the plaintext after,
    /// which on a 512 KiB file part was two more LOH arrays per frame before the result was even
    /// dispatched.
    let decrypt (authKey: AuthKey) (data: byte[]) : Result<int64 * int64 * int32 * TlReadBuffer, MtProtoError> =
        try
            let reader = TlReadBuffer(data)
            let authKeyId = reader.ReadInt64()
            if authKeyId <> authKey.Id then
                Error (MtProtoError.InvalidResponse $"auth_key_id mismatch: expected %d{authKey.Id}, got %d{authKeyId}")
            else
                let msgKey = reader.ReadRawBytes(16)
                let ciphertextLength = reader.Remaining

                // Derive AES key/iv (x=8 for server->client)
                let aesParams = KeyDerivation.deriveAesKeyIv authKey.Data msgKey 8

                // Decrypt straight out of the frame. `decryptTo` refuses a length that is not a
                // whole number of AES blocks, which is the one shape check the frame needs.
                let decrypted = Array.zeroCreate<byte> ciphertextLength
                AesIge.decryptTo (reader.ReadSpan(ciphertextLength)) aesParams.Key aesParams.Iv (Span(decrypted))

                // Verify msg_key
                let expectedMsgKey = KeyDerivation.computeMsgKey authKey.Data decrypted 8
                if not (ctEquals expectedMsgKey msgKey) then
                    Error (MtProtoError.CryptoError "msg_key verification failed")
                else
                    // Parse plaintext
                    let innerReader = TlReadBuffer(decrypted)
                    let _salt = innerReader.ReadInt64()
                    let sessionId = innerReader.ReadInt64()
                    let msgId = innerReader.ReadInt64()
                    let seqNo = innerReader.ReadInt32()
                    let bodyLength = innerReader.ReadInt32()
                    // MTProto 2.0 receive rules also require a 4-aligned body and 12..1024 bytes
                    // of trailing padding. This is spec compliance, not an exposure: msg_key is
                    // already verified in constant time over the whole plaintext above, so a
                    // violation means a broken peer rather than a forgery attempt.
                    let paddingLength = decrypted.Length - PlaintextHeaderLength - bodyLength
                    if bodyLength < 0 || PlaintextHeaderLength + bodyLength > decrypted.Length then
                        Error (MtProtoError.InvalidResponse $"invalid body length %d{bodyLength}")
                    elif bodyLength % 4 <> 0 then
                        Error (MtProtoError.InvalidResponse $"body length %d{bodyLength} is not a multiple of 4")
                    elif paddingLength < 12 || paddingLength > 1024 then
                        Error (MtProtoError.InvalidResponse $"padding length %d{paddingLength} outside 12..1024")
                    else
                        Ok (msgId, sessionId, seqNo, innerReader.Slice(bodyLength))
        with
        | :? TlFormatException as ex -> Error (MtProtoError.SerializationError ex.Message)
        | :? ArgumentException as ex -> Error (MtProtoError.CryptoError ex.Message)
