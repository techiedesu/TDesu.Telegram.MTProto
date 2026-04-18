namespace TDesu.MTProto

open TDesu.Serialization
open TDesu.Crypto

/// Encrypted message framing for MTProto 2.0.
/// Plaintext: salt (8) + session_id (8) + msg_id (8) + seq_no (4) + length (4) + body + padding (12-1024)
/// Encrypted: auth_key_id (8) + msg_key (16) + AES-IGE encrypted plaintext
module MessageFraming =

    let encrypt (authKey: AuthKey) (session: SessionState) (msgId: int64) (seqNo: int32) (body: byte[]) : byte[] =
        // Build plaintext: salt + session_id + msg_id + seq_no + length + body
        use innerWriter = new TlWriteBuffer()
        innerWriter.WriteInt64(session.Salt)
        innerWriter.WriteInt64(session.SessionId)
        innerWriter.WriteInt64(msgId)
        innerWriter.WriteInt32(seqNo)
        innerWriter.WriteInt32(body.Length)
        innerWriter.WriteRawBytes(body)
        let innerData = innerWriter.ToArray()

        // Add padding (12-1024 bytes, total divisible by 16)
        let padded = Padding.addPadding innerData

        // Compute msg_key (x=0 for client->server)
        let msgKey = KeyDerivation.computeMsgKey authKey.Data padded 0

        // Derive AES key/iv
        let aesParams = KeyDerivation.deriveAesKeyIv authKey.Data msgKey 0

        // Encrypt with AES-IGE
        let encrypted = AesIge.encrypt padded aesParams.Key aesParams.Iv

        // Build final message: auth_key_id + msg_key + encrypted_data
        use resultWriter = new TlWriteBuffer()
        resultWriter.WriteInt64(authKey.Id)
        resultWriter.WriteRawBytes(msgKey)
        resultWriter.WriteRawBytes(encrypted)
        resultWriter.ToArray()

    let decrypt (authKey: AuthKey) (data: byte[]) : Result<int64 * int64 * int32 * byte[], MtProtoError> =
        try
            use reader = new TlReadBuffer(data)
            let authKeyId = reader.ReadInt64()
            if authKeyId <> authKey.Id then
                Error (MtProtoError.InvalidResponse $"auth_key_id mismatch: expected %d{authKey.Id}, got %d{authKeyId}")
            else
                let msgKey = reader.ReadRawBytes(16)
                let encryptedData = reader.ReadRawBytes(data.Length - 24)

                // Derive AES key/iv (x=8 for server->client)
                let aesParams = KeyDerivation.deriveAesKeyIv authKey.Data msgKey 8

                // Decrypt
                let decrypted = AesIge.decrypt encryptedData aesParams.Key aesParams.Iv

                // Verify msg_key
                let expectedMsgKey = KeyDerivation.computeMsgKey authKey.Data decrypted 8
                if expectedMsgKey <> msgKey then
                    Error (MtProtoError.CryptoError "msg_key verification failed")
                else
                    // Parse plaintext
                    use innerReader = new TlReadBuffer(decrypted)
                    let _salt = innerReader.ReadInt64()
                    let _sessionId = innerReader.ReadInt64()
                    let msgId = innerReader.ReadInt64()
                    let seqNo = innerReader.ReadInt32()
                    let bodyLength = innerReader.ReadInt32()
                    let body = innerReader.ReadRawBytes(bodyLength)
                    Ok (msgId, _sessionId, seqNo, body)
        with ex ->
            Error (MtProtoError.SerializationError ex.Message)
