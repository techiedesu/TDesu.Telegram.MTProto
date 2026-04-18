namespace TDesu.MTProto

open TDesu.Serialization

/// Unencrypted messages used during DH key exchange.
/// Format: auth_key_id (8 bytes, 0) + msg_id (8 bytes) + body_length (4 bytes) + body
module UnencryptedMessage =

    let serialize (msgId: int64) (body: byte[]) : byte[] =
        use writer = new TlWriteBuffer()
        writer.WriteInt64(0L)              // auth_key_id = 0 for unencrypted
        writer.WriteInt64(msgId)           // message_id
        writer.WriteInt32(body.Length)      // body length
        writer.WriteRawBytes(body)         // body (no padding)
        writer.ToArray()

    let deserialize (data: byte[]) : Result<int64 * byte[], MtProtoError> =
        try
            use reader = new TlReadBuffer(data)
            let authKeyId = reader.ReadInt64()
            if authKeyId <> 0L then
                Error (MtProtoError.InvalidResponse "Expected auth_key_id = 0 for unencrypted message")
            else
                let msgId = reader.ReadInt64()
                let bodyLength = reader.ReadInt32()
                let body = reader.ReadRawBytes(bodyLength)
                Ok (msgId, body)
        with ex ->
            Error (MtProtoError.SerializationError ex.Message)
