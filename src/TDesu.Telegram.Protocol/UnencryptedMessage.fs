namespace TDesu.MTProto

open System
open System.Buffers.Binary
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
        // A 4-byte frame is not a message but a transport error code, sent bare before the server
        // closes the socket: -404 for an auth key or handshake it rejects, -444 for a bad DC. Read
        // as a message it was "Expected auth_key_id = 0", which said nothing about the cause.
        if data.Length = 4 then
            Error(MtProtoError.TransportErrorCode(BinaryPrimitives.ReadInt32LittleEndian(ReadOnlySpan data)))
        else

        try
            let reader = TlReadBuffer(data)
            let authKeyId = reader.ReadInt64()
            if authKeyId <> 0L then
                Error (MtProtoError.InvalidResponse "Expected auth_key_id = 0 for unencrypted message")
            else
                let msgId = reader.ReadInt64()
                let bodyLength = reader.ReadInt32()

                // First length ever read off a new connection, before any key or nonce exists to
                // authenticate it. A negative value walks the read cursor backwards and returns an
                // empty body as success; an absurd one allocates whatever it asks for.
                if bodyLength < 0 || bodyLength > reader.Remaining then
                    Error(MtProtoError.InvalidResponse $"Unencrypted body length {bodyLength} does not fit the message")
                else
                    let body = reader.ReadRawBytes(bodyLength)
                    Ok(msgId, body)
        with :? TlFormatException as ex ->
            Error (MtProtoError.SerializationError ex.Message)
