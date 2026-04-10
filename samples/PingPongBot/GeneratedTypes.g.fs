// Auto-generated from MTProto TL schema by td-tl-gen.
// Source: pong#347773c5 msg_id:long ping_id:long = Pong;
//         resPQ#05162463 nonce:int128 server_nonce:int128 pq:string server_public_key_fingerprints:Vector<long> = ResPQ;
//         rpc_error#2144ca19 error_code:int error_message:string = RpcError;
//         future_salt#0949d9dc valid_since:int valid_until:int salt:long = FutureSalt;

namespace TDesu.Serialization

open TDesu.Serialization

type Pong = {
    msgId: int64
    pingId: int64
} with

    static member ConstructorId: uint32 = 0x347773C5u

    static member Serialize(writer: TlWriteBuffer, value: Pong) : unit =
        writer.WriteConstructorId(0x347773C5u)
        writer.WriteInt64(value.msgId)
        writer.WriteInt64(value.pingId)

    static member Deserialize(reader: TlReadBuffer) : Pong =
        let _cid = reader.ReadConstructorId()
        let msgId = reader.ReadInt64()
        let pingId = reader.ReadInt64()
        { msgId = msgId; pingId = pingId }

type ResPQ = {
    nonce: byte[]          // int128
    serverNonce: byte[]    // int128
    pq: byte[]             // TL bytes
    serverPublicKeyFingerprints: int64 array
} with

    static member ConstructorId: uint32 = 0x05162463u

    static member Serialize(writer: TlWriteBuffer, value: ResPQ) : unit =
        writer.WriteConstructorId(0x05162463u)
        writer.WriteRawBytes(value.nonce)       // int128 = 16 raw bytes
        writer.WriteRawBytes(value.serverNonce)  // int128 = 16 raw bytes
        writer.WriteBytes(value.pq)              // string (TL-encoded)
        writer.WriteConstructorId(0x1CB5C415u)   // vector
        writer.WriteInt32(value.serverPublicKeyFingerprints.Length)
        for item in value.serverPublicKeyFingerprints do
            writer.WriteInt64(item)

    static member Deserialize(reader: TlReadBuffer) : ResPQ =
        let _cid = reader.ReadConstructorId()
        let nonce = reader.ReadRawBytes(16)       // int128
        let serverNonce = reader.ReadRawBytes(16)  // int128
        let pq = reader.ReadBytes()                // TL string → byte[]
        let _vectorCid = reader.ReadConstructorId()
        let count = reader.ReadInt32()
        let serverPublicKeyFingerprints = [| for _ in 1 .. count -> reader.ReadInt64() |]
        { nonce = nonce; serverNonce = serverNonce; pq = pq; serverPublicKeyFingerprints = serverPublicKeyFingerprints }

type RpcError = {
    errorCode: int32
    errorMessage: string
} with

    static member ConstructorId: uint32 = 0x2144CA19u

    static member Serialize(writer: TlWriteBuffer, value: RpcError) : unit =
        writer.WriteConstructorId(0x2144CA19u)
        writer.WriteInt32(value.errorCode)
        writer.WriteString(value.errorMessage)

    static member Deserialize(reader: TlReadBuffer) : RpcError =
        let _cid = reader.ReadConstructorId()
        let errorCode = reader.ReadInt32()
        let errorMessage = reader.ReadString()
        { errorCode = errorCode; errorMessage = errorMessage }

/// req_pq_multi#be7e8ef1 nonce:int128 = ResPQ;
type ReqPqMulti = {
    nonce: byte[]  // 16 bytes (int128)
} with

    static member ConstructorId: uint32 = 0xBE7E8EF1u

    static member Serialize(writer: TlWriteBuffer, value: ReqPqMulti) : unit =
        writer.WriteConstructorId(0xBE7E8EF1u)
        writer.WriteRawBytes(value.nonce)  // int128 = 16 raw bytes

    static member Deserialize(reader: TlReadBuffer) : ReqPqMulti =
        let _cid = reader.ReadConstructorId()
        let nonce = reader.ReadRawBytes(16)
        { nonce = nonce }

type FutureSalt = {
    validSince: int32
    validUntil: int32
    salt: int64
} with

    static member ConstructorId: uint32 = 0x0949D9DCu

    static member Serialize(writer: TlWriteBuffer, value: FutureSalt) : unit =
        writer.WriteConstructorId(0x0949D9DCu)
        writer.WriteInt32(value.validSince)
        writer.WriteInt32(value.validUntil)
        writer.WriteInt64(value.salt)

    static member Deserialize(reader: TlReadBuffer) : FutureSalt =
        let _cid = reader.ReadConstructorId()
        let validSince = reader.ReadInt32()
        let validUntil = reader.ReadInt32()
        let salt = reader.ReadInt64()
        { validSince = validSince; validUntil = validUntil; salt = salt }
