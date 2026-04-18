namespace TDesu.MTProto

open System

type AuthKey = {
    Data: byte[]
    Id: int64
    AuxHash: int64
}

type SessionState = {
    mutable Salt: int64
    mutable SessionId: int64
    mutable SeqNo: int32
    mutable LastMsgId: int64
    mutable TimeOffset: int32
}

[<RequireQualifiedAccess>]
type MtProtoError =
    | TransportError of TDesu.Transport.TransportError
    | SerializationError of message: string
    | CryptoError of message: string
    | AuthKeyExchangeFailed of message: string
    | RpcError of errorCode: int * errorMessage: string
    | Timeout
    | InvalidResponse of message: string
    | SessionExpired
