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

/// Why a call did not produce an answer. Every case is something a caller can act on without
/// reading a message string: 0.12 carried "not connected", "bad_msg_notification 33", a transport
/// error code and a malformed frame all inside `InvalidResponse of string`, and the one consumer
/// collapsed everything that was not `RpcError` into "no answer" because nothing else was
/// distinguishable.
[<RequireQualifiedAccess>]
type MtProtoError =
    /// The carrier failed: refused connection, closed socket, unreadable frame, cancelled read.
    | TransportError of TDesu.Transport.TransportError
    /// The server answered with a 4-byte transport error instead of a message: -404 (unknown
    /// auth key — the session must be discarded and a login redone), -429 (too many connections
    /// from this address), -444 (invalid DC). Until 0.13 these were undecodable and looked like a
    /// flaky network that reconnect retries could fix.
    | TransportErrorCode of code: int
    | SerializationError of message: string
    | CryptoError of message: string
    | AuthKeyExchangeFailed of message: string
    /// An `rpc_error` from the server, as sent. Migrations are split out below.
    | RpcError of errorCode: int * errorMessage: string
    /// A 303 naming the DC the request belongs to: `kind` is the prefix (`PHONE`, `NETWORK`,
    /// `FILE`, `USER`, `STATS`), `dc` the number after `_MIGRATE_`.
    | Migrate of kind: string * dc: int
    /// rpc_error 420 `FLOOD_WAIT_n` / `FLOOD_PREMIUM_WAIT_n`: the account must not call for
    /// `seconds`.
    | FloodWait of seconds: int
    /// A `bad_msg_notification` the client could not repair by itself; the code is the server's.
    | BadMsgNotification of code: int
    /// No session: the client was never connected, or was disconnected and not revived.
    | NotConnected
    /// The response deadline passed (see `MtProtoClient`'s `responseTimeout`).
    | Timeout
    /// The caller's token fired.
    | Cancelled
    /// The connection dropped and every reconnect attempt failed; the request was not re-sent.
    | ReconnectFailed
    /// A reply that could not be read: wrong auth_key_id, bad padding, a frame that does not parse.
    | InvalidResponse of message: string

module MtProtoError =
    /// The error a server `rpc_error` becomes. A 303 whose message is `<KIND>_MIGRATE_<dc>` is the
    /// server saying which DC the request belongs to, and every consumer was parsing that out of
    /// the string; it is its own case now.
    let ofRpcError (code: int) (message: string) : MtProtoError =
        if code = 303 then
            let marker = "_MIGRATE_"
            let at = message.IndexOf(marker, StringComparison.Ordinal)

            if at > 0 then
                match Int32.TryParse(message.Substring(at + marker.Length)) with
                | true, dc -> MtProtoError.Migrate(message.Substring(0, at), dc)
                | _ -> MtProtoError.RpcError(code, message)
            else
                MtProtoError.RpcError(code, message)
        elif code = 420 then
            // `SLOWMODE_WAIT_n` also contains the `_WAIT_` marker but is a per-chat limit, not an
            // account-wide one, so the prefix — not just the marker — decides whether this is a
            // flood wait.
            let marker = "_WAIT_"
            let at = message.IndexOf(marker, StringComparison.Ordinal)
            let prefix = if at > 0 then message.Substring(0, at) else ""

            if prefix = "FLOOD" || prefix = "FLOOD_PREMIUM" then
                match Int32.TryParse(message.Substring(at + marker.Length)) with
                | true, seconds -> MtProtoError.FloodWait seconds
                | _ -> MtProtoError.RpcError(code, message)
            else
                MtProtoError.RpcError(code, message)
        else
            MtProtoError.RpcError(code, message)
