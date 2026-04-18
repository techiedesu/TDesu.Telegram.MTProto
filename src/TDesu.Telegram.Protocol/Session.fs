namespace TDesu.MTProto

open System
open TDesu.Crypto

module Session =

    let private random = Random()

    /// Generate a new random session ID
    let newSessionId () : int64 =
        let buf = Padding.randomBytes 8
        BitConverter.ToInt64(buf, 0)

    /// Generate a monotonically increasing message ID.
    /// msg_id must be divisible by 4 and roughly equal to unixtime * 2^32.
    /// Lowest 2 bits encode direction: client→server uses 0b01 or 0b11.
    let generateMsgId (session: SessionState) : int64 =
        let now = DateTimeOffset.UtcNow
        let unixTime = now.ToUnixTimeSeconds() + int64 session.TimeOffset
        let fractional = int64 now.Millisecond * 0x100000000L / 1000L
        let raw = (unixTime <<< 32) ||| (fractional &&& 0xFFFFFFFC00000000L)
        // Ensure divisible by 4 and set client bit
        let aligned = (raw &&& ~~~3L) ||| 1L
        // Ensure monotonically increasing
        let newMsgId =
            if aligned <= session.LastMsgId then session.LastMsgId + 4L
            else aligned
        session.LastMsgId <- newMsgId
        newMsgId

    /// Generate next sequence number.
    /// Content-related messages: seqNo = seqNo*2 + 1
    /// Non-content (acks, etc.): seqNo = seqNo*2
    let nextSeqNo (session: SessionState) (contentRelated: bool) : int32 =
        if contentRelated then
            let s = session.SeqNo * 2 + 1
            session.SeqNo <- session.SeqNo + 1
            s
        else
            session.SeqNo * 2

    /// Create a fresh session state
    let createSession () : SessionState = {
        Salt = 0L
        SessionId = newSessionId ()
        SeqNo = 0
        LastMsgId = 0L
        TimeOffset = 0
    }
