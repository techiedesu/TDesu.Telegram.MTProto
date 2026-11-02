namespace TDesu.MTProto

open System
open TDesu.Crypto

module Session =

    /// Generate a new random session ID
    let newSessionId () : int64 =
        let buf = Padding.randomBytes 8
        BitConverter.ToInt64(buf, 0)

    /// Generate a monotonically increasing message ID.
    /// Per MTProto, a client→server msg_id is approximately unixtime · 2^32 and MUST be
    /// divisible by 4 (low two bits = 00); the server rejects anything else with
    /// bad_msg_notification code 18. The high 32 bits hold the unix time, the low 32 the
    /// sub-second fraction.
    ///
    /// Locks on `session`: this is a read-modify-write of `LastMsgId`, and `resetClock`
    /// below writes the same field from the receive loop's thread, outside any lock a
    /// caller here might hold (MtProtoClient's sendLock only serializes senders against
    /// each other, never against the receive loop). Without a lock shared with
    /// `resetClock`, its write can land between this function's read of the stale
    /// `LastMsgId` and its write-back, and the write-back then silently overwrites the
    /// reset with a value still clamped off the pre-correction clock.
    let generateMsgId (session: SessionState) : int64 =
        lock session (fun () ->
            let now = DateTimeOffset.UtcNow
            let unixTime = now.ToUnixTimeSeconds() + int64 session.TimeOffset
            let fractional = int64 now.Millisecond * 0x100000000L / 1000L // < 2^32
            let raw = (unixTime <<< 32) ||| fractional
            // Clear the low two bits → divisible by 4.
            let aligned = raw &&& ~~~3L
            // Ensure strictly monotonically increasing while preserving divisibility by 4.
            let newMsgId =
                if aligned <= session.LastMsgId then session.LastMsgId + 4L
                else aligned
            session.LastMsgId <- newMsgId
            newMsgId)

    /// Correct the client/server clock disagreement a bad_msg_notification 16/17 reports.
    ///
    /// Answers whether the session can go on. A correction *forward* (16: our clock was behind)
    /// is harmless — the next msg_id is simply larger. A correction *backward* (17: our clock was
    /// ahead) is not: msg_ids must keep increasing within a session, and every id the wrong clock
    /// already issued sits above where the corrected clock now is. Dropping the floor, as this
    /// used to, made the next id lower than ones already sent under higher seq_nos, which the
    /// server refuses in turn; keeping it would repeat the too-high ids forever. The only way out
    /// is a new session, and `false` tells the caller to open one.
    ///
    /// Takes the same `session` lock as `generateMsgId` — see there — so this write can never
    /// land between that function's read and write-back of `LastMsgId` and be lost to it.
    let resetClock (session: SessionState) (newTimeOffset: int32) : bool =
        lock session (fun () ->
            session.TimeOffset <- newTimeOffset
            let correctedNow = (DateTimeOffset.UtcNow.ToUnixTimeSeconds() + int64 newTimeOffset) <<< 32
            correctedNow > session.LastMsgId)

    /// Start over on the same connection and auth key: a fresh session id, seq_no 0, a msg_id
    /// floor of 0. For the cases the server answers with a seq_no complaint (bad_msg 32–35) or
    /// after a backward clock correction, where nothing sent on the old session can be repaired.
    let renew (session: SessionState) : unit =
        lock session (fun () ->
            session.SessionId <- newSessionId ()
            session.SeqNo <- 0
            session.LastMsgId <- 0L
            session.Announced <- false)

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
        FutureSalts = []
        SaltConfirmed = false
        Announced = false
    }

    // ─── Server salts ────────────────────────────────────────────────────────
    //
    // The server rotates the salt about once an hour and refuses a message carrying an expired one
    // with `bad_server_salt`, which costs a round trip and a re-send. Asking `get_future_salts`
    // ahead of time and switching by the server's clock, as TDLib does, keeps every message on a
    // valid salt. `bad_server_salt` is then a fault worth a warning, not an hourly routine.

    /// How long a salt must already have been valid, and must stay valid, before it is chosen —
    /// slack for the gap between our corrected clock and the server's.
    [<Literal>]
    let SaltSwitchMarginSeconds = 60L

    /// Ask for more salts once the announced ones cover less than this much time ahead.
    [<Literal>]
    let SaltRefillHorizonSeconds = 3600L

    /// How many salts to ask for at once. The server answers with up to 64, about an hour each.
    [<Literal>]
    let FutureSaltsRequested = 32

    /// The server's clock as far as this session knows it: local time plus the correction
    /// `bad_msg_notification` 16/17 taught it.
    let serverNow (session: SessionState) : int64 =
        DateTimeOffset.UtcNow.ToUnixTimeSeconds() + int64 session.TimeOffset

    /// Which announced salt to use at server time `now`: the newest whose window has been open for
    /// at least the margin and stays open at least the margin longer, else any window containing
    /// `now`, else none.
    let selectSalt (salts: SaltWindow list) (now: int64) : int64 option =
        let comfortable =
            salts
            |> List.filter (fun s ->
                s.ValidSince <= now - SaltSwitchMarginSeconds && s.ValidUntil > now + SaltSwitchMarginSeconds)

        let containing =
            salts |> List.filter (fun s -> s.ValidSince <= now && s.ValidUntil > now)

        match comfortable, containing with
        | _ :: _, _ -> Some (comfortable |> List.maxBy _.ValidSince).Salt
        | [], _ :: _ -> Some (containing |> List.maxBy _.ValidSince).Salt
        | [], [] -> None

    /// Whether the announced salts run out within `SaltRefillHorizonSeconds` of `now` (or there are
    /// none at all).
    let needsFutureSalts (salts: SaltWindow list) (now: int64) : bool =
        salts |> List.forall (fun s -> s.ValidUntil < now + SaltRefillHorizonSeconds)

    /// Drop expired windows and switch `Salt` to the one valid now. Called right before a message
    /// is encrypted; a no-op while no salts have been announced.
    let refreshSalt (session: SessionState) : unit =
        lock session (fun () ->
            let now = serverNow session
            session.FutureSalts <- session.FutureSalts |> List.filter (fun s -> s.ValidUntil > now)

            match selectSalt session.FutureSalts now with
            | Some salt -> session.Salt <- salt
            | None -> ())

    /// Keep the salts a `future_salts` answer announced and switch to the one valid now.
    let storeFutureSalts (session: SessionState) (salts: SaltWindow list) : unit =
        lock session (fun () ->
            session.FutureSalts <- salts |> List.sortBy _.ValidSince
            session.SaltConfirmed <- true)

        refreshSalt session

    /// Take a salt the server named outright (`bad_server_salt`, `new_session_created`). Announced
    /// salts are dropped with it: they disagreed with the server, or belong to a session it no
    /// longer has, and the next refill asks again.
    let replaceSalt (session: SessionState) (salt: int64) : unit =
        lock session (fun () ->
            session.Salt <- salt
            session.SaltConfirmed <- true
            session.FutureSalts <- [])

    /// Whether this session should ask for more salts now.
    let needsMoreSalts (session: SessionState) : bool =
        lock session (fun () -> needsFutureSalts session.FutureSalts (serverNow session))
