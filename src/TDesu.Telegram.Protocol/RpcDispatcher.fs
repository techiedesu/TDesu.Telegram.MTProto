namespace TDesu.MTProto

open System
open System.Collections.Concurrent
open System.Threading.Tasks
open TDesu.FSharp
open TDesu.FSharp.Operators
/// Matches RPC responses to pending requests by msg_id. Failures are delivered as `Error`
/// Results on the awaited task (not raised exceptions), so callers branch on the Result instead
/// of having to catch — a mid-flight disconnect surfaces as Error, not a thrown exception.
type RpcDispatcher() =

    // msg_id -> (completion source, seq_no it was sent under, original request body). The body is
    // kept for a re-send after bad_server_salt; the seq_no is what a retransmission after a
    // reconnect needs, because that one goes out under the *original* ids so a server that did
    // execute the request answers once instead of twice.
    let pending =
        ConcurrentDictionary<int64, TaskCompletionSource<Result<byte[], MtProtoError>> * int32 * byte[]>()

    // old msg_id -> current msg_id, for requests moved by Rekey. The caller still waits on the id
    // it sent under, so without this its timeout removes nothing and the entry — task and request
    // body — is stranded until the next FailAll.
    let redirects = ConcurrentDictionary<int64, int64>()

    /// Follow a chain of re-keys to the id the request currently lives under.
    let rec resolve (msgId: int64) (hops: int) =
        if hops = 0 then
            msgId
        else
            match redirects.TryGetValue msgId with
            | true, next -> resolve next (hops - 1)
            | false, _ -> msgId

    let remove (msgId: int64) =
        let current = resolve msgId 8
        %redirects.TryRemove msgId

        match pending.TryRemove current with
        | true, entry -> Some entry
        | false, _ -> None

    /// Register a pending request (keeping its body for a possible re-send) and return a Task
    /// that completes (Ok/Error) when the response arrives or the request fails.
    member _.RegisterRequest(msgId: int64, seqNo: int32, body: byte[]) : Task<Result<byte[], MtProtoError>> =
        let tcs =
            TaskCompletionSource<Result<byte[], MtProtoError>>(TaskCreationOptions.RunContinuationsAsynchronously)

        %pending.TryAdd(msgId, (tcs, seqNo, body))
        tcs.Task

    /// Complete a pending request with the response data
    member _.CompleteRequest(msgId: int64, data: byte[]) : bool =
        match remove msgId with
        | Some(tcs, _, _) -> tcs.TrySetResult(Ok data)
        | None -> false

    /// Fail a pending request with an error (delivered as an Error Result, not an exception)
    member _.FailRequest(msgId: int64, error: MtProtoError) : bool =
        match remove msgId with
        | Some(tcs, _, _) -> tcs.TrySetResult(Error error)
        | None -> false

    /// Fail all pending requests (e.g., on disconnect)
    member _.FailAll(error: MtProtoError) =
        redirects.Clear()

        for kvp in pending do
            match pending.TryRemove(kvp.Key) with
            | true, (tcs, _, _) -> %tcs.TrySetResult(Error error)
            | false, _ -> ()

    /// The stored request body for a still-pending msg_id (used to re-send after bad_server_salt).
    member _.TryGetBody(msgId: int64) : byte[] option =
        match pending.TryGetValue(resolve msgId 8) with
        | true, (_, _, body) -> Some body
        | false, _ -> None

    /// Every msg_id still awaiting a response, under the id it currently lives at.
    member _.PendingIds : int64 list = pending.Keys |> List.ofSeq

    /// Every pending request as it was last sent — msg_id, seq_no, body — for retransmission on
    /// the same session after a reconnect.
    member _.PendingRequests : (int64 * int32 * byte[]) list =
        pending
        |> Seq.map (fun kvp ->
            let (_, seqNo, body) = kvp.Value
            kvp.Key, seqNo, body)
        |> List.ofSeq

    /// Move a pending request onto a new msg_id (after re-sending with a corrected salt). The seq_no
    /// it will go out under is recorded separately by `SetSeqNo`, once the sender has taken one —
    /// taking it before the move is what `resendRequest` must not do.
    ///
    /// Resolves the redirect chain first, exactly as `TryGetBody` does. Without that the two
    /// disagreed: a request already re-keyed once had its body found under the resolved id and then
    /// failed to move, because this looked for the *original* id in `pending` where it no longer was.
    /// The re-send was silently abandoned and the caller waited out its full timeout — and every
    /// service message that re-sends reaches this, so a chat that earned two bad_server_salts in a
    /// row lost the request rather than retrying it.
    member _.Rekey(oldMsgId: int64, newMsgId: int64) : bool =
        let current = resolve oldMsgId 8

        match pending.TryRemove(current) with
        | true, entry ->
            if pending.TryAdd(newMsgId, entry) then
                // Chained from the id the caller named, so a caller still waiting on the original
                // resolves all the way forward however many times the request has moved.
                redirects[oldMsgId] <- newMsgId

                if current <> oldMsgId then
                    redirects[current] <- newMsgId

                true
            else
                // Put it back rather than dropping it on the floor: a failed add means the new id is
                // somehow taken, and the request is still legitimately pending under `current`.
                pending[current] <- entry
                false
        | false, _ -> false

    /// Record the seq_no a re-sent request went out under, so a later retransmission repeats it.
    member _.SetSeqNo(msgId: int64, seqNo: int32) : unit =
        match pending.TryGetValue msgId with
        | true, (tcs, _, body) -> pending[msgId] <- (tcs, seqNo, body)
        | false, _ -> ()

    /// Number of pending requests
    member _.PendingCount = pending.Count
