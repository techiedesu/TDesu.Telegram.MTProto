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

    // msg_id -> (completion source, original request body kept for re-send after bad_server_salt)
    let pending =
        ConcurrentDictionary<int64, TaskCompletionSource<Result<byte[], MtProtoError>> * byte[]>()

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
    member _.RegisterRequest(msgId: int64, body: byte[]) : Task<Result<byte[], MtProtoError>> =
        let tcs =
            TaskCompletionSource<Result<byte[], MtProtoError>>(TaskCreationOptions.RunContinuationsAsynchronously)

        %pending.TryAdd(msgId, (tcs, body))
        tcs.Task

    /// Complete a pending request with the response data
    member _.CompleteRequest(msgId: int64, data: byte[]) : bool =
        match remove msgId with
        | Some(tcs, _) -> tcs.TrySetResult(Ok data)
        | None -> false

    /// Fail a pending request with an error (delivered as an Error Result, not an exception)
    member _.FailRequest(msgId: int64, error: MtProtoError) : bool =
        match remove msgId with
        | Some(tcs, _) -> tcs.TrySetResult(Error error)
        | None -> false

    /// Fail all pending requests (e.g., on disconnect)
    member _.FailAll(error: MtProtoError) =
        redirects.Clear()

        for kvp in pending do
            match pending.TryRemove(kvp.Key) with
            | true, (tcs, _) -> %tcs.TrySetResult(Error error)
            | false, _ -> ()

    /// The stored request body for a still-pending msg_id (used to re-send after bad_server_salt).
    member _.TryGetBody(msgId: int64) : byte[] option =
        match pending.TryGetValue(resolve msgId 8) with
        | true, (_, body) -> Some body
        | false, _ -> None

    /// Every msg_id still awaiting a response, under the id it currently lives at.
    member _.PendingIds : int64 list = pending.Keys |> List.ofSeq

    /// Move a pending request onto a new msg_id (after re-sending with a corrected salt).
    member _.Rekey(oldMsgId: int64, newMsgId: int64) : bool =
        match pending.TryRemove(oldMsgId) with
        | true, entry ->
            if pending.TryAdd(newMsgId, entry) then
                redirects[oldMsgId] <- newMsgId
                true
            else
                false
        | false, _ -> false

    /// Number of pending requests
    member _.PendingCount = pending.Count
