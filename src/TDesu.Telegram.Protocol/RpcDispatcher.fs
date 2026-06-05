namespace TDesu.MTProto

open System
open System.Collections.Concurrent
open System.Threading.Tasks
open TDesu.FSharp
open TDesu.FSharp.Operators
/// Matches RPC responses to pending requests by msg_id.
type RpcDispatcher() =

    // msg_id -> (completion source, original request body kept for re-send after bad_server_salt)
    let pending = ConcurrentDictionary<int64, TaskCompletionSource<byte[]> * byte[]>()

    /// Register a pending request (keeping its body for a possible re-send) and return a Task
    /// that completes when the response arrives.
    member _.RegisterRequest(msgId: int64, body: byte[]) : Task<byte[]> =
        let tcs = TaskCompletionSource<byte[]>(TaskCreationOptions.RunContinuationsAsynchronously)
        %pending.TryAdd(msgId, (tcs, body))
        tcs.Task

    /// Complete a pending request with the response data
    member _.CompleteRequest(msgId: int64, data: byte[]) : bool =
        match pending.TryRemove(msgId) with
        | true, (tcs, _) ->
            tcs.SetResult(data)
            true
        | false, _ -> false

    /// Fail a pending request with an error
    member _.FailRequest(msgId: int64, error: MtProtoError) : bool =
        match pending.TryRemove(msgId) with
        | true, (tcs, _) ->
            tcs.SetException(Exception($"%A{error}"))
            true
        | false, _ -> false

    /// Fail all pending requests (e.g., on disconnect)
    member _.FailAll(error: MtProtoError) =
        let ex = Exception($"%A{error}")
        for kvp in pending do
            match pending.TryRemove(kvp.Key) with
            | true, (tcs, _) -> tcs.SetException(ex)
            | false, _ -> ()

    /// The stored request body for a still-pending msg_id (used to re-send after bad_server_salt).
    member _.TryGetBody(msgId: int64) : byte[] option =
        match pending.TryGetValue(msgId) with
        | true, (_, body) -> Some body
        | false, _ -> None

    /// Move a pending request onto a new msg_id (after re-sending with a corrected salt).
    member _.Rekey(oldMsgId: int64, newMsgId: int64) : bool =
        match pending.TryRemove(oldMsgId) with
        | true, entry -> pending.TryAdd(newMsgId, entry)
        | false, _ -> false

    /// Number of pending requests
    member _.PendingCount = pending.Count
