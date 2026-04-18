namespace TDesu.MTProto

open System
open System.Collections.Concurrent
open System.Threading.Tasks
open TDesu.FSharp
open TDesu.FSharp.Operators
/// Matches RPC responses to pending requests by msg_id.
type RpcDispatcher() =

    let pending = ConcurrentDictionary<int64, TaskCompletionSource<byte[]>>()

    /// Register a pending request and return a Task that completes when the response arrives
    member _.RegisterRequest(msgId: int64) : Task<byte[]> =
        let tcs = TaskCompletionSource<byte[]>(TaskCreationOptions.RunContinuationsAsynchronously)
        %pending.TryAdd(msgId, tcs)
        tcs.Task

    /// Complete a pending request with the response data
    member _.CompleteRequest(msgId: int64, data: byte[]) : bool =
        match pending.TryRemove(msgId) with
        | true, tcs ->
            tcs.SetResult(data)
            true
        | false, _ -> false

    /// Fail a pending request with an error
    member _.FailRequest(msgId: int64, error: MtProtoError) : bool =
        match pending.TryRemove(msgId) with
        | true, tcs ->
            tcs.SetException(Exception($"%A{error}"))
            true
        | false, _ -> false

    /// Fail all pending requests (e.g., on disconnect)
    member _.FailAll(error: MtProtoError) =
        let ex = Exception($"%A{error}")
        for kvp in pending do
            match pending.TryRemove(kvp.Key) with
            | true, tcs -> tcs.SetException(ex)
            | false, _ -> ()

    /// Number of pending requests
    member _.PendingCount = pending.Count
