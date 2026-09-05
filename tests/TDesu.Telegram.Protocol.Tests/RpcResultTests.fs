namespace TDesu.Telegram.Protocol.Tests

open System
open System.Threading
open System.Threading.Tasks
open TDesu.MTProto
open TDesu.Serialization
open TDesu.Crypto
open TDesu.Transport

/// Loopback transport for driving the encrypted RPC round trip end to end. `processRpcResult` is
/// private to `MtProtoClient`, so the only seam that reaches it is a real `RpcAsync` call
/// answered by a real (encrypted) response — there is no smaller unit to call directly.
/// `SendAsync` decrypts the client's own request to recover the exact msg_id and session_id it
/// used, the reverse of what `MessageFramingTests.serverEncrypt` already does for the opposite
/// direction, then hands back whatever reply body the test supplied, encrypted the way the real
/// server would.
type internal LoopbackTransport(authKey: AuthKey, buildReplyBody: int64 -> byte[]) =
    let mutable connected = false
    let replyReady = new SemaphoreSlim(0)
    let mutable reply: byte[] = Array.empty

    // Inverse of `MessageFraming.encrypt` (client->server, x=0). `MessageFraming` only exposes
    // the server->client (x=8) half as `decrypt`, so reading what the client just sent needs the
    // other half — the same gap `serverEncrypt` fills on the other side of `decrypt`.
    let clientDecrypt (data: byte[]) : int64 * int64 =
        let reader = TlReadBuffer(data)
        reader.ReadInt64() |> ignore // auth_key_id
        let msgKey = reader.ReadRawBytes(16)
        let encryptedData = reader.ReadRawBytes(data.Length - 24)
        let aes = KeyDerivation.deriveAesKeyIv authKey.Data msgKey 0
        let decrypted = AesIge.decrypt encryptedData aes.Key aes.Iv
        let inner = TlReadBuffer(decrypted)
        inner.ReadInt64() |> ignore // salt
        let sessionId = inner.ReadInt64()
        let msgId = inner.ReadInt64()
        msgId, sessionId

    interface ITransport with
        member _.IsConnected = connected

        member _.ConnectAsync(_) =
            connected <- true
            Task.FromResult(Ok())

        member _.SendAsync(payload, _) =
            let reqMsgId, sessionId = clientDecrypt payload
            let sess = Session.createSession ()
            sess.SessionId <- sessionId
            reply <- MessageFramingTests.serverEncrypt authKey sess (reqMsgId + 1L) 1 (buildReplyBody reqMsgId)
            replyReady.Release() |> ignore
            Task.FromResult(Ok())

        member _.ReceiveAsync(ct) =
            task {
                do! replyReady.WaitAsync(ct)
                return Ok reply
            }

        member _.Disconnect() = connected <- false

    interface IDisposable with
        member _.Dispose() = replyReady.Dispose()

namespace TDesu.Telegram.Protocol.Tests

open System.Threading
open NUnit.Framework
open NUnit.Framework.Legacy
open TDesu.MTProto
open TDesu.Serialization
open TDesu.Transport

/// The result slot on the wire is the same for success and failure: an rpc_error#2144ca19 arrives
/// exactly where a normal rpc_result would, and only `processRpcResult` tells them apart. Before
/// `e3c49c7` it didn't: an rpc_error was handed to the caller as `Ok` bytes holding an error
/// object, so no consumer could ever pattern-match `Error(RpcError ...)` — flood-wait backoff and
/// 2FA (SESSION_PASSWORD_NEEDED) detection were both silently dead on every client built on this
/// library. These pin the fixed behaviour in both directions so a future refactor can't quietly
/// reopen it.
[<TestFixture>]
module RpcResultTests =

    let private testAuthKey: AuthKey = {
        Data = Array.init 256 (fun i -> byte ((i * 11 + 5) % 256))
        Id = 0x2233445566778899L
        AuxHash = 0L
    }

    // rpc_error#2144ca19 error_code:int error_message:string = RpcError
    let private buildRpcErrorBody (code: int) (message: string) : byte[] =
        use w = new TlWriteBuffer()
        w.WriteConstructorId(0x2144ca19u)
        w.WriteInt32(code)
        w.WriteString(message)
        w.ToArray()

    // rpc_result#f35c6d01 req_msg_id:long result:Object = RpcResult
    let private buildRpcResultBody (reqMsgId: int64) (result: byte[]) : byte[] =
        use w = new TlWriteBuffer()
        w.WriteConstructorId(0xf35c6d01u)
        w.WriteInt64(reqMsgId)
        w.WriteRawBytes(result)
        w.ToArray()

    /// `ConnectWithAuthKeyAsync` skips the DH exchange the same way `ClientLifecycleTests` does,
    /// so the test drives the result-slot handling in isolation from key negotiation.
    let private connectedClient (transport: LoopbackTransport) =
        let client =
            new MtProtoClient(DataCenters.defaultDc, transportFactory = fun _ -> transport :> ITransport)

        match client.ConnectWithAuthKeyAsync(testAuthKey, 1L, 0, CancellationToken.None).GetAwaiter().GetResult() with
        | Error e -> Assert.Fail($"stub connect failed: %A{e}")
        | Ok() -> ()

        client

    /// FLOOD_WAIT_42 / code 420 specifically: it's the rpc_error whose silent loss is the actual
    /// production incident this pins — a client that never sees it never sleeps 42s, it just
    /// hammers Telegram with the next retry instead.
    [<Test>]
    let ``an rpc_error in the result slot fails the call with its code and message`` () =
        use transport =
            new LoopbackTransport(testAuthKey, fun reqMsgId ->
                buildRpcResultBody reqMsgId (buildRpcErrorBody 420 "FLOOD_WAIT_42"))

        use client = connectedClient transport

        let result =
            client.RpcAsync([| 1uy; 2uy; 3uy; 4uy |], CancellationToken.None).GetAwaiter().GetResult()

        match result with
        | Error(MtProtoError.RpcError(420, "FLOOD_WAIT_42")) -> ()
        | other -> Assert.Fail($"expected RpcError(420, FLOOD_WAIT_42), got %A{other}")

    /// Paired with the case above: a fix that stopped forwarding rpc_error but also broke plain
    /// results would still pass an error-only test, and a client that failed every request would
    /// pass one too. This is what proves the call still succeeds when the server means it to.
    [<Test>]
    let ``an ordinary result in the same slot still arrives as Ok with the bytes intact`` () =
        let payload = [| 42uy; 43uy; 44uy; 45uy |]

        use transport =
            new LoopbackTransport(testAuthKey, fun reqMsgId -> buildRpcResultBody reqMsgId payload)

        use client = connectedClient transport

        let result =
            client.RpcAsync([| 1uy; 2uy; 3uy; 4uy |], CancellationToken.None).GetAwaiter().GetResult()

        match result with
        | Ok bytes -> CollectionAssert.AreEqual(payload, bytes)
        | Error e -> Assert.Fail($"expected Ok, got %A{e}")
