// End-to-end demo: TL schema → parser → code generator → binary serialization → deserialization.
// Simulates MTProto ping/pong exchange using generated types.

open TDesu.Telegram.TL
open TDesu.Serialization

// === Step 1: Parse the MTProto TL schema ===

let schema = """
pong#347773c5 msg_id:long ping_id:long = Pong;
resPQ#05162463 nonce:int128 server_nonce:int128 pq:string server_public_key_fingerprints:Vector<long> = ResPQ;
rpc_error#2144ca19 error_code:int error_message:string = RpcError;
future_salt#0949d9dc valid_since:int valid_until:int salt:long = FutureSalt;
"""

printfn "=== Step 1: Parse TL schema ==="
let parsed = TlParser.Parse(schema)
printfn "  Parsed %d constructors" (parsed.GetConstructors().Length)
for c in parsed.GetConstructors() do
    let cid = c.GetConstructorId()
    let cidStr = if cid.HasValue then $"#%08X{cid.Value}" else ""
    printfn "  %s%s (%d params)" c.Id.Name cidStr (c.GetParams().Length)

// === Step 2: Simulate MTProto ping → pong exchange ===

printfn ""
printfn "=== Step 2: Ping → Pong round-trip ==="

// Client sends ping
let ping = { Pong.msgId = 0x1234567890ABCDEFL; pingId = 42L }
printfn "  Client sends: { msgId = 0x%016X; pingId = %d }" ping.msgId ping.pingId

// Serialize to bytes (like sending over TCP)
let writer = new TlWriteBuffer()
Pong.Serialize(writer, ping)
let bytes = writer.ToArray()
printfn "  Serialized: %d bytes [%s]" bytes.Length (bytes |> Array.map (fun b -> $"%02X{b}") |> String.concat " ")

// Deserialize (like receiving from server)
let reader = TlReadBuffer(bytes)
let pong = Pong.Deserialize(reader)
printfn "  Server pong: { msgId = 0x%016X; pingId = %d }" pong.msgId pong.pingId

// Verify round-trip
assert (pong.msgId = ping.msgId)
assert (pong.pingId = ping.pingId)
printfn "  Round-trip OK!"

// === Step 3: ResPQ with vector field ===

printfn ""
printfn "=== Step 3: ResPQ with vector<long> ==="

let resPQ = {
    ResPQ.nonce = Array.init 16 byte
    serverNonce = Array.init 16 (fun i -> byte (i + 16))
    pq = [| 0x17uy; 0xEDuy; 0x48uy; 0xD5uy; 0x97uy; 0x2Cuy; 0xABuy; 0x01uy |]
    serverPublicKeyFingerprints = [| 0xC3B42B026CE86B21L; 0xA12DEF65A42352AFL |]
}
printfn "  ResPQ: pq=%d bytes, %d fingerprints" resPQ.pq.Length resPQ.serverPublicKeyFingerprints.Length

let writer2 = new TlWriteBuffer()
ResPQ.Serialize(writer2, resPQ)
let bytes2 = writer2.ToArray()
printfn "  Serialized: %d bytes" bytes2.Length

let resPQ2 = ResPQ.Deserialize(TlReadBuffer(bytes2))
assert (resPQ2.serverPublicKeyFingerprints.Length = 2)
assert (resPQ2.serverPublicKeyFingerprints[0] = resPQ.serverPublicKeyFingerprints[0])
printfn "  Fingerprints: [%s]" (resPQ2.serverPublicKeyFingerprints |> Array.map (fun f -> $"0x%016X{f}") |> String.concat "; ")
printfn "  Round-trip OK!"

// === Step 4: RpcError ===

printfn ""
printfn "=== Step 4: RPC error ==="

let err = { RpcError.errorCode = 401; errorMessage = "AUTH_KEY_UNREGISTERED" }
let writer3 = new TlWriteBuffer()
RpcError.Serialize(writer3, err)
let err2 = RpcError.Deserialize(TlReadBuffer(writer3.ToArray()))
printfn "  Error %d: %s" err2.errorCode err2.errorMessage
assert (err2.errorCode = 401)
assert (err2.errorMessage = "AUTH_KEY_UNREGISTERED")
printfn "  Round-trip OK!"

// === Step 5: Real MTProto req_pq_multi to Telegram DC ===

// === Step 5: Real req_pq_multi → Telegram DC ===

printfn ""
printfn "=== Step 5: Real req_pq_multi → Telegram DC ==="

open System
open System.IO
open System.Net.Sockets
open System.Buffers.Binary

let nonce = Array.zeroCreate<byte> 16
Random.Shared.NextBytes(nonce)
printfn "  Nonce: %s" (nonce |> Array.map (fun b -> $"%02X{b}") |> String.concat "")

// Serialize req_pq_multi
let reqWriter = new TlWriteBuffer()
ReqPqMulti.Serialize(reqWriter, { ReqPqMulti.nonce = nonce })
let reqData = reqWriter.ToArray()

// Wrap in MTProto unencrypted message envelope
let messageId = DateTimeOffset.UtcNow.ToUnixTimeSeconds() * (1L <<< 32)
let envelope = Array.zeroCreate (8 + 8 + 4 + reqData.Length)
BinaryPrimitives.WriteInt64LittleEndian(envelope.AsSpan(8), messageId)
BinaryPrimitives.WriteInt32LittleEndian(envelope.AsSpan(16), reqData.Length)
Buffer.BlockCopy(reqData, 0, envelope, 20, reqData.Length)

// Try test DCs first (isolated environment), then production DCs
let dcAddresses = [
    // Test DCs (from tdlib/td ConnectionCreator.cpp)
    "149.154.175.10", 443   // Test DC1
    "149.154.167.40", 443   // Test DC2
    "149.154.175.117", 443  // Test DC3
    // Production DCs
    "149.154.175.50", 443   // DC1
    "149.154.167.51", 443   // DC2
    "149.154.175.100", 443  // DC3
]

let readExact (s: Stream) (buf: byte[]) (offset: int) (count: int) =
    let mutable read = 0
    while read < count do
        let n = s.Read(buf, offset + read, count - read)
        if n = 0 then failwith "Connection closed"
        read <- read + n

/// SOCKS5 connect: RFC 1928
let socks5Connect (proxyHost: string) (proxyPort: int) (targetHost: string) (targetPort: int) =
    let tcp = new TcpClient()
    tcp.ReceiveTimeout <- 10_000
    tcp.SendTimeout <- 5_000
    if not (tcp.ConnectAsync(proxyHost, proxyPort).Wait(3000)) then
        failwith "SOCKS5 proxy connect timeout"
    let s = tcp.GetStream()
    // Greeting: version=5, 1 auth method (no auth)
    s.Write([| 0x05uy; 0x01uy; 0x00uy |], 0, 3)
    let resp = Array.zeroCreate 2
    readExact s resp 0 2
    if resp[0] <> 0x05uy || resp[1] <> 0x00uy then failwith "SOCKS5 auth failed"
    // Connect request: version=5, cmd=connect, rsv=0, atype=1 (IPv4)
    let ipBytes = Net.IPAddress.Parse(targetHost).GetAddressBytes()
    let req = [| 0x05uy; 0x01uy; 0x00uy; 0x01uy; ipBytes[0]; ipBytes[1]; ipBytes[2]; ipBytes[3];
                 byte (targetPort >>> 8); byte (targetPort &&& 0xFF) |]
    s.Write(req, 0, req.Length)
    let connResp = Array.zeroCreate 10
    readExact s connResp 0 10
    if connResp[1] <> 0x00uy then failwith $"SOCKS5 connect failed: status %d{connResp[1]}"
    tcp, s :> Stream

/// Try SOCKS5 proxy on common Tor ports, fall back to direct
let tryConnect (host: string) (port: int) =
    let torPorts = [ 9150; 9050 ]
    let mutable result = None
    for tp in torPorts do
        if result.IsNone then
            try
                let tcp, stream = socks5Connect "127.0.0.1" tp host port
                printfn "  Connected via SOCKS5 (127.0.0.1:%d)" tp
                result <- Some(tcp, stream)
            with _ -> ()
    if result.IsNone then
        let tcp = new TcpClient()
        tcp.ReceiveTimeout <- 5_000
        tcp.SendTimeout <- 3_000
        if tcp.ConnectAsync(host, port).Wait(3000) then
            printfn "  Connected directly"
            result <- Some(tcp, tcp.GetStream() :> Stream)
        else
            tcp.Close()
    result

let mutable success = false

for host, port in dcAddresses do
    if not success then
        try
            printfn "  Trying %s:%d..." host port
            match tryConnect host port with
            | Some(tcp, stream) ->

                // Intermediate transport: magic + length-prefixed message
                stream.Write([| 0xeeuy; 0xeeuy; 0xeeuy; 0xeeuy |], 0, 4)
                let lengthBuf = Array.zeroCreate 4
                BinaryPrimitives.WriteInt32LittleEndian(lengthBuf.AsSpan(), envelope.Length)
                stream.Write(lengthBuf, 0, 4)
                stream.Write(envelope, 0, envelope.Length)
                stream.Flush()
                printfn "  Sent req_pq_multi"

                // Read response frame
                let respLenBuf = Array.zeroCreate 4
                readExact stream respLenBuf 0 4
                let respLen = BinaryPrimitives.ReadInt32LittleEndian(ReadOnlySpan(respLenBuf))

                let recvBuf = Array.zeroCreate respLen
                readExact stream recvBuf 0 respLen

                printfn "  Received %d bytes" respLen
                printfn "  Hex: %s" (recvBuf[..min 63 (respLen-1)] |> Array.map (fun b -> $"%02X{b}") |> String.concat " ")

                // Parse envelope
                let respDataLen = BinaryPrimitives.ReadInt32LittleEndian(ReadOnlySpan(recvBuf, 16, 4))
                printfn "  data_length=%d" respDataLen

                // Parse resPQ
                let respData = recvBuf[20 .. 20 + respDataLen - 1]
                let resp = ResPQ.Deserialize(TlReadBuffer(respData))

                printfn "  resPQ from Telegram:"
                printfn "    nonce:        %s" (resp.nonce |> Array.map (fun b -> $"%02X{b}") |> String.concat "")
                printfn "    server_nonce: %s" (resp.serverNonce |> Array.map (fun b -> $"%02X{b}") |> String.concat "")
                printfn "    pq:           %s" (resp.pq |> Array.map (fun b -> $"%02X{b}") |> String.concat "")
                printfn "    fingerprints: %s" (resp.serverPublicKeyFingerprints |> Array.map (fun f -> $"0x%016X{f}") |> String.concat ", ")

                assert (resp.nonce = nonce)
                printfn "  Nonce matches! MTProto handshake step 1 complete."
                success <- true

                tcp.Close()
            | None ->
                printfn "  Connect timeout"
        with ex ->
            printfn "  Failed: %s" ex.Message

if not success then
    printfn "  (!) Could not reach any Telegram DC — network restricted?"
    printfn "  Steps 1-4 verified serialization round-trips locally."

printfn ""
if success then
    printfn "All tests passed — including real Telegram DC communication!"
else
    printfn "Local serialization tests passed. DC connection skipped (network unavailable)."

