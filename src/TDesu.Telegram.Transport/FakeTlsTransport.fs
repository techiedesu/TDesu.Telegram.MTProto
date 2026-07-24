namespace TDesu.Transport

open System
open System.Net.Sockets
open System.Security.Cryptography
open System.Text
open System.Threading
open TDesu.FSharp
open TDesu.FSharp.Operators

/// MTProto transport over MTProxy "fake-TLS" (obfuscation2 tag `ee...`): the stream
/// is disguised as a TLS 1.3 session. The client sends a TLS ClientHello whose
/// 32-byte client-random is an HMAC-SHA256(secret, hello) digest (last 4 bytes XOR
/// unixtime); the proxy authenticates by recomputing it. After the handshake, the
/// standard 64-byte obfuscation init (protocol tag SECURE = padded/randomized
/// intermediate) and all subsequent CTR-encrypted frames travel inside TLS
/// application-data records (0x17 0x03 0x03 <len>).
///
/// Connects to the MTProxy at (proxyHost, proxyPort) with the 16-byte `secret` and
/// the fronting `domain` (both from the `ee`-prefixed proxy secret). `dc.Id` is only
/// used for the dc_id field of the obfuscation init.
type FakeTlsTransport(dc: DataCenter, proxyHost: string, proxyPort: int, secret: byte[], domain: string) =

    let mutable client: TcpClient option = None
    let mutable stream: NetworkStream option = None
    let mutable send: Aes256Ctr option = None
    let mutable recv: Aes256Ctr option = None
    let mutable connected = false
    // Raw (TLS-unwrapped, still CTR-encrypted) bytes pending frame parsing.
    let mutable recvBuf: byte[] = Array.empty

    let u16be (n: int) : byte[] = [| byte (n >>> 8); byte n |]
    let u24be (n: int) : byte[] = [| byte (n >>> 16); byte (n >>> 8); byte n |]

    let readExactly (ns: NetworkStream) (count: int) (ct: CancellationToken) = task {
        let buffer = Array.zeroCreate<byte> count
        let mutable total = 0
        let mutable closed = false

        while total < count && not closed do
            let! read = ns.ReadAsync(Memory(buffer, total, count - total), ct)
            if read = 0 then closed <- true else total <- total + read

        return if closed then Error TransportError.ConnectionClosed else Ok buffer
    }

    /// Build the fake-TLS ClientHello record with the HMAC digest as client-random.
    let buildClientHello () =
        let randBytes n =
            let b = Array.zeroCreate<byte> n
            RandomNumberGenerator.Fill(Span b)
            b

        let hostBytes = Encoding.ASCII.GetBytes domain

        let sni =
            let nameEntry = Array.concat [ [| 0uy |]; u16be hostBytes.Length; hostBytes ]
            let list = Array.concat [ u16be nameEntry.Length; nameEntry ]
            Array.concat [ [| 0x00uy; 0x00uy |]; u16be list.Length; list ]

        let supportedVersions = [| 0x00uy; 0x2buy; 0x00uy; 0x03uy; 0x02uy; 0x03uy; 0x04uy |]
        let supportedGroups = [| 0x00uy; 0x0auy; 0x00uy; 0x04uy; 0x00uy; 0x02uy; 0x00uy; 0x1duy |]

        let keyShare =
            let shares = Array.concat [ [| 0x00uy; 0x1duy |]; u16be 32; randBytes 32 ]
            let extData = Array.concat [ u16be shares.Length; shares ]
            Array.concat [ [| 0x00uy; 0x33uy |]; u16be extData.Length; extData ]

        let baseExts = Array.concat [ sni; supportedVersions; supportedGroups; keyShare ]
        let cipherSuites = [| 0x13uy; 0x01uy; 0x13uy; 0x02uy; 0x13uy; 0x03uy |]
        let sessionId = randBytes 32

        // preExt = version(2)+random(32)+sessId(1+32)+cipherField(2+6)+compression(2)
        let preExtLen = 2 + 32 + 1 + 32 + (2 + cipherSuites.Length) + 2
        let baseHsLen = preExtLen + (2 + baseExts.Length)
        // MTProxy fake-TLS requires the ClientHello record body to be EXACTLY 512
        // bytes (fixed prefix 16 03 01 02 00 ... 00 01 fc), so handshake content = 508.
        // Pad the extensions to hit it exactly (padding-ext overhead is 4 bytes).
        let padContent = 508 - baseHsLen - 4
        let paddingExt = Array.concat [ [| 0x00uy; 0x15uy |]; u16be padContent; Array.zeroCreate padContent ]

        let exts = Array.concat [ baseExts; paddingExt ]

        let hsContent =
            Array.concat [
                [| 0x03uy; 0x03uy |] // client version TLS 1.2
                Array.zeroCreate<byte> 32 // random placeholder (digest)
                [| 0x20uy |]
                sessionId
                u16be cipherSuites.Length
                cipherSuites
                [| 0x01uy; 0x00uy |] // compression: none
                u16be exts.Length
                exts
            ]

        let handshake = Array.concat [ [| 0x01uy |]; u24be hsContent.Length; hsContent ]
        let record = Array.concat [ [| 0x16uy; 0x03uy; 0x01uy |]; u16be handshake.Length; handshake ]

        // digest = HMAC-SHA256(secret, record-with-zeroed-random); last 4 bytes XOR unixtime (LE)
        use hmac = new HMACSHA256(secret)
        let digest = hmac.ComputeHash record
        let ts = uint32 (DateTimeOffset.UtcNow.ToUnixTimeSeconds())
        digest[28] <- digest[28] ^^^ byte ts
        digest[29] <- digest[29] ^^^ byte (ts >>> 8)
        digest[30] <- digest[30] ^^^ byte (ts >>> 16)
        digest[31] <- digest[31] ^^^ byte (ts >>> 24)
        Buffer.BlockCopy(digest, 0, record, 11, 32)
        record

    /// Read and discard the server's handshake records (ServerHello, ChangeCipherSpec,
    /// and the fake certificate app-data record), stopping after the first 0x17 record.
    let readServerHandshake (ns: NetworkStream) (ct: CancellationToken) = task {
        let mutable result = Ok()
        let mutable sawAppData = false

        while (match result with Ok() -> not sawAppData | _ -> false) do
            match! readExactly ns 5 ct with
            | Error e -> result <- Error e
            | Ok hdr ->
                let recType = hdr[0]
                let len = (int hdr[3] <<< 8) ||| int hdr[4]

                match! readExactly ns len ct with
                | Error e -> result <- Error e
                | Ok _ -> if recType = 0x17uy then sawAppData <- true

        return result
    }

    /// Send bytes wrapped in TLS application-data records (chunked to 16 KiB).
    let sendAppData (ns: NetworkStream) (data: byte[]) (ct: CancellationToken) = task {
        let mutable off = 0

        while off < data.Length do
            let chunk = min 16384 (data.Length - off)
            do! ns.WriteAsync(ReadOnlyMemory [| 0x17uy; 0x03uy; 0x03uy; byte (chunk >>> 8); byte chunk |], ct)
            do! ns.WriteAsync(ReadOnlyMemory(data[off .. off + chunk - 1]), ct)
            off <- off + chunk

        do! ns.FlushAsync(ct)
    }

    /// Read the data of the next application-data (0x17) record, skipping
    /// ChangeCipherSpec (0x14) records.
    let rec readRecordData (ns: NetworkStream) (ct: CancellationToken) = task {
        match! readExactly ns 5 ct with
        | Error e -> return Error e
        | Ok hdr ->
            let recType = hdr[0]
            let len = (int hdr[3] <<< 8) ||| int hdr[4]

            match! readExactly ns len ct with
            | Error e -> return Error e
            | Ok data ->
                if recType = 0x17uy then return Ok data
                elif recType = 0x14uy then return! readRecordData ns ct
                else return Error(TransportError.InvalidFrame $"unexpected TLS record type 0x%02x{recType}")
    }

    /// Pull `n` raw bytes from the record stream, buffering leftover record data.
    let ensureRaw (ns: NetworkStream) (n: int) (ct: CancellationToken) = task {
        let mutable err = None

        while recvBuf.Length < n && err.IsNone do
            match! readRecordData ns ct with
            | Error e -> err <- Some e
            | Ok data -> recvBuf <- Array.append recvBuf data

        match err with
        | Some e -> return Error e
        | None ->
            let head = recvBuf[0 .. n - 1]
            recvBuf <- recvBuf[n..]
            return Ok head
    }

    member _.IsConnected = connected

    member _.ConnectAsync(ct: CancellationToken) = task {
        try
            let tcp = new TcpClient()
            tcp.NoDelay <- true
            do! tcp.ConnectAsync(proxyHost, proxyPort, ct)
            let ns = tcp.GetStream()

            // TLS handshake camouflage.
            do! ns.WriteAsync(ReadOnlyMemory(buildClientHello ()), ct)
            do! ns.FlushAsync(ct)

            match! readServerHandshake ns ct with
            | Error e -> return Error e
            | Ok() ->
                // Obfuscation init (SECURE tag) inside a ChangeCipherSpec + app-data record.
                let obf = Obfuscation.createMtproxy Obfuscation.SecureTag dc.Id secret
                do! ns.WriteAsync(ReadOnlyMemory [| 0x14uy; 0x03uy; 0x03uy; 0x00uy; 0x01uy; 0x01uy |], ct)
                do! sendAppData ns obf.InitPacket ct

                client <- Some tcp
                stream <- Some ns
                send <- Some obf.Send
                recv <- Some obf.Recv
                connected <- true
                return Ok()
        with
        | :? OperationCanceledException -> return Error TransportError.Timeout
        | ex -> return Error(TransportError.ConnectionFailed ex.Message)
    }

    member _.SendAsync(payload: byte[], ct: CancellationToken) = task {
        match stream, send with
        | Some ns, Some s ->
            try
                let frame = FrameCodec.RandomizedIntermediate.encodeFrame payload
                do! sendAppData ns (s.Process frame) ct
                return Ok()
            with
            | :? OperationCanceledException -> return Error TransportError.Timeout
            | ex -> return Error(TransportError.WriteError ex.Message)
        | _ -> return Error TransportError.ConnectionClosed
    }

    member _.ReceiveAsync(ct: CancellationToken) = task {
        match stream, recv with
        | Some ns, Some r ->
            try
                match! ensureRaw ns 4 ct with
                | Error e -> return Error e
                | Ok lenRaw ->
                    match FrameCodec.decodeFrameLength (r.Process lenRaw) with
                    | Error e -> return Error e
                    | Ok length ->
                        match! ensureRaw ns length ct with
                        | Error e -> return Error e
                        | Ok payRaw -> return Ok(FrameCodec.RandomizedIntermediate.stripPadding (r.Process payRaw))
            with
            | :? OperationCanceledException -> return Error TransportError.Timeout
            | ex -> return Error(TransportError.ReadError ex.Message)
        | _ -> return Error TransportError.ConnectionClosed
    }

    member _.Disconnect() =
        connected <- false
        send |> Option.iter (fun s -> (s :> IDisposable).Dispose())
        recv |> Option.iter (fun r -> (r :> IDisposable).Dispose())
        stream |> Option.iter (fun s -> s.Dispose())
        client |> Option.iter (fun c -> c.Dispose())
        send <- None
        recv <- None
        stream <- None
        client <- None
        recvBuf <- Array.empty

    interface ITransport with
        member this.IsConnected = this.IsConnected
        member this.ConnectAsync(ct) = this.ConnectAsync(ct)
        member this.SendAsync(payload, ct) = this.SendAsync(payload, ct)
        member this.ReceiveAsync(ct) = this.ReceiveAsync(ct)
        member this.Disconnect() = this.Disconnect()

    interface IDisposable with
        member this.Dispose() = this.Disconnect()
