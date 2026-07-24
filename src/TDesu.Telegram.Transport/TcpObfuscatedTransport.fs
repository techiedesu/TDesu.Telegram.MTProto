namespace TDesu.Transport

open System
open System.Net.Sockets
open System.Threading
open TDesu.FSharp
open TDesu.FSharp.Operators

/// MTProto transport over raw TCP using the obfuscated ("obfuscation2") protocol:
/// a 64-byte obfuscation init followed by AES-256-CTR-encrypted frames. Unlike the
/// plain TcpTransport, no cleartext framing header is sent (0xef / 0xeeeeeeee live
/// inside the init tag instead), which defeats DPI that fingerprints those headers.
///
/// Framing defaults to Abridged (1-byte length for most packets); Intermediate is
/// available for parity with the plain transport.
type TcpObfuscatedTransport(dc: DataCenter, ?framing: TransportFraming) =

    let framing = defaultArg framing TransportFraming.Abridged

    let tag =
        match framing with
        | TransportFraming.Abridged -> Obfuscation.AbridgedTag
        | TransportFraming.Intermediate -> Obfuscation.IntermediateTag

    let mutable client: TcpClient option = None
    let mutable stream: NetworkStream option = None
    let mutable send: Aes256Ctr option = None
    let mutable recv: Aes256Ctr option = None
    let mutable connected = false

    /// Read exactly `count` bytes and CTR-decrypt them in place of the wire order.
    let readDecrypted (ns: NetworkStream) (r: Aes256Ctr) (count: int) (ct: CancellationToken) = task {
        let buffer = Array.zeroCreate<byte> count
        let mutable totalRead = 0
        let mutable closed = false

        while totalRead < count && not closed do
            let! read = ns.ReadAsync(Memory(buffer, totalRead, count - totalRead), ct)

            if read = 0 then
                connected <- false
                closed <- true
            else
                totalRead <- totalRead + read

        if closed then
            return Error TransportError.ConnectionClosed
        else
            return Ok(r.Process buffer)
    }

    let receiveIntermediate (ns: NetworkStream) (r: Aes256Ctr) (ct: CancellationToken) = task {
        match! readDecrypted ns r 4 ct with
        | Error e -> return Error e
        | Ok header ->
            match FrameCodec.decodeFrameLength header with
            | Error e -> return Error e
            | Ok length -> return! readDecrypted ns r length ct
    }

    let receiveAbridged (ns: NetworkStream) (r: Aes256Ctr) (ct: CancellationToken) = task {
        match! readDecrypted ns r 1 ct with
        | Error e -> return Error e
        | Ok first ->
            let! lengthResult = task {
                if FrameCodec.Abridged.isExtended first[0] then
                    match! readDecrypted ns r 3 ct with
                    | Error e -> return Error e
                    | Ok three -> return FrameCodec.Abridged.extendedLength three
                else
                    return FrameCodec.Abridged.shortLength first[0]
            }

            match lengthResult with
            | Error e -> return Error e
            | Ok length -> return! readDecrypted ns r length ct
    }

    member _.IsConnected = connected

    member _.ConnectAsync(ct: CancellationToken) = task {
        try
            let tcp = new TcpClient()
            tcp.NoDelay <- true
            tcp.ReceiveBufferSize <- 64 * 1024
            tcp.SendBufferSize <- 64 * 1024

            // TCP keepalive: detect dead connections within ~25s.
            let sock = tcp.Client
            sock.SetSocketOption(SocketOptionLevel.Socket, SocketOptionName.KeepAlive, true)
            sock.SetSocketOption(SocketOptionLevel.Tcp, SocketOptionName.TcpKeepAliveTime, 5)
            sock.SetSocketOption(SocketOptionLevel.Tcp, SocketOptionName.TcpKeepAliveInterval, 5)
            sock.SetSocketOption(SocketOptionLevel.Tcp, SocketOptionName.TcpKeepAliveRetryCount, 3)

            do! tcp.ConnectAsync(dc.Address, dc.Port, ct)
            let ns = tcp.GetStream()

            // Obfuscation handshake: send the 64-byte init (cleartext apart from its
            // last 8 encrypted bytes); everything after it is CTR-encrypted.
            let obf = Obfuscation.create tag dc.Id
            do! ns.WriteAsync(ReadOnlyMemory obf.InitPacket, ct)
            do! ns.FlushAsync(ct)

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
                let frame =
                    match framing with
                    | TransportFraming.Abridged -> FrameCodec.Abridged.encodeFrame payload
                    | TransportFraming.Intermediate -> FrameCodec.encodeFrame payload

                do! ns.WriteAsync(ReadOnlyMemory(s.Process frame), ct)
                do! ns.FlushAsync(ct)
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
                match framing with
                | TransportFraming.Abridged -> return! receiveAbridged ns r ct
                | TransportFraming.Intermediate -> return! receiveIntermediate ns r ct
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

    interface ITransport with
        member this.IsConnected = this.IsConnected
        member this.ConnectAsync(ct) = this.ConnectAsync(ct)
        member this.SendAsync(payload, ct) = this.SendAsync(payload, ct)
        member this.ReceiveAsync(ct) = this.ReceiveAsync(ct)
        member this.Disconnect() = this.Disconnect()

    interface IDisposable with
        member this.Dispose() = this.Disconnect()
