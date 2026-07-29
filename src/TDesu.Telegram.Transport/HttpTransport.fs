namespace TDesu.Transport

open System
open System.Net.Sockets
open System.Text
open System.Threading
open System.Threading.Channels
open TDesu.FSharp
open TDesu.FSharp.Operators

/// MTProto transport over HTTP/1.1 (legacy carrier, still accepted by DCs on the
/// standard port). Framing is handled by HTTP itself, not by MTProto: each request
/// body is a raw MTProto payload and each response body is the reply.
///
/// HTTP is request/response, not full-duplex, so this transport couples the two:
/// SendAsync POSTs the payload and reads the paired response into an internal queue;
/// ReceiveAsync serves from that queue. Server-initiated pushes therefore piggyback
/// on the responses to the periodic keepalive (ack/ping) POSTs rather than arriving
/// spontaneously — adequate for RPC, with push latency bounded by the keepalive cadence.
type HttpTransport(dc: DataCenter) =

    let mutable client: TcpClient option = None
    let mutable stream: NetworkStream option = None
    let mutable connected = false

    // Responses are decoupled from ReceiveAsync via an unbounded channel; the send
    // path is the sole producer, so ordering matches the wire. Rebuilt on connect: Disconnect
    // completes it, and a client that is disconnected and connected again would otherwise drop
    // every reply into a closed channel without a sound.
    let mutable inbound = Channel.CreateUnbounded<byte[]>()
    // HTTP/1.1 is strictly one exchange at a time on a connection.
    let exchangeLock = new SemaphoreSlim(1, 1)

    let getStream () =
        match stream with
        | Some s -> Ok s
        | None -> Error TransportError.ConnectionClosed

    /// Read one CRLF-terminated header line (ASCII).
    let readLine (s: NetworkStream) (ct: CancellationToken) = task {
        let sb = StringBuilder()
        let one = Array.zeroCreate<byte> 1
        let mutable prevCr = false
        let mutable go = true

        while go do
            let! n = s.ReadAsync(Memory(one, 0, 1), ct)

            if n = 0 then
                go <- false
            elif one[0] = 10uy && prevCr then
                go <- false
            elif one[0] = 13uy then
                prevCr <- true
            else
                %sb.Append(char one[0])
                prevCr <- false

        return sb.ToString()
    }

    let readExactly (s: NetworkStream) (count: int) (ct: CancellationToken) = task {
        let buffer = Array.zeroCreate<byte> count
        let mutable total = 0
        let mutable closed = false

        while total < count && not closed do
            let! read = s.ReadAsync(Memory(buffer, total, count - total), ct)

            if read = 0 then closed <- true else total <- total + read

        return if closed then Error TransportError.ConnectionClosed else Ok buffer
    }

    /// Read an HTTP response and return its body (the MTProto payload).
    let readResponse (s: NetworkStream) (ct: CancellationToken) = task {
        let! statusLine = readLine s ct

        if not (statusLine.StartsWith "HTTP/") then
            return Error(TransportError.ReadError $"Malformed HTTP status line: {statusLine}")
        else
            let mutable contentLength = -1
            let mutable go = true

            while go do
                let! line = readLine s ct

                if line = "" then
                    go <- false
                elif line.StartsWith("Content-Length:", StringComparison.OrdinalIgnoreCase) then
                    // The only length on this wire without a structural cap. Anything else would
                    // hand an attacker-chosen size straight to Array.zeroCreate below.
                    match Int32.TryParse(line.Substring(15).Trim()) with
                    | true, v when v >= 0 && v <= FrameCodec.MaxFrameLength -> contentLength <- v
                    | _ -> go <- false

            if contentLength < 0 then
                return Error(TransportError.ReadError "HTTP response missing Content-Length")
            elif contentLength = 0 then
                return Ok Array.empty
            else
                return! readExactly s contentLength ct
    }

    member _.IsConnected = connected

    member _.ConnectAsync(ct: CancellationToken) = task {
        let tcp = new TcpClient()

        try
            tcp.NoDelay <- true
            do! tcp.ConnectAsync(dc.Address, dc.Port, ct)
            inbound <- Channel.CreateUnbounded<byte[]>()
            client <- Some tcp
            stream <- Some(tcp.GetStream())
            connected <- true
            return Ok()
        with ex ->
            tcp.Dispose()

            match ex with
            | :? OperationCanceledException -> return Error TransportError.Timeout
            | _ -> return Error(TransportError.ConnectionFailed ex.Message)
    }

    member _.SendAsync(payload: byte[], ct: CancellationToken) = task {
        match getStream () with
        | Error e -> return Error e
        | Ok s ->
            let mutable acquired = false

            try
                do! exchangeLock.WaitAsync(ct)
                acquired <- true
            with :? OperationCanceledException ->
                // Acquiring the lock is part of the call: an exception here would escape the
                // Result contract and leave the caller's request registered forever.
                ()

            if not acquired then
                return Error TransportError.Timeout
            else

            try
                try
                    let header =
                        sprintf
                            "POST /api HTTP/1.1\r\nHost: %O\r\nContent-Length: %d\r\nConnection: keep-alive\r\n\r\n"
                            dc.Address
                            payload.Length

                    do! s.WriteAsync(ReadOnlyMemory(Encoding.ASCII.GetBytes header), ct)
                    do! s.WriteAsync(ReadOnlyMemory payload, ct)
                    do! s.FlushAsync(ct)

                    // A server that takes the POST and never answers would otherwise hold the
                    // exchange lock forever and stop every other sender on this connection.
                    use responseCts = CancellationTokenSource.CreateLinkedTokenSource(ct)
                    responseCts.CancelAfter(TimeSpan.FromSeconds(60.0))

                    match! readResponse s responseCts.Token with
                    | Error e ->
                        connected <- false
                        return Error e
                    | Ok body ->
                        if body.Length > 0 then
                            %inbound.Writer.TryWrite body

                        return Ok()
                with ex ->
                    // A half-written request or a half-read response leaves the connection
                    // mid-message; HTTP gives no way to resynchronise it.
                    connected <- false

                    match ex with
                    | :? OperationCanceledException -> return Error TransportError.Timeout
                    | _ -> return Error(TransportError.WriteError ex.Message)
            finally
                %exchangeLock.Release()
    }

    member _.ReceiveAsync(ct: CancellationToken) = task {
        try
            let! item = inbound.Reader.ReadAsync(ct)
            return Ok item
        with
        | :? OperationCanceledException -> return Error TransportError.Timeout
        | :? ChannelClosedException -> return Error TransportError.ConnectionClosed
    }

    member _.Disconnect() =
        connected <- false
        %inbound.Writer.TryComplete()
        stream |> Option.iter (fun s -> s.Dispose())
        client |> Option.iter (fun c -> c.Dispose())
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
