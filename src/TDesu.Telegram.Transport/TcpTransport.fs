namespace TDesu.Transport

open System
open System.IO
open System.Net.Sockets
open System.Threading
open TDesu.FSharp
open TDesu.FSharp.Operators
type TcpTransport(dc: DataCenter) =

    let mutable client: TcpClient option = None
    let mutable stream: NetworkStream option = None
    let mutable connected = false

    let getStream () =
        match stream with
        | Some s -> Ok s
        | None -> Error TransportError.ConnectionClosed

    let readExactly (ns: NetworkStream) (buffer: byte[]) (offset: int) (count: int) (ct: CancellationToken) = task {
        let mutable totalRead = 0
        let mutable closed = false
        while totalRead < count && not closed do
            let! read = ns.ReadAsync(Memory(buffer, offset + totalRead, count - totalRead), ct)
            if read = 0 then
                connected <- false
                closed <- true
            else
                totalRead <- totalRead + read
        return not closed
    }

    member _.IsConnected = connected

    member _.ConnectAsync(ct: CancellationToken) = task {
        try
            let tcp = new TcpClient()
            tcp.NoDelay <- true
            tcp.ReceiveBufferSize <- 64 * 1024
            tcp.SendBufferSize <- 64 * 1024
            do! tcp.ConnectAsync(dc.Address, dc.Port, ct)
            let ns = tcp.GetStream()
            client <- Some tcp
            stream <- Some ns
            connected <- true
            // Send intermediate transport header
            let header = FrameCodec.encodeHeader ()
            do! ns.WriteAsync(ReadOnlyMemory header, ct)
            return Ok ()
        with
        | :? OperationCanceledException -> return Error TransportError.Timeout
        | ex -> return Error (TransportError.ConnectionFailed ex.Message)
    }

    member _.SendAsync(payload: byte[], ct: CancellationToken) = task {
        match getStream () with
        | Error e -> return Error e
        | Ok ns ->
            try
                let frame = FrameCodec.encodeFrame payload
                do! ns.WriteAsync(ReadOnlyMemory frame, ct)
                do! ns.FlushAsync(ct)
                return Ok ()
            with
            | :? OperationCanceledException -> return Error TransportError.Timeout
            | ex -> return Error (TransportError.WriteError ex.Message)
    }

    member _.ReceiveAsync(ct: CancellationToken) = task {
        match getStream () with
        | Error e -> return Error e
        | Ok ns ->
            try
                // Read 4-byte length header
                let headerBuf = Array.zeroCreate<byte> 4
                let! headerOk = readExactly ns headerBuf 0 4 ct
                if not headerOk then
                    return Error TransportError.ConnectionClosed
                else
                    match FrameCodec.decodeFrameLength headerBuf with
                    | Error e -> return Error e
                    | Ok length ->
                        let payload = Array.zeroCreate<byte> length
                        let! payloadOk = readExactly ns payload 0 length ct
                        if not payloadOk then
                            return Error TransportError.ConnectionClosed
                        else
                            return Ok payload
            with
            | :? OperationCanceledException -> return Error TransportError.Timeout
            | ex -> return Error (TransportError.ReadError ex.Message)
    }

    member _.Disconnect() =
        connected <- false
        stream |> Option.iter (fun s -> s.Dispose())
        client |> Option.iter (fun c -> c.Dispose())
        stream <- None
        client <- None

    interface IDisposable with
        member this.Dispose() = this.Disconnect()
