namespace TDesu.Transport

open System
open System.Net
open System.Threading
open System.Threading.Tasks

[<RequireQualifiedAccess>]
type TransportError =
    | ConnectionFailed of message: string
    | ConnectionClosed
    | ReadError of message: string
    | WriteError of message: string
    | InvalidFrame of message: string
    | Timeout

/// Transport abstraction shared by TcpTransport and WsTransport. MtProtoClient and the
/// auth-key exchange talk to whichever concrete transport through this interface, so the
/// wire carrier (raw TCP intermediate vs. WebSocket binary frames) is swappable.
type ITransport =
    inherit IDisposable
    abstract member IsConnected: bool
    abstract member ConnectAsync: ct: CancellationToken -> Task<Result<unit, TransportError>>
    abstract member SendAsync: payload: byte[] * ct: CancellationToken -> Task<Result<unit, TransportError>>
    abstract member ReceiveAsync: ct: CancellationToken -> Task<Result<byte[], TransportError>>
    abstract member Disconnect: unit -> unit

/// Framing used on the wire once a connection is up. Abridged is the lightest
/// (1- or 4-byte length prefix); Intermediate uses a fixed 4-byte LE length.
[<RequireQualifiedAccess>]
type TransportFraming =
    | Abridged
    | Intermediate

/// Wire carrier used to reach a data centre. It travels with the `DataCenter` the caller hands
/// to the client, so a consumer picks its carrier when it initialises the connection and never
/// has to reach into how the client builds transports.
///
/// `Uri` is not structurally comparable, so neither is this. Equality stays.
[<RequireQualifiedAccess; NoComparison>]
type TransportKind =
    /// Raw TCP with cleartext intermediate framing. The default.
    | Tcp
    /// Raw TCP with the obfuscated ("obfuscation2") init and CTR-encrypted frames.
    | TcpObfuscated of framing: TransportFraming
    /// Obfuscated intermediate framing inside WebSocket binary messages. Survives networks
    /// that drop raw MTProto TCP. `None` resolves Telegram's gateway for `dc.Id`
    /// (wss://&lt;name&gt;.web.telegram.org/apiws); pass an endpoint to reach any other host —
    /// a self-hosted DC is not discoverable from `dc.Address`, since the carrier needs a URL.
    | WebSocket of endpoint: Uri option
    /// HTTP/1.1 POST/response carrier; pushes piggyback on keepalive replies.
    | Http
    /// MTProxy fake-TLS: obfuscated frames disguised as a TLS 1.3 session.
    | FakeTls of proxyHost: string * proxyPort: int * secret: byte[] * domain: string

/// IPAddress doesn't satisfy F# structural-comparison constraint, so the
/// record can't get auto-derived comparison either. Equality stays.
[<NoComparison>]
type DataCenter = {
    Id: int
    Address: IPAddress
    Port: int
    Transport: TransportKind
}

module DataCenters =

    let private dc id address port = {
        Id = id
        Address = IPAddress.Parse(address: string)
        Port = port
        Transport = TransportKind.Tcp
    }

    /// Same data centre reached over a different carrier:
    /// `DataCenters.production |> List.find (fun d -> d.Id = 2) |> DataCenters.over (TransportKind.WebSocket None)`.
    let over (kind: TransportKind) (dc: DataCenter) = { dc with Transport = kind }

    let production = [
        dc 1 "149.154.175.53" 443
        dc 2 "149.154.167.51" 443
        dc 3 "149.154.175.100" 443
        dc 4 "149.154.167.91" 443
        dc 5 "91.108.56.130" 443
    ]

    let test = [
        dc 1 "149.154.175.10" 443
        dc 2 "149.154.167.40" 443
        dc 3 "149.154.175.117" 443
    ]

    let defaultDc = production[1]
    let defaultTestDc = test[1]
