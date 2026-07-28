namespace TDesu.Transport

/// Builds the carrier a `DataCenter` asks for.
///
/// This is the default transport factory: a caller selects its carrier by handing the client a
/// `DataCenter` whose `Transport` says which one, so nothing has to be configured globally and
/// no consumer needs to name a concrete transport type.
module Transports =

    let create (dc: DataCenter) : ITransport =
        match dc.Transport with
        | TransportKind.Tcp -> new TcpTransport(dc) :> ITransport
        | TransportKind.TcpObfuscated framing -> new TcpObfuscatedTransport(dc, framing) :> ITransport
        | TransportKind.WebSocket -> new WsTransport(dc) :> ITransport
        | TransportKind.Http -> new HttpTransport(dc) :> ITransport
        | TransportKind.FakeTls(proxyHost, proxyPort, secret, domain) ->
            new FakeTlsTransport(dc, proxyHost, proxyPort, secret, domain) :> ITransport
