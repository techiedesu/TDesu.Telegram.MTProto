namespace TDesu.Telegram.Protocol.Tests

open NUnit.Framework
open TDesu.Transport

/// The carrier is part of the connection target, so these check that a `DataCenter` decides which
/// transport gets built — that is the whole contract a consumer relies on when it picks one.
[<TestFixture>]
module TransportSelectionTests =

    let private dc2 = DataCenters.production |> List.find (fun d -> d.Id = 2)

    [<Test>]
    let ``production data centres default to raw TCP`` () =
        for dc in DataCenters.production do
            Assert.That(dc.Transport, Is.EqualTo TransportKind.Tcp)

    [<Test>]
    let ``over replaces only the carrier`` () =
        let ws = dc2 |> DataCenters.over TransportKind.WebSocket
        Assert.That(ws.Transport, Is.EqualTo TransportKind.WebSocket)
        Assert.That(ws.Id, Is.EqualTo dc2.Id)
        Assert.That(ws.Address, Is.EqualTo dc2.Address)
        Assert.That(ws.Port, Is.EqualTo dc2.Port)

    [<Test>]
    let ``each kind builds its own transport`` () =
        let cases = [
            TransportKind.Tcp, typeof<TcpTransport>
            TransportKind.TcpObfuscated TransportFraming.Abridged, typeof<TcpObfuscatedTransport>
            TransportKind.WebSocket, typeof<WsTransport>
            TransportKind.Http, typeof<HttpTransport>
            TransportKind.FakeTls("proxy.example", 443, Array.init 16 byte, "example.com"), typeof<FakeTlsTransport>
        ]

        for kind, expected in cases do
            use transport = Transports.create (dc2 |> DataCenters.over kind)
            Assert.That(transport.GetType(), Is.EqualTo expected)
            Assert.That(transport.IsConnected, Is.False)
