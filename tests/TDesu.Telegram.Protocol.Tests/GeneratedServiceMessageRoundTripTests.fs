namespace TDesu.Telegram.Protocol.Tests

open System.Text
open FsCheck
open FsCheck.FSharp
open NUnit.Framework
open TDesu.Serialization
open TDesu.MTProto.Service.Requests

/// Round-trip properties for the TL types `td-tl-gen` generates for this project's service layer
/// (`Generated/GeneratedTlRequests.g.fs`). The generator itself lives in its own repository and
/// is covered there; what belongs here is that every `Serialize`/`Deserialize` pair it emits for
/// *this* schema actually inverts for arbitrary field values, the same property
/// `TDesu.Telegram.Serialization`'s own `RoundTripPropertyTests.fs` checks one level down for
/// `bytes`/`string` encoding.
[<TestFixture>]
type GeneratedServiceMessageRoundTripTests() =

    let config = Config.QuickThrowOnFailure.WithMaxTest(200)

    /// `Encoding.UTF8.GetBytes` replaces an unpaired UTF-16 surrogate with U+FFFD, so a .NET
    /// string containing one cannot round-trip through any UTF-8 encoding -- the same boundary
    /// `RoundTripPropertyTests.fs` scopes its own string property to.
    let isUtf8RoundTrippable (s: string) =
        not (isNull s) && Encoding.UTF8.GetString(Encoding.UTF8.GetBytes s) = s

    let roundTripOk (toBytes: 'T -> byte[]) (fromBytes: byte[] -> 'T) (value: 'T) = fromBytes (toBytes value) = value

    [<Test>]
    member _.``BadMsgNotification round-trips``() =
        let toBytes v = Tl.build (fun w -> BadMsgNotification.Serialize(w, v))
        let fromBytes b = BadMsgNotification.Deserialize(TlReadBuffer b)
        Check.One(config, roundTripOk toBytes fromBytes)

    [<Test>]
    member _.``DestroySessionRes round-trips``() =
        let toBytes v = Tl.build (fun w -> DestroySessionRes.Serialize(w, v))
        let fromBytes b = DestroySessionRes.Deserialize(TlReadBuffer b)
        Check.One(config, roundTripOk toBytes fromBytes)

    [<Test>]
    member _.``FutureSalts round-trips``() =
        let prop (reqMsgId: int64) (now: int32) (salts: (int32 * int32 * int64) list) =
            let v: FutureSalts =
                { ReqMsgId = reqMsgId
                  Now = now
                  Salts = salts |> List.map (fun (since, until, salt) -> ({ ValidSince = since; ValidUntil = until; Salt = salt }: FutureSalt)) |> Array.ofList }

            let toBytes v = Tl.build (fun w -> FutureSalts.Serialize(w, v))
            let fromBytes b = FutureSalts.Deserialize(TlReadBuffer b)
            roundTripOk toBytes fromBytes v

        Check.One(config, prop)

    [<Test>]
    member _.``MsgDetailedInfo round-trips``() =
        let toBytes v = Tl.build (fun w -> MsgDetailedInfo.Serialize(w, v))
        let fromBytes b = MsgDetailedInfo.Deserialize(TlReadBuffer b)
        Check.One(config, roundTripOk toBytes fromBytes)

    [<Test>]
    member _.``MsgResendReq round-trips``() =
        let toBytes (v: MsgResendReq) = Tl.bytesOf v
        let fromBytes b = MsgResendReq.Deserialize(TlReadBuffer b)
        Check.One(config, roundTripOk toBytes fromBytes)

    [<Test>]
    member _.``MsgsAck round-trips``() =
        let toBytes (v: MsgsAck) = Tl.bytesOf v
        let fromBytes b = MsgsAck.Deserialize(TlReadBuffer b)
        Check.One(config, roundTripOk toBytes fromBytes)

    [<Test>]
    member _.``MsgsAllInfo round-trips``() =
        let prop (msgIds: int64[]) (info: string) =
            isUtf8RoundTrippable info
            ==> lazy
                (let toBytes (v: MsgsAllInfo) = Tl.bytesOf v
                 let fromBytes b = MsgsAllInfo.Deserialize(TlReadBuffer b)
                 roundTripOk toBytes fromBytes { MsgIds = msgIds; Info = info })

        Check.One(config, prop)

    [<Test>]
    member _.``MsgsStateInfo round-trips``() =
        let prop (reqMsgId: int64) (info: string) =
            isUtf8RoundTrippable info
            ==> lazy
                (let toBytes (v: MsgsStateInfo) = Tl.bytesOf v
                 let fromBytes b = MsgsStateInfo.Deserialize(TlReadBuffer b)
                 roundTripOk toBytes fromBytes { ReqMsgId = reqMsgId; Info = info })

        Check.One(config, prop)

    [<Test>]
    member _.``MsgsStateReq round-trips``() =
        let toBytes (v: MsgsStateReq) = Tl.bytesOf v
        let fromBytes b = MsgsStateReq.Deserialize(TlReadBuffer b)
        Check.One(config, roundTripOk toBytes fromBytes)

    [<Test>]
    member _.``NewSession round-trips``() =
        let toBytes (v: NewSession) = Tl.bytesOf v
        let fromBytes b = NewSession.Deserialize(TlReadBuffer b)
        Check.One(config, roundTripOk toBytes fromBytes)

    [<Test>]
    member _.``Pong round-trips``() =
        let toBytes (v: Pong) = Tl.bytesOf v
        let fromBytes b = Pong.Deserialize(TlReadBuffer b)
        Check.One(config, roundTripOk toBytes fromBytes)

    [<Test>]
    member _.``RpcError round-trips``() =
        let prop (errorCode: int32) (errorMessage: string) =
            isUtf8RoundTrippable errorMessage
            ==> lazy
                (let toBytes (v: RpcError) = Tl.bytesOf v
                 let fromBytes b = RpcError.Deserialize(TlReadBuffer b)
                 roundTripOk toBytes fromBytes { ErrorCode = errorCode; ErrorMessage = errorMessage })

        Check.One(config, prop)

    [<Test>]
    member _.``GetFutureSalts round-trips``() =
        let toBytes v = Tl.build (fun w -> GetFutureSalts.Serialize(w, v))
        Check.One(config, roundTripOk toBytes GetFutureSalts.Deserialize)

    [<Test>]
    member _.``PingDelayDisconnect round-trips``() =
        let toBytes v = Tl.build (fun w -> PingDelayDisconnect.Serialize(w, v))
        Check.One(config, roundTripOk toBytes PingDelayDisconnect.Deserialize)
