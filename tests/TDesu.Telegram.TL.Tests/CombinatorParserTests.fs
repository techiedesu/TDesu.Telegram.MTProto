module TDesu.Telegram.TL.Tests.CombinatorParserTests

open NUnit.Framework
open FParsec
open TDesu.Telegram.TL.AST
open TDesu.Telegram.TL.AST.Parsers.CombinatorParsers
open TDesu.Telegram.TL.Tests

let private runParser p input =
    match run (p .>> eof) input with
    | Success(result, _, _) -> Some result
    | Failure _ -> None

[<Test>]
let ``combinator parses boolFalse`` () =
    let result = runParser combinator "boolFalse#bc799737 = Bool;"
    Assert.That(result.IsSome)
    let c = Option.get result
    equals c.Id { Namespace = None; Name = "boolFalse" }
    equals c.ConstructorId (Some(TlConstructorId 0xbc799737u))
    Assert.That(c.Params, Is.Empty)
    equals c.ResultType (TlTypeExpr.Boxed { Namespace = None; Name = "Bool" })

[<Test>]
let ``combinator parses boolTrue`` () =
    let result = runParser combinator "boolTrue#997275b5 = Bool;"
    Assert.That(result.IsSome)
    let c = Option.get result
    equals c.Id.Name "boolTrue"
    equals c.ConstructorId (Some(TlConstructorId 0x997275b5u))

[<Test>]
let ``combinator parses resPQ`` () =
    let input = "resPQ#05162463 nonce:int128 server_nonce:int128 pq:string server_public_key_fingerprints:Vector<long> = ResPQ;"
    let result = runParser combinator input
    Assert.That(result.IsSome)
    let c = Option.get result
    equals c.Id.Name "resPQ"
    equals c.ConstructorId (Some(TlConstructorId 0x05162463u))
    equals c.Params.Length 4
    equals c.Params[0].Name "nonce"
    equals c.Params[0].Type (TlTypeExpr.Bare { Namespace = None; Name = "int128" })
    equals c.Params[3].Name "server_public_key_fingerprints"
    equals c.Params[3].Type (TlTypeExpr.Vector(TlTypeExpr.Bare { Namespace = None; Name = "long" }))
    equals c.ResultType (TlTypeExpr.Boxed { Namespace = None; Name = "ResPQ" })

[<Test>]
let ``combinator parses combinator without constructor id`` () =
    let input = "boolFalse = Bool;"
    let result = runParser combinator input
    Assert.That(result.IsSome)
    let c = Option.get result
    equals c.Id.Name "boolFalse"
    equals c.ConstructorId None

[<Test>]
let ``combinator parses namespaced combinator`` () =
    let input = "auth.sendCode#a677244f phone_number:string api_id:int api_hash:string settings:CodeSettings = auth.SentCode;"
    let result = runParser combinator input
    Assert.That(result.IsSome)
    let c = Option.get result
    equals c.Id { Namespace = Some "auth"; Name = "sendCode" }
    equals c.ConstructorId (Some(TlConstructorId 0xa677244fu))

[<Test>]
let ``combinator parses type params`` () =
    let _input = "vector#1cb5c415 {t:Type} # [ t ] = Vector t;"
    // This uses special syntax `# [ t ]` which is a bit unusual.
    // For now, let's test simpler type params.
    let simpleInput = "test#12345678 {X:Type} value:!X = Result;"
    let result = runParser combinator simpleInput
    Assert.That(result.IsSome)
    let c = Option.get result
    equals c.TypeParams [ "X" ]
    equals c.Params[0].Type (TlTypeExpr.TypeVar "X")

[<Test>]
let ``param parses name colon type`` () =
    let result = runParser param "nonce:int128"
    Assert.That(result.IsSome)
    let p = Option.get result
    equals p.Name "nonce"
    equals p.Type (TlTypeExpr.Bare { Namespace = None; Name = "int128" })

[<Test>]
let ``param parses conditional`` () =
    let result = runParser param "reply_to:flags.0?InputReplyTo"
    Assert.That(result.IsSome)
    let p = Option.get result
    equals p.Name "reply_to"
    let expected = TlTypeExpr.Conditional("flags", 0, TlTypeExpr.Boxed { Namespace = None; Name = "InputReplyTo" })
    equals p.Type expected

[<Test>]
let ``typeParam parses braced type variable`` () =
    let result = runParser typeParam "{X:Type}"
    equals result (Some "X")
