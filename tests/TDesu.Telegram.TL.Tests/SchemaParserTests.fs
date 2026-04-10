module TDesu.Telegram.TL.Tests.SchemaParserTests

open NUnit.Framework
open TDesu.FSharp
open TDesu.FSharp.Operators
open TDesu.Telegram.TL
open TDesu.Telegram.TL.AST
open TDesu.Telegram.TL.Generator
open TDesu.Telegram.TL.Tests

[<Test>]
let ``parse simple schema with constructors`` () =
    let input = """boolFalse#bc799737 = Bool;
boolTrue#997275b5 = Bool;
"""
    let result = AstFactory.parse input
    Assert.That(Result.isOk result)
    let schema = Result.get result
    equals schema.Constructors.Length 2
    equals schema.Functions.Length 0
    equals schema.Constructors[0].Id.Name "boolFalse"
    equals schema.Constructors[1].Id.Name "boolTrue"

[<Test>]
let ``parse schema with functions section`` () =
    let input = """boolFalse#bc799737 = Bool;
---functions---
auth.sendCode#a677244f phone_number:string api_id:int api_hash:string settings:CodeSettings = auth.SentCode;
"""
    let result = AstFactory.parse input
    Assert.That(Result.isOk result)
    let schema = Result.get result
    equals schema.Constructors.Length 1
    equals schema.Functions.Length 1
    equals schema.Functions[0].Id { Namespace = Some "auth"; Name = "sendCode" }

[<Test>]
let ``parse schema with layer directive`` () =
    let input = """// LAYER 185
boolFalse#bc799737 = Bool;
"""
    let result = AstFactory.parse input
    Assert.That(Result.isOk result)
    let schema = Result.get result
    equals schema.Layer (Some 185)

[<Test>]
let ``parse schema with comments`` () =
    let input = """// This is a comment
boolFalse#bc799737 = Bool;
// Another comment
boolTrue#997275b5 = Bool;
"""
    let result = AstFactory.parse input
    Assert.That(Result.isOk result)
    let schema = Result.get result
    equals schema.Constructors.Length 2

[<Test>]
let ``parse schema with types and functions sections`` () =
    let input = """---types---
boolFalse#bc799737 = Bool;
boolTrue#997275b5 = Bool;
---functions---
auth.sendCode#a677244f phone_number:string api_id:int api_hash:string settings:CodeSettings = auth.SentCode;
"""
    let result = AstFactory.parse input
    Assert.That(Result.isOk result)
    let schema = Result.get result
    equals schema.Constructors.Length 2
    equals schema.Functions.Length 1

[<Test>]
let ``parse resPQ combinator in schema`` () =
    let input = """resPQ#05162463 nonce:int128 server_nonce:int128 pq:string server_public_key_fingerprints:Vector<long> = ResPQ;
"""
    let result = AstFactory.parse input
    Assert.That(Result.isOk result)
    let schema = Result.get result
    equals schema.Constructors.Length 1
    let c = schema.Constructors[0]
    equals c.Params.Length 4
    equals c.ResultType (TlTypeExpr.Boxed { Namespace = None; Name = "ResPQ" })

[<Test>]
let ``parse empty schema`` () =
    let input = ""
    let result = AstFactory.parse input
    Assert.That(Result.isOk result)
    let schema = Result.get result
    equals schema.Constructors.Length 0
    equals schema.Functions.Length 0

// --- tdlib real schema tests ---

[<Test>]
let ``parse tdlib mtproto_api.tl`` () =
    let text = readTestData "tdlib_mtproto.tl" |> Downloader.preprocess
    match AstFactory.parse text with
    | Ok schema ->
        Assert.That(schema.Constructors.Length, Is.GreaterThan(20))
        Assert.That(schema.Functions.Length, Is.GreaterThan(3))
        let resPQ = schema.Constructors |> List.tryFind (fun c -> c.Id.Name = "resPQ")
        Assert.That(resPQ.IsSome, Is.True)
        equals resPQ.Value.Params.Length 4
    | Error e ->
        Assert.Fail(e)

[<Test>]
let ``parse tdlib telegram_api.tl`` () =
    let text = readTestData "tdlib_telegram_api.tl" |> Downloader.preprocess
    match AstFactory.parse text with
    | Ok schema ->
        Assert.That(schema.Constructors.Length, Is.GreaterThan(500))
        Assert.That(schema.Functions.Length, Is.GreaterThan(100))
        let boolFalse = schema.Constructors |> List.tryFind (fun c -> c.Id.Name = "boolFalse")
        Assert.That(boolFalse.IsSome, Is.True)
        let inputPeerEmpty = schema.Constructors |> List.tryFind (fun c -> c.Id.Name = "inputPeerEmpty")
        Assert.That(inputPeerEmpty.IsSome, Is.True)
    | Error e ->
        Assert.Fail(e)
