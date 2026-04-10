namespace TDesu.Telegram.TL.Tests

open NUnit.Framework
open TDesu.Telegram.TL
open TDesu.Telegram.TL.Generator

[<TestFixture>]
module AstBuilderTests =

    let private schema =
        match AstFactory.parse (readTestData "test_schema.tl") with
        | Ok s -> s
        | Error e -> failwith e

    let private types, functions = SchemaMapper.mapSchema schema

    let private findRecord name =
        types
        |> List.pick (function
            | Record(n, f, c) when n = name -> Some(f, c)
            | _ -> None)

    let private findUnion name =
        types
        |> List.pick (function
            | Union(n, c) when n = name -> Some c
            | _ -> None)

    [<Test>]
    let ``buildRecordCode - simple record without flags`` () =
        let fields, cid = findRecord "MessageEntity"
        let actual = EmitTypes.buildRecordCode "MessageEntity" fields cid
        assertMatchesSnapshot actual "AstBuilder_Record_Simple"

    [<Test>]
    let ``buildRecordCode - record with flags`` () =
        let fields, cid = findRecord "Message"
        let actual = EmitTypes.buildRecordCode "Message" fields cid
        assertMatchesSnapshot actual "AstBuilder_Record_WithFlags"

    [<Test>]
    let ``buildRecordCode - record with nested types`` () =
        let fields, cid = findRecord "User"
        let actual = EmitTypes.buildRecordCode "User" fields cid
        assertMatchesSnapshot actual "AstBuilder_Record_User"

    [<Test>]
    let ``buildUnionCode - simple union`` () =
        let cases = findUnion "Bool"
        let actual = EmitTypes.buildUnionCode "Bool" cases
        assertMatchesSnapshot actual "AstBuilder_Union_Bool"

    [<Test>]
    let ``buildUnionCode - union with fields`` () =
        let cases = findUnion "InputPeer"
        let actual = EmitTypes.buildUnionCode "InputPeer" cases
        assertMatchesSnapshot actual "AstBuilder_Union_InputPeer"

    [<Test>]
    let ``buildFunctionCode - simple function`` () =
        let func = functions |> List.find (fun f -> f.Name = "MessagesSendMessage")
        let actual = EmitTypes.buildFunctionCode func
        assertMatchesSnapshot actual "AstBuilder_Function_SendMessage"

    [<Test>]
    let ``buildFunctionCode - multiple params`` () =
        let func = functions |> List.find (fun f -> f.Name = "AuthSignIn")
        let actual = EmitTypes.buildFunctionCode func
        assertMatchesSnapshot actual "AstBuilder_Function_SignIn"

    [<Test>]
    let ``buildModule - full schema`` () =
        let actual = EmitTypes.buildModule "Test.Namespace" "TestModule" types functions
        assertMatchesSnapshot (normalizeTimestamp actual) "AstBuilder_Module"
