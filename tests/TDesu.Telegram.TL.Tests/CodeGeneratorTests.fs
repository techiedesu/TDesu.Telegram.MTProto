namespace TDesu.Telegram.TL.Tests

open System.IO
open NUnit.Framework
open TDesu.Telegram.TL
open TDesu.Telegram.TL.Generator
open TDesu.Telegram.TL.Generator.Overrides

[<TestFixture>]
module CodeGeneratorTests =

    let private emptyConfig = OverrideConfig.empty

    let private apiSchema =
        match AstFactory.parse (readTestData "test_schema.tl") with
        | Ok s -> s
        | Error e -> failwith e

    let private mtprotoSchema =
        match AstFactory.parse (readTestData "test_mtproto.tl") with
        | Ok s -> s
        | Error e -> failwith e

    let private withTempFile (action: string -> unit) =
        let path = Path.Combine(Path.GetTempPath(), $"tl_test_{System.Guid.NewGuid()}.g.fs")

        try
            action path
        finally
            if File.Exists path then
                File.Delete path

    let private testNs = "TDesu.Serialization"

    [<Test>]
    let ``generateCidModule`` () =
        let actual = EmitTemplates.generateCidModule testNs emptyConfig mtprotoSchema apiSchema
        assertMatchesSnapshot actual "CodeGen_CidModule"

    [<Test>]
    let ``generateClientCids`` () =
        withTempFile (fun path ->
            EmitTemplates.generateClientCids "TDesu.MTProto.Client.Api" apiSchema path
            assertMatchesSnapshot (File.ReadAllText path) "CodeGen_ClientCids")

    [<Test>]
    let ``generateCoverageValidator`` () =
        withTempFile (fun path ->
            EmitTemplates.generateCoverageValidator testNs emptyConfig apiSchema path
            assertMatchesSnapshot (File.ReadAllText path) "CodeGen_CoverageValidator")

    [<Test>]
    let ``generateReturnTypeMap`` () =
        withTempFile (fun path ->
            EmitTemplates.generateReturnTypeMap testNs emptyConfig apiSchema path
            assertMatchesSnapshot (File.ReadAllText path) "CodeGen_ReturnTypeMap")

    [<Test>]
    let ``generateLayerAliases`` () =
        withTempFile (fun path ->
            EmitTemplates.generateLayerAliases testNs mtprotoSchema apiSchema path
            assertMatchesSnapshot (File.ReadAllText path) "CodeGen_LayerAliases")

    [<Test>]
    let ``generateRoundTripTests`` () =
        let config =
            { OverrideConfig.empty with
                TypeWhitelist = set [ "MessagesSendMessage"; "AuthSignIn" ] }

        withTempFile (fun path ->
            EmitTemplates.generateRoundTripTests
                "TDesu.MTProto.Tests.GeneratedRoundTripTests" testNs config apiSchema path
            assertMatchesSnapshot (File.ReadAllText path) "CodeGen_RoundTripTests")

    [<Test>]
    let ``generateSerializationTypes`` () =
        let config =
            { OverrideConfig.empty with
                TypeWhitelist = set [ "MessagesSendMessage"; "AuthSignIn" ] }

        withTempFile (fun path ->
            Pipeline.generateSerializationTypes testNs config apiSchema path
            assertMatchesSnapshot (normalizeTimestamp (File.ReadAllText path)) "CodeGen_SerializationTypes")
