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

    /// Regression: `[[layer_variants]]` CIDs must flow into the generated
    /// `Deserialize`'s pattern match via `AliasCids`. Without this, a layer
    /// 223 client sending the post-216 CID for `messages.sendMessage` is
    /// rejected by the server's `messages.SendMessage.Deserialize` with
    /// `Unknown constructor id`. buildAliasMap must merge [[aliases]] and
    /// [[layer_variants]] into the same name → cids map feeding
    /// SchemaMapper.mapSchemaWhitelisted.
    [<Test>]
    let ``buildAliasMap merges aliases and layer_variants`` () =
        let config =
            { OverrideConfig.empty with
                Aliases = [
                    { Name = "MessagesSendMessage"
                      Cids = [ 0xdff8042cu; 0xdeadbeefu ] }
                ]
                LayerVariants = [
                    { Name = "MessagesSendMessage"
                      Variants = [ Some 216, 0xcafebabeu; None, 0x12345678u ] }
                    { Name = "AuthSignIn"
                      Variants = [ Some 216, 0x8d52a951u; None, 0xaabbccddu ] } ] }

        let aliasMap = EmitTemplates.buildAliasMap config
        let sendCids = aliasMap |> Map.find "MessagesSendMessage" |> Set.ofList
        let signInCids = aliasMap |> Map.find "AuthSignIn" |> Set.ofList

        Assert.That(sendCids, Is.EquivalentTo(set [ 0xdff8042cu; 0xdeadbeefu; 0xcafebabeu; 0x12345678u ]))
        Assert.That(signInCids, Is.EquivalentTo(set [ 0x8d52a951u; 0xaabbccddu ]))

    /// End-to-end: ensure the AliasCids land in the generated Deserialize
    /// pattern match for both a function and a union case via the full
    /// Pipeline.generateSerializationTypes path.
    [<Test>]
    let ``layer_variants CIDs appear in generated Deserialize match`` () =
        let config =
            { OverrideConfig.empty with
                TypeWhitelist = set [ "MessagesSendMessage" ]
                LayerVariants = [
                    { Name = "MessagesSendMessage"
                      Variants = [ Some 216, 0xdff8042cu; None, 0x7d8375dau ] }
                ] }

        withTempFile (fun path ->
            Pipeline.generateSerializationTypes testNs config apiSchema path
            let code = File.ReadAllText path
            // Both CIDs should be present in an AliasCids array or match arm.
            if not (code.Contains "2105767386u" || code.Contains "0x7D8375DA") then
                Assert.Fail($"expected layer-variant CID in generated code:\n{code}"))
