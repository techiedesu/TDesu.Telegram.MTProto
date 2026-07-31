namespace TDesu.Telegram.TL.Tests

open NUnit.Framework
open Microsoft.CodeAnalysis.CSharp
open TDesu.Telegram.TL.Generator

/// The C# backend had no tests at all, which is how a wrong flag-bit
/// serialization and a false "these types are not emitted" belief survived.
/// These build the IR directly (no schema parsing) so a failure points at the
/// emitter and nothing else.
[<TestFixture>]
module EmitCSharpTests =

    let private ns = "Test.Schema"

    let private field name ty =
        { Name = name
          RecordName = name
          FSharpType = ty
          IsOptional = false
          FlagField = None
          FlagBit = None
          LayerGate = None }

    let private flagsWord = { field "flags" "int32" with FlagField = None }

    /// A `flags.N?true` field: the bit itself is the value.
    let private presence name bit =
        { field name "bool" with
            FlagField = Some "flags"
            FlagBit = Some bit
            IsOptional = false }

    /// A `flags.N?T` field: present iff the bit is set.
    let private optional name ty bit =
        { field name (ty + " option") with
            FlagField = Some "flags"
            FlagBit = Some bit
            IsOptional = true }

    let private emit types functions = EmitCSharp.buildModule ns types functions

    /// Roslyn reflows `if (c) stmt;` onto two lines; assertions about emitted
    /// code care about the tokens, not the layout.
    let private flat (s: string) =
        System.Text.RegularExpressions.Regex.Replace(s, @"\s+", " ")

    let private syntaxErrors (source: string) =
        CSharpSyntaxTree.ParseText(source).GetDiagnostics()
        |> Seq.filter (fun d -> d.Severity = Microsoft.CodeAnalysis.DiagnosticSeverity.Error)
        |> Seq.map string
        |> List.ofSeq

    // ── Shared flag bits ───────────────────────────────────────────────────

    [<Test>]
    let ``fields sharing one flag bit get a consistency guard`` () =
        // premium.boostsStatus shape: my_boost:flags.2?true + my_boost_slots:flags.2?Vector<int>
        let fields =
            [ flagsWord; presence "MyBoost" 2; optional "MyBoostSlots" "int32 array" 2 ]

        let source = emit [ Record("BoostsStatus", fields, 0x11111111u) ] []

        Assert.That(flat source, Does.Contain "share flags bit 2")
        Assert.That(flat source, Does.Contain "throw new System.InvalidOperationException")
        // The bit is derived ONCE, from the first field of the group.
        Assert.That(flat source, Does.Contain "if (MyBoost) flags |= (1 << 2);")
        Assert.That(syntaxErrors source, Is.Empty)

    [<Test>]
    let ``one field per bit stays guard-free`` () =
        let fields =
            [ flagsWord; optional "Title" "string" 0; optional "Count" "int32" 1 ]

        let source = emit [ Record("Simple", fields, 0x22222222u) ] []

        Assert.That(flat source, Does.Not.Contain "InvalidOperationException")
        Assert.That(flat source, Does.Contain "if (Title != null) flags |= (1 << 0);")
        Assert.That(flat source, Does.Contain "if (Count.HasValue) flags |= (1 << 1);")

    // ── Structural shape ───────────────────────────────────────────────────

    [<Test>]
    let ``union emits an abstract base and dispatches aliases to the same case`` () =
        let cases =
            [ { Name = "PeerUser"
                ConstructorId = 0x33333333u
                AliasCids = [ 0x44444444u ]
                Fields = [ field "UserId" "int64" ] }
              { Name = "PeerChat"
                ConstructorId = 0x55555555u
                AliasCids = []
                Fields = [ field "ChatId" "int64" ] } ]

        let source = emit [ Union("Peer", cases) ] []

        Assert.That(flat source, Does.Contain "public abstract class Peer : ITlObject")
        Assert.That(flat source, Does.Contain "public sealed class PeerUser : Peer")
        Assert.That(flat source, Does.Contain "0x33333333u or 0x44444444u => PeerUser.ReadBody(r)")
        Assert.That(flat source, Does.Contain "Unknown constructor")
        Assert.That(syntaxErrors source, Is.Empty)

    [<Test>]
    let ``a union case sharing the union name pushes the base to <Name>Base`` () =
        let cases =
            [ { Name = "User"
                ConstructorId = 0x66666666u
                AliasCids = []
                Fields = [ field "Id" "int64" ] } ]

        let source = emit [ Union("User", cases) ] []

        Assert.That(flat source, Does.Contain "public abstract class UserBase : ITlObject")
        Assert.That(flat source, Does.Contain "public sealed class User : UserBase")

    [<Test>]
    let ``a function exposes both a cid-consuming and a fields-only reader`` () =
        let fn =
            { Name = "MessagesGetHistory"
              ConstructorId = 0x77777777u
              AliasCids = []
              Params = [ field "Limit" "int32" ]
              ReturnType = "MessagesMessages" }

        let source = emit [] [ fn ]

        Assert.That(flat source, Does.Contain "public static MessagesGetHistory DeserializeFields(TlReadBuffer r)")
        Assert.That(flat source, Does.Contain "public static MessagesGetHistory Deserialize(TlReadBuffer r)")
        Assert.That(flat source, Does.Contain "r.ReadConstructorId();")
        Assert.That(syntaxErrors source, Is.Empty)

    // ── Request → response map ─────────────────────────────────────────────

    [<Test>]
    let ``the return-type map expands a union response to every case cid`` () =
        let cases =
            [ { Name = "MessagesMessages"
                ConstructorId = 0x88888888u
                AliasCids = []
                Fields = [] }
              { Name = "MessagesMessagesSlice"
                ConstructorId = 0x99999999u
                AliasCids = []
                Fields = [] } ]

        let fn =
            { Name = "MessagesGetHistory"
              ConstructorId = 0xAAAAAAAAu
              AliasCids = []
              Params = []
              ReturnType = "MessagesMessages" }

        let source = emit [ Union("MessagesMessages", cases) ] [ fn ]

        Assert.That(flat source, Does.Contain "public static class GeneratedReturnTypes")
        Assert.That(flat source, Does.Contain "[0xAAAAAAAAu] = [0x88888888u, 0x99999999u]")

    [<Test>]
    let ``a bare-primitive response contributes no map entry`` () =
        let fn =
            { Name = "PingDelayDisconnect"
              ConstructorId = 0xBBBBBBBBu
              AliasCids = []
              Params = []
              ReturnType = "int64" }

        let source = emit [] [ fn ]

        Assert.That(flat (source.Substring(source.IndexOf "GeneratedReturnTypes")), Does.Not.Contain "0xBBBBBBBBu")

    // ── Whole-file shape ───────────────────────────────────────────────────

    [<Test>]
    let ``record with flags matches the golden file`` () =
        let fields =
            [ field "Id" "int64"
              flagsWord
              presence "Pinned" 0
              optional "Title" "string" 1
              field "Peers" "Peer array" ]

        let source = emit [ Record("Dialog", fields, 0xCCCCCCCCu) ] []
        assertMatchesSnapshot source "EmitCSharp_Record_WithFlags"

    [<Test>]
    let ``every emitted file carries the banner and nullable directive`` () =
        let files = EmitCSharp.buildFiles ns [ Record("Ping", [ field "Id" "int64" ], 0xDDDDDDDDu) ] []

        for name, content in files do
            Assert.That(content, Does.StartWith "// <auto-generated>", NUnitString $"banner missing in {name}")
            Assert.That(flat content, Does.Contain "#nullable enable")
            Assert.That(flat content, Does.Contain $"namespace {ns};")
            Assert.That(syntaxErrors content, Is.Empty, NUnitString $"invalid C# in {name}")
