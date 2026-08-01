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

    [<Test>]
    let ``emitted files are LF-only, whatever the host OS uses`` () =
        let fields = [ flagsWord; presence "MyBoost" 2; optional "Title" "string" 0 ]

        let files =
            EmitCSharp.buildFiles ns [ Record("BoostsStatus", fields, 0xEEEEEEEEu) ] []

        for name, content in files do
            Assert.That(content, Does.Not.Contain "\r", NUnitString $"CR in {name}")
            Assert.That(content, Does.Contain "\n")


    // ── #117: bare vs boxed ────────────────────────────────────────────────
    //
    // The parser tests prove `vector<T>` and `Vector<T>` are different nodes.
    // These prove the emitter spends the difference: they are the only checks
    // in this repository that can see the wire shape, because a round-trip
    // over the emitted code cannot — it was 2506/2506 green with both defects
    // in place (Altergram #118).

    /// `future_salts#ae500895 ... salts:vector<future_salt>` — a bare vector
    /// of bare elements. Neither the `0x1CB5C415` header nor the element's own
    /// `0x0949d9dc` belongs on the wire; both of them did, and a real client
    /// reads the first as an element count of 482,092,053 and aborts.
    [<Test>]
    let ``a bare vector of bare elements writes neither cid`` () =
        let salt =
            Record("FutureSalt", [ field "ValidSince" "int32"; field "Salt" "int64" ], 0x0949D9DCu)

        let salts =
            Record(
                "FutureSalts",
                [ field "ReqMsgId" "int64"
                  field "Salts" (IrType.vectorOf true (IrType.bare "FutureSalt")) ],
                0xAE500895u
            )

        let source = flat (emit [ salt; salts ] [])

        // The vector: count and elements, through the generated bare codec.
        Assert.That(source, Does.Contain "TlBare.WriteVector<Test.Schema.FutureSalt>(w, Salts,")
        Assert.That(source, Does.Contain "TlBare.ReadVector<Test.Schema.FutureSalt>(r,")
        Assert.That(source, Does.Not.Contain "w.WriteVector<Test.Schema.FutureSalt>")
        // The element: its body, not its boxed form.
        Assert.That(source, Does.Contain "it.SerializeBody(w_);")
        Assert.That(source, Does.Contain "Test.Schema.FutureSalt.ReadBody(r_)")
        Assert.That(source, Does.Not.Contain "it.Serialize(w_);")
        Assert.That(syntaxErrors (emit [ salt; salts ] []), Is.Empty)

    /// The control on the same emitter run: `msgs_ack#62d6b459
    /// msg_ids:Vector<long>` is BOXED and must keep the header it always had.
    /// Without this pairing, "no 0x1CB5C415 anywhere" and "the emitter stopped
    /// writing vectors" are the same green.
    [<Test>]
    let ``a boxed vector still goes through the runtime vector codec`` () =
        let ack =
            Record("MsgsAck", [ field "MsgIds" (IrType.vectorOf false "int64") ], 0x62D6B459u)

        let source = flat (emit [ ack ] [])

        Assert.That(source, Does.Contain "w.WriteVector<long>(MsgIds,")
        Assert.That(source, Does.Contain "r.ReadVector<long>(")
        Assert.That(source, Does.Not.Contain "TlBare.")
        Assert.That(syntaxErrors (emit [ ack ] []), Is.Empty)

    /// `accessPointRule ... ips:vector<IpPort>` — a bare vector whose elements
    /// are boxed. The two bits are independent and the emitter has to spend
    /// them independently.
    [<Test>]
    let ``a bare vector of boxed elements drops only the vector header`` () =
        let ip = Record("IpPort", [ field "Ipv4" "int32" ], 0xD433AD73u)

        let rule =
            Record("AccessPointRule", [ field "Ips" (IrType.vectorOf true "IpPort") ], 0x4679B65Fu)

        let source = flat (emit [ ip; rule ] [])

        Assert.That(source, Does.Contain "TlBare.WriteVector<Test.Schema.IpPort>(w, Ips,")
        // Elements keep their own constructor id: they are boxed.
        Assert.That(source, Does.Contain "it.Serialize(w_);")
        Assert.That(source, Does.Contain "Test.Schema.IpPort.Deserialize(r_)")
        Assert.That(source, Does.Not.Contain "SerializeBody")

    /// A surface with no bare vector must emit no helper — the fix has to be
    /// invisible to a consumer whose schema does not need it.
    [<Test>]
    let ``the bare-vector helper is emitted only when something uses it`` () =
        let boxed = Record("MsgsAck", [ field "MsgIds" (IrType.vectorOf false "int64") ], 0x62D6B459u)
        let bare = Record("TlsHello", [ field "Blocks" (IrType.vectorOf true "int64") ], 0x11223344u)

        let names files = files |> List.map fst

        Assert.That(names (EmitCSharp.buildFiles ns [ boxed ] []), Does.Not.Contain "TlBare.g.cs")
        Assert.That(names (EmitCSharp.buildFiles ns [ bare ] []), Does.Contain "TlBare.g.cs")

    /// A bare reference resolves to a CONSTRUCTOR. Pointed at a boxed union it
    /// would emit `SomeUnionBase.ReadBody`, which does not exist, and the
    /// consumer would find out at their compile instead of ours.
    [<Test>]
    let ``a bare reference to a union base is refused at generation`` () =
        let union =
            Union("Peer", [ { Name = "PeerUser"; ConstructorId = 0x1u; AliasCids = []; Fields = [ field "Id" "int64" ] }
                            { Name = "PeerChat"; ConstructorId = 0x2u; AliasCids = []; Fields = [ field "Id" "int64" ] } ])

        let holder = Record("Holder", [ field "P" (IrType.bare "Peer") ], 0x99999999u)

        let ex = Assert.Throws<System.Exception>(fun () -> emit [ union; holder ] [] |> ignore)
        Assert.That(ex.Message, Does.Contain "bare reference")
        Assert.That(ex.Message, Does.Contain "Peer")

    // ── #116: fixed-width scalars ──────────────────────────────────────────

    /// `int128`/`int256` are N raw bytes. They used to collapse to `byte[]`,
    /// which already means the length-prefixed `bytes` primitive, so a 16-byte
    /// nonce went out as 1 length byte + 16 + 3 padding = 20.
    [<Test>]
    let ``fixed-width scalars are raw bytes of the declared width`` () =
        let fields =
            [ field "Nonce" IrType.Int128
              field "NewNonce" IrType.Int256
              field "Blob" "byte[]" ]

        let source = flat (emit [ Record("ResPQ", fields, 0x05162463u) ] [])

        Assert.That(source, Does.Contain "w.WriteRawBytes(Nonce);")
        Assert.That(source, Does.Contain "w.WriteRawBytes(NewNonce);")
        Assert.That(source, Does.Contain "r.ReadRawBytes(16)")
        Assert.That(source, Does.Contain "r.ReadRawBytes(32)")
        // The declared width is a wire invariant `byte[]` cannot state.
        Assert.That(source, Does.Contain "if (Nonce.Length != 16)")
        Assert.That(source, Does.Contain "if (NewNonce.Length != 32)")
        // Control: the real `bytes` primitive is untouched and still
        // length-prefixed, so this is a distinction and not a blanket change.
        Assert.That(source, Does.Contain "w.WriteBytes(Blob);")
        Assert.That(source, Does.Contain "r.ReadBytes()")
        Assert.That(syntaxErrors (emit [ Record("ResPQ", fields, 0x05162463u) ] []), Is.Empty)

    /// A default-constructed instance must already satisfy the width check.
    /// The zero value of an `int128` is sixteen zero bytes; `[]` is not a
    /// smaller nonce, it is a malformed message. Measured consequence of
    /// getting this wrong: Altergram's `tl-fuzz` builds a default instance to
    /// derive its truncation sweep, and 7 types silently dropped out of the
    /// sweep when `Serialize` threw on them (629 round-trips fell to 622).
    [<Test>]
    let ``a fixed-width field defaults to its declared width`` () =
        let fields = [ field "Nonce" IrType.Int128; field "NewNonce" IrType.Int256; field "Blob" "byte[]" ]
        let source = flat (emit [ Record("ResPQ", fields, 0x05162463u) ] [])

        Assert.That(source, Does.Contain "public byte[] Nonce = new byte[16];")
        Assert.That(source, Does.Contain "public byte[] NewNonce = new byte[32];")
        // A real `bytes` field still defaults to empty: it has no fixed width.
        Assert.That(source, Does.Contain "public byte[] Blob = [];")

    /// They are still `byte[]` to a caller — the width is a wire fact, not a
    /// CLR one, and inventing a type for it would break every call site.
    [<Test>]
    let ``fixed-width scalars keep the byte array spelling`` () =
        Assert.That(EmitCSharp.csType IrType.Int128, Is.EqualTo "byte[]")
        Assert.That(EmitCSharp.csType IrType.Int256, Is.EqualTo "byte[]")
        Assert.That(EmitCSharp.csType (IrType.Int128 + IrType.OptionSuffix), Is.EqualTo "byte[]?")