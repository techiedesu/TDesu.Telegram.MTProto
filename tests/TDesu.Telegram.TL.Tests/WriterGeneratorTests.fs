namespace TDesu.Telegram.TL.Tests

open NUnit.Framework
open TDesu.Telegram.TL
open TDesu.Telegram.TL.Generator
open TDesu.Telegram.TL.Generator.Overrides

[<TestFixture>]
module WriterGeneratorTests =

    let private schema =
        match AstFactory.parse (readTestData "test_schema.tl") with
        | Ok s -> s
        | Error e -> failwith e

    [<Test>]
    let ``generateWriterModule - basic types`` () =
        let whitelist =
            set
                [ "inputPeerEmpty"
                  "inputPeerSelf"
                  "inputPeerChat"
                  "boolFalse"
                  "boolTrue"
                  "messageEntity"
                  "user"
                  "message" ]

        let actual = EmitWriters.generateWriterModule "TDesu.Serialization" schema whitelist Set.empty [] [] Set.empty
        assertMatchesSnapshot actual "WriterGen_WriterModule"

    /// Regression: when a whitelisted record references a TL union/single
    /// that is NOT in the writers whitelist, the field falls back to
    /// `byte[]` semantics — but on the wire it must be written raw
    /// (caller pre-serializes), not wrapped in TL `bytes` (length prefix).
    /// Pre-2026-04-17 the generator emitted `WriteBytes`, which corrupted
    /// every such field. SedBot saw this as
    /// `documentAttributeSticker.stickerset:InputStickerSet` writing
    /// 4 zero bytes that clients then misread as
    /// `TypeNotFoundError(constructor=0x00000000)`.
    [<Test>]
    let ``opaque-ref field outside writers whitelist emits WriteRawBytes`` () =
        let inlineSchema =
            "boolFalse#bc799737 = Bool;\n\
             boolTrue#997275b5 = Bool;\n\
             inputStickerSetEmpty#ffb62b95 = InputStickerSet;\n\
             documentAttributeSticker#6319d612 alt:string stickerset:InputStickerSet = DocumentAttribute;\n"
        let parsed =
            match AstFactory.parse inlineSchema with
            | Ok s -> s
            | Error e -> failwith e
        // documentAttributeSticker is on the writers whitelist, but
        // InputStickerSet is NOT — so its `stickerset` field hits the
        // resolveFieldType fallback branch.
        let whitelist = set [ "documentAttributeSticker"; "boolFalse"; "boolTrue" ]
        let actual = EmitWriters.generateWriterModule "TDesu.Serialization" parsed whitelist Set.empty [] [] Set.empty
        let containsAny (needles: string list) (s: string) = needles |> List.exists s.Contains
        // Field labels are emitted in PascalCase since 2026-04-17.
        let writeRaw = [ "w.WriteRawBytes(p.Stickerset)"; "w.WriteRawBytes(p_.Stickerset)" ]
        let writeLenPrefixed = [ "w.WriteBytes(p.Stickerset)"; "w.WriteBytes(p_.Stickerset)" ]
        if not (containsAny writeRaw actual) then
            Assert.Fail("stickerset:InputStickerSet must use WriteRawBytes (raw blob), not WriteBytes (length-prefixed). Generated:\n" + actual)
        if containsAny writeLenPrefixed actual then
            Assert.Fail("writer must NOT emit WriteBytes for opaque-ref union fields. Generated:\n" + actual)

    /// Structural overlays (0.2.3+): [[structural_overlays]] entries add
    /// scalar fields at a newer layer without needing a hand-rolled hotfix.
    /// The struct grows to include the new field unconditionally, and the
    /// writer wraps its write in `if layer > max_old_layer then ...`. CID
    /// dispatch still goes through `[[layer_variants]]`.
    [<Test>]
    let ``structural overlay splices extra scalar and layer-gates the write`` () =
        let inlineSchema =
            "boolFalse#bc799737 = Bool;\n\
             boolTrue#997275b5 = Bool;\n\
             dialog#d58a08c6 unread_reactions_count:int = Dialog;\n"
        let parsed =
            match AstFactory.parse inlineSchema with
            | Ok s -> s
            | Error e -> failwith e
        let overlay = {
            Name = "Dialog"
            MaxOldLayer = 216
            ExtraFields = [
                { After = "unread_reactions_count"
                  Name = "unread_poll_votes_count"
                  Type = "int" }
            ]
        }
        let layerVariant = {
            Name = "Dialog"
            Variants = [ Some 216, 0xd58a08c6u; None, 0xfc89f7f3u ]
        }
        let whitelist = set [ "dialog"; "boolFalse"; "boolTrue" ]
        let actual =
            EmitWriters.generateWriterModule
                "TDesu.Serialization"
                parsed
                whitelist
                (set [ "dialog" ])
                [ layerVariant ]
                [ overlay ]
                Set.empty

        // Struct now exposes the extra field.
        let mustContain (needle: string) (label: string) =
            if not (actual.Contains needle) then
                Assert.Fail($"expected generated output to contain %s{label}:\n%s{needle}\n---\n%s{actual}")
        mustContain "UnreadPollVotesCount: int32" "UnreadPollVotesCount field in WriteDialogParams"
        // Writer takes `layer` (driven by writer_layer_types + overlay presence).
        mustContain "writeDialog (w: TlWriteBuffer) (layer: int)" "writeDialog signature"
        // Layer-gated emission of the extra field.
        mustContain "if layer > 216 then" "layer-gate conditional"
        mustContain "w.WriteInt32(p.UnreadPollVotesCount)" "extra-field write under the gate"
        // CID dispatch via GeneratedLayerCid.
        mustContain "GeneratedLayerCid.dialog layer" "layer-variant CID dispatch"
