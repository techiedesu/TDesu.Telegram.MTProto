namespace TDesu.Telegram.TL.Tests

open NUnit.Framework
open TDesu.Telegram.TL
open TDesu.Telegram.TL.Generator

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

        let actual = EmitWriters.generateWriterModule "TDesu.Serialization" schema whitelist Set.empty [] Set.empty
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
        let actual = EmitWriters.generateWriterModule "TDesu.Serialization" parsed whitelist Set.empty [] Set.empty
        let containsAny (needles: string list) (s: string) = needles |> List.exists s.Contains
        let writeRaw = [ "w.WriteRawBytes(p.stickerset)"; "w.WriteRawBytes(p_.stickerset)" ]
        let writeLenPrefixed = [ "w.WriteBytes(p.stickerset)"; "w.WriteBytes(p_.stickerset)" ]
        if not (containsAny writeRaw actual) then
            Assert.Fail("stickerset:InputStickerSet must use WriteRawBytes (raw blob), not WriteBytes (length-prefixed). Generated:\n" + actual)
        if containsAny writeLenPrefixed actual then
            Assert.Fail("writer must NOT emit WriteBytes for opaque-ref union fields. Generated:\n" + actual)
