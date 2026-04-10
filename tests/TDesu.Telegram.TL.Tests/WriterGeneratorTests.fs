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

        let actual = EmitWriters.generateWriterModule schema whitelist Set.empty []
        assertMatchesSnapshot actual "WriterGen_WriterModule"
