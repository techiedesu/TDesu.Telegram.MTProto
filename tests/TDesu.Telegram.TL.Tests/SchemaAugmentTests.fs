namespace TDesu.Telegram.TL.Tests

open NUnit.Framework
open TDesu.Telegram.TL
open TDesu.Telegram.TL.AST
open TDesu.Telegram.TL.Generator
open TDesu.Telegram.TL.Generator.Overrides

[<TestFixture>]
module SchemaAugmentTests =

    let private baseSchema =
        match AstFactory.parse (readTestData "test_schema.tl") with
        | Ok s -> s
        | Error e -> failwith e

    [<Test>]
    let ``fold: empty config is a no-op`` () =
        let result = SchemaAugment.fold OverrideConfig.empty baseSchema
        Assert.That(result.Functions.Length, Is.EqualTo(baseSchema.Functions.Length))
        Assert.That(result.Constructors.Length, Is.EqualTo(baseSchema.Constructors.Length))

    [<Test>]
    let ``fold: appends a function to the schema`` () =
        let config =
            { OverrideConfig.empty with
                ExtraCombinators = [
                    { Raw = "messages.fooBar#1234abcd peer:InputPeer = Bool;"
                      Section = Functions
                      Comment = "" }
                ] }
        let result = SchemaAugment.fold config baseSchema
        Assert.That(result.Functions.Length, Is.EqualTo(baseSchema.Functions.Length + 1))
        let added = result.Functions |> List.find (fun c -> c.Id.Name = "fooBar")
        Assert.That(added.Id.Namespace, Is.EqualTo(Some "messages"))

    [<Test>]
    let ``fold: overrides a schema entry on CID collision`` () =
        // Pick an existing function from the schema and override it.
        let existing =
            baseSchema.Functions
            |> List.head
        let (TlConstructorId existingCid) =
            existing.ConstructorId |> Option.defaultWith (fun () -> failwith "schema head lacks CID")
        // Keep the same CID so the conflict fires; change the body.
        let raw =
            $"overridden.method#%08x{existingCid} flag:int = Bool;"
        let config =
            { OverrideConfig.empty with
                ExtraCombinators = [
                    { Raw = raw; Section = Functions; Comment = "" }
                ] }
        let result = SchemaAugment.fold config baseSchema
        // Total function count unchanged: schema entry dropped, extra added.
        Assert.That(result.Functions.Length, Is.EqualTo(baseSchema.Functions.Length))
        // The overridden entry should be present.
        let found =
            result.Functions
            |> List.exists (fun c -> c.Id.Name = "method" && c.Id.Namespace = Some "overridden")
        Assert.That(found, Is.True)

    [<Test>]
    let ``fold: rejects a combinator with no explicit CID`` () =
        let config =
            { OverrideConfig.empty with
                ExtraCombinators = [
                    { Raw = "messages.noExplicitCid peer:InputPeer = Bool;"
                      Section = Functions
                      Comment = "" }
                ] }
        Assert.Throws<System.Exception>(fun () ->
            SchemaAugment.fold config baseSchema |> ignore) |> ignore
