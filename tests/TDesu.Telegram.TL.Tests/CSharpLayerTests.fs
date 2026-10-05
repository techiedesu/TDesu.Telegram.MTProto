namespace TDesu.Telegram.TL.Tests

open NUnit.Framework
open TDesu.Telegram.TL
open TDesu.Telegram.TL.Generator
open TDesu.Telegram.TL.Generator.Overrides

[<TestFixture>]
module CSharpLayerTests =
    let private schema text =
        match text |> Downloader.preprocess |> AstFactory.parse with
        | Ok parsed -> SchemaMapper.mapSchema parsed
        | Error error -> failwith error

    let private plan mappings oldText currentText =
        let oldTypes, oldFunctions = schema oldText
        let types, functions = schema currentText
        CSharpLayers.plan 228 mappings oldTypes oldFunctions types functions |> ignore

    let private mapping =
        { Source = "Old"
          Target = "Current"
          NestedField = None
          NestedConstructor = None }

    [<Test>]
    let ``primitive retyping cannot corrupt the following field`` () =
        Assert.That(
            TestDelegate(fun () ->
                plan [ mapping ]
                    "old#11111111 value:int suffix:string = Old;"
                    "current#22222222 value:long suffix:string = Current;"),
            Throws.Exception)

    [<Test>]
    let ``a new mandatory field needs an explicit source`` () =
        Assert.That(
            TestDelegate(fun () ->
                plan []
                    "item#11111111 value:int = Item;"
                    "item#22222222 value:int extra:string = Item;"),
            Throws.Exception)

    [<Test>]
    let ``a split field cannot bind both root and action`` () =
        let nested = { mapping with NestedField = Some "Action"; NestedConstructor = Some "ActionValue" }
        Assert.That(
            TestDelegate(fun () ->
                plan [ nested ]
                    "old#11111111 value:int = Old;"
                    "current#22222222 value:int action:Action = Current;\nactionValue#33333333 value:int = Action;\nactionEmpty#44444444 = Action;"),
            Throws.Exception)

    [<Test>]
    let ``duplicate source mappings cannot choose a writer by accident`` () =
        Assert.That(
            TestDelegate(fun () ->
                plan [ mapping; mapping ]
                    "old#11111111 value:int = Old;"
                    "current#22222222 value:int = Current;"),
            Throws.Exception)

    [<Test>]
    let ``reference retyping requires a declared semantic mapping`` () =
        Assert.That(
            TestDelegate(fun () ->
                plan [ mapping ]
                    "old#11111111 value:OldValue = Old;\noldValue#33333333 value:int = OldValue;"
                    "current#22222222 value:NewValue = Current;\nnewValue#44444444 value:int = NewValue;"),
            Throws.Exception)
