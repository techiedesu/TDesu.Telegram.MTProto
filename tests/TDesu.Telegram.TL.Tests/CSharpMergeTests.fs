namespace TDesu.Telegram.TL.Tests

open NUnit.Framework
open TDesu.Telegram.TL
open TDesu.Telegram.TL.Generator

/// `--target csharp --mtproto-schema <path>`: mtproto.tl declarations are folded
/// into the api.tl surface, with api.tl winning every name collision.
[<TestFixture>]
module CSharpMergeTests =

    let private parse (fileName: string) =
        match readTestData fileName |> Downloader.preprocess |> AstFactory.parse with
        | Ok s -> s
        | Error e -> failwith e

    let private apiSchema = parse "merge_api.tl"
    let private mtSchema = parse "merge_mtproto.tl"

    let private apiTypes, apiFunctions = SchemaMapper.mapSchema apiSchema
    let private mergedTypes, mergedFunctions, skipped =
        SchemaMapper.mergeMtprotoForCSharp apiSchema mtSchema

    let private typeNames =
        mergedTypes |> List.map (function Record(n, _, _) -> n | Union(n, _) -> n)

    let private findRecord name =
        mergedTypes
        |> List.tryPick (function
            | Record(n, fields, cid) when n = name -> Some(fields, cid)
            | _ -> None)

    [<Test>]
    let ``merge: unique mtproto constructors reach the surface`` () =
        Assert.That(typeNames, Does.Contain "BindAuthKeyInner")
        Assert.That(typeNames, Does.Contain "Pong")
        // …and they carry the mtproto constructor ids, not invented ones.
        match findRecord "BindAuthKeyInner" with
        | None -> Assert.Fail "BindAuthKeyInner was not emitted"
        | Some(fields, cid) ->
            Assert.That(cid, Is.EqualTo 0x75a3f765u)
            Assert.That(fields.Length, Is.EqualTo 5)

    [<Test>]
    let ``merge: colliding mtproto constructor is dropped`` () =
        // mtproto's `message` maps to the same C# class as api's `message`.
        Assert.That(
            skipped
            |> List.contains
                { SchemaMapper.Declaration = "message"; CSharpName = "Message"; Owner = "api.tl" },
            Is.True)
        Assert.That(typeNames |> List.filter ((=) "Message") |> List.length, Is.EqualTo 1)

    [<Test>]
    let ``merge: colliding mtproto function is dropped`` () =
        Assert.That(
            skipped
            |> List.contains { SchemaMapper.Declaration = "ping"; CSharpName = "Ping"; Owner = "api.tl" },
            Is.True)
        Assert.That(
            mergedFunctions |> List.filter (fun f -> f.Name = "Ping") |> List.length,
            Is.EqualTo 1)

    [<Test>]
    let ``merge: an mtproto self-collision drops the function, not the type`` () =
        // `rpc_drop_answer` is both an mtproto function and the boxed
        // `RpcDropAnswer` union; the union must survive so the return-type map
        // has something to point at.
        let api = parse "merge_api.tl"
        let mt = parse "tdlib_mtproto.tl"
        let types, functions, skips = SchemaMapper.mergeMtprotoForCSharp api mt
        let names = types |> List.map (function Record(n, _, _) -> n | Union(n, _) -> n)
        Assert.That(names, Does.Contain "RpcDropAnswer")
        Assert.That(functions |> List.exists (fun f -> f.Name = "RpcDropAnswer"), Is.False)
        Assert.That(
            skips
            |> List.contains
                { SchemaMapper.Declaration = "rpc_drop_answer"
                  CSharpName = "RpcDropAnswer"
                  Owner = "mtproto.tl" },
            Is.True)

    [<Test>]
    let ``merge: the api declaration survives untouched`` () =
        let apiMessage =
            apiTypes
            |> List.pick (function Record("Message", f, c) -> Some(f, c) | _ -> None)
        Assert.That(findRecord "Message" = Some apiMessage, Is.True)
        Assert.That(snd apiMessage, Is.EqualTo 0x5a686d7cu)
        // The surviving `Ping` is api's, not mtproto's #7abe77ec.
        let ping = mergedFunctions |> List.find (fun f -> f.Name = "Ping")
        Assert.That(ping.ConstructorId, Is.EqualTo 0x12ab34cdu)
        Assert.That(ping.ReturnType, Is.EqualTo "bool")

    [<Test>]
    let ``merge: the api prefix is bit-identical to the unmerged surface`` () =
        // `--mtproto-schema` is opt-in; it must only ever append.
        Assert.That(mergedTypes |> List.truncate apiTypes.Length = apiTypes, Is.True)
        Assert.That(mergedFunctions |> List.truncate apiFunctions.Length = apiFunctions, Is.True)

    [<Test>]
    let ``merge: no duplicate top-level name reaches the emitter`` () =
        // EmitCSharp.setup hard-fails on a duplicate; this is the real contract.
        Assert.DoesNotThrow(fun () ->
            EmitCSharp.buildModule "Test.Namespace" mergedTypes mergedFunctions |> ignore)

    [<Test>]
    let ``merge: surface snapshot`` () =
        let rendered =
            [
                for t in mergedTypes do
                    match t with
                    | Record(n, fields, cid) ->
                        let fs = fields |> List.map (fun f -> f.Name) |> String.concat ", "
                        yield $"record %s{n}#%08x{cid}(%s{fs})"
                    | Union(n, cases) ->
                        let cs =
                            cases
                            |> List.map (fun c -> $"%s{c.Name}#%08x{c.ConstructorId}")
                            |> String.concat " | "
                        yield $"union %s{n} = %s{cs}"
                for f in mergedFunctions do
                    yield $"function %s{f.Name}#%08x{f.ConstructorId} -> %s{f.ReturnType}"
                for s in skipped do
                    yield $"skipped %s{s.Declaration} -> %s{s.CSharpName} (owned by %s{s.Owner})"
            ]
            |> String.concat "\n"
        assertMatchesSnapshot rendered "CSharpMerge_Surface"

    [<Test>]
    let ``merge: real tdlib schemas emit BindAuthKeyInner and stay collision-free`` () =
        let api = parse "tdlib_telegram_api.tl"
        let mt = parse "tdlib_mtproto.tl"
        let types, functions, _ = SchemaMapper.mergeMtprotoForCSharp api mt
        let names = types |> List.map (function Record(n, _, _) -> n | Union(n, _) -> n)
        Assert.That(names, Does.Contain "BindAuthKeyInner")
        Assert.DoesNotThrow(fun () ->
            EmitCSharp.buildModule "Test.Namespace" types functions |> ignore)
