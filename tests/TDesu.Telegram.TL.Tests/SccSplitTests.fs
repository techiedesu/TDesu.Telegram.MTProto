namespace TDesu.Telegram.TL.Tests

open System.IO
open System.Diagnostics
open NUnit.Framework
open TDesu.Telegram.TL
open TDesu.Telegram.TL.AST
open TDesu.Telegram.TL.Generator
open TDesu.Telegram.TL.Generator.Overrides

/// §2.4 of docs/design/td-tl-gen-improvements.md (SedBot repo): SCC-based
/// file split for the `Base` monolith. `EmitTypes.topoSortSCCs` (Tarjan)
/// predates this section — it already grouped mutually recursive types
/// into `and`-chains for a SINGLE file (0.2.7). What's new here is
/// `EmitTypes.binPackSccs` / `buildSccShardedModule`: splitting an
/// oversized domain's type set across SEVERAL files without ever breaking
/// a strongly-connected component across two of them, and without
/// reordering — `namespace rec` does NOT let separate files
/// forward-reference each other (verified empirically; see the design
/// doc), so shard order is a correctness requirement, not cosmetics.
[<TestFixture>]
module SccSplitTests =

    let private nameOf =
        function
        | Record(n, _, _) -> n
        | Union(n, _) -> n

    let private mkField (name: string) (fsharpType: string) : GeneratedField =
        { Name = name
          RecordName = name
          FSharpType = fsharpType
          IsOptional = false
          FlagField = None
          FlagBit = None
          LayerGate = None }

    let private mkRecord (name: string) (fields: (string * string) list) : GeneratedType =
        Record(name, fields |> List.map (fun (n, t) -> mkField n t), 0u)

    let private mkFunction (name: string) : GeneratedFunction =
        { Name = name
          ConstructorId = 0u
          AliasCids = []
          Params = []
          ReturnType = "int32" }

    // --- Tarjan SCC (EmitTypes.topoSortSCCs) ---

    /// The fixture the assignment specifies: A -> B -> C -> A (cycle), D
    /// independent (no edge to or from the cycle in either direction).
    [<Test>]
    let ``topoSortSCCs groups a mutually recursive cycle into one SCC, independent type stays separate`` () =
        let a = mkRecord "A" [ "b", "B" ]
        let b = mkRecord "B" [ "c", "C" ]
        let c = mkRecord "C" [ "a", "A" ]
        let d = mkRecord "D" [ "x", "int32" ]

        let sccs = EmitTypes.topoSortSCCs [ a; b; c; d ]
        let sccNameSets = sccs |> List.map (fun scc -> scc |> List.map nameOf |> Set.ofList)

        Assert.That(sccNameSets.Length, Is.EqualTo 2, NUnitString.op_Implicit $"expected 2 SCCs, got: %A{sccNameSets}")
        Assert.That(sccNameSets, Does.Contain(set [ "A"; "B"; "C" ]))
        Assert.That(sccNameSets, Does.Contain(set [ "D" ]))

    /// Within the cyclic SCC, every member must carry ALL the others as
    /// `and`-siblings when rendered — this is what makes the cycle
    /// compile at all inside one file (§2.4's premise: it does NOT work
    /// across files).
    [<Test>]
    let ``a mutually recursive SCC renders as one and-chain within a single shard file`` () =
        let a = mkRecord "CycA" [ "b", "CycB" ]
        let b = mkRecord "CycB" [ "a", "CycA" ]

        let shards = EmitTypes.buildSccShardedModule "Test.Ns" [] "Base" 400 1_000_000 [ a; b ] []

        Assert.That(shards.Length, Is.EqualTo 1, NUnitString.op_Implicit "a 2-node cycle must never straddle shards")
        let code = shards[0].Code
        Assert.That(code, Does.Contain "type CycA")
        Assert.That(code, Does.Contain "and CycB")

    /// Topological order of the SCC DAG: `E` references `A` (a member of
    /// the cycle), so the cycle's SCC MUST come before `E`'s SCC in the
    /// result — a later block may reference an earlier one, never the
    /// reverse. `D` stays independent, unconstrained relative to either.
    [<Test>]
    let ``topoSortSCCs orders a dependency's SCC before its dependent's SCC`` () =
        let a = mkRecord "A" [ "b", "B" ]
        let b = mkRecord "B" [ "c", "C" ]
        let c = mkRecord "C" [ "a", "A" ]
        let d = mkRecord "D" [ "x", "int32" ]
        let e = mkRecord "E" [ "a", "A" ]

        let sccs = EmitTypes.topoSortSCCs [ a; b; c; d; e ]
        let indexOfSccContaining name =
            sccs |> List.findIndex (fun scc -> scc |> List.exists (fun t -> nameOf t = name))

        let cycleIndex = indexOfSccContaining "A"
        Assert.That(cycleIndex, Is.EqualTo(indexOfSccContaining "B"), NUnitString.op_Implicit "A and B must be the same SCC")
        Assert.That(cycleIndex, Is.EqualTo(indexOfSccContaining "C"), NUnitString.op_Implicit "A and C must be the same SCC")
        Assert.That(cycleIndex, Is.LessThan(indexOfSccContaining "E"),
            NUnitString.op_Implicit "the cycle's SCC must be declared before E, which references a cycle member")

    // --- Bin-packing (EmitTypes.binPackSccs) ---

    /// The core §2.4 invariant: an SCC is one indivisible unit. A 3-member
    /// cycle must land whole in one shard even when the per-shard type
    /// budget (1) is smaller than the cycle itself.
    [<Test>]
    let ``binPackSccs never splits one SCC across two shards even at a 1-type-per-shard budget`` () =
        let a = mkRecord "A" [ "b", "B" ]
        let b = mkRecord "B" [ "c", "C" ]
        let c = mkRecord "C" [ "a", "A" ]
        let d = mkRecord "D" [ "x", "int32" ]

        let sccs = EmitTypes.topoSortSCCs [ a; b; c; d ]
        let shards = EmitTypes.binPackSccs 1 System.Int32.MaxValue sccs
        let shardContaining name =
            shards |> List.findIndex (fun shard -> shard |> List.exists (List.exists (fun t -> nameOf t = name)))

        Assert.That(shardContaining "A", Is.EqualTo(shardContaining "B"), NUnitString.op_Implicit "the whole cycle must share a shard")
        Assert.That(shardContaining "A", Is.EqualTo(shardContaining "C"), NUnitString.op_Implicit "the whole cycle must share a shard")
        Assert.That(shardContaining "D", Is.Not.EqualTo(shardContaining "A"),
            NUnitString.op_Implicit "an unrelated independent type must not be forced into the cycle's shard")

    /// Same invariant, driven by the BYTE budget instead of the type-count
    /// budget — a generous type budget (10, never trips) alongside a
    /// starved byte budget (1) must still force one SCC per shard, not
    /// split any of them.
    [<Test>]
    let ``binPackSccs splits on the byte budget even when the type-count budget alone would not trip`` () =
        let a = mkRecord "A" [ "x", "int32" ]
        let b = mkRecord "B" [ "y", "int32" ]
        let c = mkRecord "C" [ "z", "int32" ]
        let sccs = [ [ a ]; [ b ]; [ c ] ]

        let shards = EmitTypes.binPackSccs 10 1 sccs

        Assert.That(shards.Length, Is.EqualTo 3, NUnitString.op_Implicit $"expected one shard per SCC, got: %A{shards |> List.map (List.map (List.map nameOf))}")

    /// `binPackSccs` chunks sequentially — it must reproduce the exact
    /// input SCC sequence when shards are flattened back, never reorder,
    /// drop, or duplicate an SCC.
    [<Test>]
    let ``binPackSccs preserves the topological order of the input SCC list`` () =
        let sccGroups =
            [ [ mkRecord "A" [] ]
              [ mkRecord "B" []; mkRecord "C" [] ] // stand-in for a 2-member SCC
              [ mkRecord "D" [] ]
              [ mkRecord "E" [] ] ]

        let shards = EmitTypes.binPackSccs 1 System.Int32.MaxValue sccGroups
        let flattenedSccs = shards |> List.collect id

        Assert.That((flattenedSccs = sccGroups), NUnitString.op_Implicit "shards must reproduce the exact input SCC order, unchanged")

    /// Shard file naming: zero-padded index, ascending, matching
    /// topological (compile) order.
    [<Test>]
    let ``buildSccShardedModule names shards Base.NN.g.fs in ascending topological order`` () =
        let types = [ for i in 0 .. 5 -> mkRecord $"T{i}" [ "v", "int32" ] ]

        let shards = EmitTypes.buildSccShardedModule "Test.Ns" [] "Base" 2 System.Int32.MaxValue types []

        Assert.That(shards.Length, Is.EqualTo 3, NUnitString.op_Implicit "6 independent types at 2/shard must yield 3 shards")
        Assert.That(
            (shards |> List.map (fun s -> s.Filename)) = [ "Base.00.g.fs"; "Base.01.g.fs"; "Base.02.g.fs" ],
            NUnitString.op_Implicit $"""got filenames: %A{shards |> List.map (fun s -> s.Filename)}""")
        Assert.That((shards |> List.map (fun s -> s.Index)) = [ 0; 1; 2 ], NUnitString.op_Implicit "shard Index must be ascending from 0")
        for s in shards do
            Assert.That(s.Code, Does.Contain "namespace Test.Ns")

    /// Regression: functions never participate in SCC ordering, but a
    /// sharded domain must not silently drop them — every function has to
    /// land SOMEWHERE, and only the LAST shard is guaranteed to have every
    /// type it could reference already in scope.
    [<Test>]
    let ``buildSccShardedModule appends functions only to the last shard`` () =
        let types = [ for i in 0 .. 3 -> mkRecord $"T{i}" [ "v", "int32" ] ]
        let fn = mkFunction "DoSomething"

        let shards = EmitTypes.buildSccShardedModule "Test.Ns" [] "Base" 2 System.Int32.MaxValue types [ fn ]

        Assert.That(shards.Length, Is.EqualTo 2, NUnitString.op_Implicit "4 independent types at 2/shard must yield 2 shards")
        Assert.That(shards[0].Code, Does.Not.Contain "DoSomething",
            NUnitString.op_Implicit "a non-last shard must not carry any function")
        Assert.That(shards[1].Code, Does.Contain "DoSomething",
            NUnitString.op_Implicit "the last shard must carry every function")

    /// Edge case: a domain with functions but no (or all-promoted-away)
    /// types still needs exactly one shard to hold them, not zero.
    [<Test>]
    let ``buildSccShardedModule emits exactly one shard for an all-functions, no-types domain`` () =
        let fn = mkFunction "DoSomething"

        let shards = EmitTypes.buildSccShardedModule "Test.Ns" [] "Base" 400 1_000_000 [] [ fn ]

        Assert.That(shards.Length, Is.EqualTo 1)
        Assert.That(shards[0].Code, Does.Contain "DoSomething")

    // --- Integration: the generated multi-shard tree actually compiles ---

    /// Runtime buffer stub matching the contract `EmitTypes`/`EmitCSharp`
    /// document (`TDesu.Serialization.TlWriteBuffer` / `TlReadBuffer`):
    /// every Write*/Read* member a generated `Serialize`/`Deserialize`
    /// can reference, including `WriteVector`/`ReadVector` for boxed
    /// vector fields. Adapted from `samples/PingPongBot/TlBuffer.fs`
    /// (already proven, elsewhere in this repo's own build, to compile
    /// against real generated types) plus the two vector members that
    /// sample never needed.
    let private runtimeStubSource =
        """namespace TDesu.Serialization

open System
open System.Buffers.Binary

type TlWriteBuffer() =
    let mutable buf = Array.zeroCreate<byte> 256
    let mutable pos = 0

    let ensureCapacity n =
        if pos + n > buf.Length then
            let newBuf = Array.zeroCreate (max (buf.Length * 2) (pos + n))
            Buffer.BlockCopy(buf, 0, newBuf, 0, pos)
            buf <- newBuf

    member _.WriteConstructorId(cid: uint32) =
        ensureCapacity 4
        BinaryPrimitives.WriteUInt32LittleEndian(buf.AsSpan(pos), cid)
        pos <- pos + 4

    member _.WriteInt32(v: int32) =
        ensureCapacity 4
        BinaryPrimitives.WriteInt32LittleEndian(buf.AsSpan(pos), v)
        pos <- pos + 4

    member _.WriteInt64(v: int64) =
        ensureCapacity 8
        BinaryPrimitives.WriteInt64LittleEndian(buf.AsSpan(pos), v)
        pos <- pos + 8

    member _.WriteDouble(v: double) =
        ensureCapacity 8
        BinaryPrimitives.WriteInt64LittleEndian(buf.AsSpan(pos), BitConverter.DoubleToInt64Bits v)
        pos <- pos + 8

    member _.WriteBool(v: bool) =
        ensureCapacity 4
        let cid = if v then 0x997275B5u else 0xBC799737u
        BinaryPrimitives.WriteUInt32LittleEndian(buf.AsSpan(pos), cid)
        pos <- pos + 4

    member _.WriteBytes(v: byte array) =
        let len = v.Length
        if len < 254 then
            ensureCapacity (1 + len + (4 - (1 + len) % 4) % 4)
            buf[pos] <- byte len
            pos <- pos + 1
        else
            ensureCapacity (4 + len + (4 - len % 4) % 4)
            buf[pos] <- 254uy
            buf[pos + 1] <- byte (len &&& 0xFF)
            buf[pos + 2] <- byte ((len >>> 8) &&& 0xFF)
            buf[pos + 3] <- byte ((len >>> 16) &&& 0xFF)
            pos <- pos + 4
        Buffer.BlockCopy(v, 0, buf, pos, len)
        pos <- pos + len
        let padding = (4 - pos % 4) % 4
        for _ in 0 .. padding - 1 do
            buf[pos] <- 0uy
            pos <- pos + 1

    member this.WriteString(v: string) = this.WriteBytes(Text.Encoding.UTF8.GetBytes v)

    member _.WriteRawBytes(v: byte array) =
        ensureCapacity v.Length
        Buffer.BlockCopy(v, 0, buf, pos, v.Length)
        pos <- pos + v.Length

    member this.WriteVector<'T>(items: 'T[], writeItem: TlWriteBuffer -> 'T -> unit) =
        this.WriteConstructorId(0x1CB5C415u)
        this.WriteInt32(items.Length)
        for item in items do
            writeItem this item

    member _.ToArray() = buf[.. pos - 1]

    interface IDisposable with
        member _.Dispose() = ()

type TlReadBuffer(data: byte array) =
    let mutable pos = 0
    let span () = ReadOnlySpan(data, pos, data.Length - pos)

    member _.ReadConstructorId() : uint32 =
        let v = BinaryPrimitives.ReadUInt32LittleEndian(span ())
        pos <- pos + 4
        v

    member _.ReadInt32() : int32 =
        let v = BinaryPrimitives.ReadInt32LittleEndian(span ())
        pos <- pos + 4
        v

    member _.ReadInt64() : int64 =
        let v = BinaryPrimitives.ReadInt64LittleEndian(span ())
        pos <- pos + 8
        v

    member _.ReadDouble() : double =
        let v = BinaryPrimitives.ReadInt64LittleEndian(span ())
        pos <- pos + 8
        BitConverter.Int64BitsToDouble v

    member _.ReadRawBytes(count: int) : byte array =
        let result = data[pos .. pos + count - 1]
        pos <- pos + count
        result

    member _.ReadBool() : bool =
        let cid = BinaryPrimitives.ReadUInt32LittleEndian(span ())
        pos <- pos + 4
        cid = 0x997275B5u

    member _.ReadString() : string =
        let firstByte = int data[pos]
        let len, headerSize =
            if firstByte < 254 then
                firstByte, 1
            else
                let l = int data[pos + 1] ||| (int data[pos + 2] <<< 8) ||| (int data[pos + 3] <<< 16)
                l, 4
        pos <- pos + headerSize
        let s = Text.Encoding.UTF8.GetString(data, pos, len)
        pos <- pos + len
        let padding = (4 - pos % 4) % 4
        pos <- pos + padding
        s

    member _.ReadBytes() : byte array =
        let firstByte = int data[pos]
        let len, headerSize =
            if firstByte < 254 then
                firstByte, 1
            else
                let l = int data[pos + 1] ||| (int data[pos + 2] <<< 8) ||| (int data[pos + 3] <<< 16)
                l, 4
        pos <- pos + headerSize
        let result = data[pos .. pos + len - 1]
        pos <- pos + len
        let padding = (4 - pos % 4) % 4
        pos <- pos + padding
        result

    member this.ReadVector<'T>(readItem: TlReadBuffer -> 'T) : 'T[] =
        this.ReadConstructorId() |> ignore
        let count = this.ReadInt32()
        Array.init count (fun _ -> readItem this)

    interface IDisposable with
        member _.Dispose() = ()
"""

    let private nugetConfigSource =
        """<?xml version="1.0" encoding="utf-8"?>
<configuration>
  <packageSources>
    <clear />
    <add key="nuget.org" value="https://api.nuget.org/v3/index.json" />
  </packageSources>
</configuration>
"""

    let private fsprojSource (fsharpCoreVersion: string) =
        $"""<Project Sdk="Microsoft.NET.Sdk">
    <PropertyGroup>
        <TargetFramework>net10.0</TargetFramework>
        <OutputType>Library</OutputType>
        <TreatWarningsAsErrors>false</TreatWarningsAsErrors>
        <DisableImplicitFSharpCoreReference>true</DisableImplicitFSharpCoreReference>
    </PropertyGroup>
    <ItemGroup>
        <PackageReference Include="FSharp.Core" Version="{fsharpCoreVersion}" />
    </ItemGroup>
    <ItemGroup>
        <Compile Include="TlBuffer.fs" />
    </ItemGroup>
    <Import Project="Generated\Requests\Requests.targets" />
</Project>
"""

    /// `dotnet build` a throwaway project referencing every shard
    /// `Requests.targets` lists, in the order it lists them — the only way
    /// to actually catch a topological-order mistake (§2.4): a shard
    /// forward-referencing a later one is a real FS0039 here, not just a
    /// text diff a snapshot test would miss.
    ///
    /// `generatedDir` MUST be a directory literally named "Generated" —
    /// `Requests.targets`'s `<Compile Include>` paths are written
    /// `Generated\Requests\<file>`, which the F# compile task resolves
    /// relative to the IMPORTING project's directory (this repo's
    /// consumers, e.g. SedBot's `MTProto.Schema.fsproj`, rely on the same
    /// convention). Its `<Import Project="<Domain>.targets">` for a
    /// shard-split domain is a bare filename instead — MSBuild resolves
    /// `<Import>` relative to the file that WRITES it, not the entry
    /// project, so a `Generated\Requests\`-prefixed import would
    /// double-nest. See `Pipeline.generateSerializationTypesSplit`'s doc
    /// comment for the full reasoning; this test is what caught it.
    let private compileGeneratedTree (generatedDir: string) : int * string =
        let projDir = Path.GetDirectoryName(generatedDir: string)
        File.WriteAllText(Path.Combine(projDir, "TlBuffer.fs"), runtimeStubSource)
        File.WriteAllText(Path.Combine(projDir, "nuget.config"), nugetConfigSource)
        // Pin FSharp.Core to whatever version is actually loaded in this
        // test run — guarantees a local-cache hit (no network) instead of
        // the throwaway project's implicit-reference resolution picking a
        // different version than what every other project here restored.
        let fsharpCoreVersion = typeof<unit>.Assembly.GetName().Version.ToString()
        let fsprojPath = Path.Combine(projDir, "IntegrationCheck.fsproj")
        File.WriteAllText(fsprojPath, fsprojSource fsharpCoreVersion)

        let psi =
            ProcessStartInfo(
                "dotnet",
                $"build \"{fsprojPath}\" -c Release --nologo -v minimal",
                WorkingDirectory = projDir,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false
            )

        use proc = Process.Start(psi)
        let stdout = proc.StandardOutput.ReadToEnd()
        let stderr = proc.StandardError.ReadToEnd()
        proc.WaitForExit()
        proc.ExitCode, stdout + stderr

    /// "Message" pulls in a genuine real-schema mutually recursive cluster
    /// (`Message` <-> `MessageMedia` <-> `Peer` <-> `WebPage` <-> … — the
    /// exact family §2.4 cites as its example) from the real tdlib api.tl
    /// fixture — this is the correction §2.4 documents, exercised against
    /// real data, not a synthetic stand-in. The shard budget is set
    /// deliberately tiny so the test forces several shards deterministically,
    /// regardless of exactly how large "Message"'s closure happens to be on
    /// any given schema snapshot (shard-SIZE tuning against the real ~3.3 MB
    /// `Base` bucket is what the unit tests above and the design doc's
    /// calibration note already cover).
    [<Test>]
    let ``SCC-split Base output for a real cyclic api.tl closure actually compiles`` () =
        let apiSchema =
            match readTestData "tdlib_telegram_api.tl" |> Downloader.preprocess |> AstFactory.parse with
            | Ok s -> s
            | Error e -> failwith e

        let config = { OverrideConfig.empty with TypeWhitelist = set [ "Message" ] }

        let tempRoot = Path.Combine(Path.GetTempPath(), $"td_tl_gen_scc_it_{System.Guid.NewGuid():N}")
        let generatedDir = Path.Combine(tempRoot, "Generated")
        Directory.CreateDirectory(generatedDir) |> ignore

        try
            let sccSplit = Some { Pipeline.defaultSccSplitConfig with MaxTypesPerShard = 5; MaxBytesPerShard = 50_000 }
            Pipeline.generateSerializationTypesSplit
                "TDesu.Serialization" config apiSchema generatedDir EmitTypes.defaultRequestDomains sccSplit

            let requestsDir = Path.Combine(generatedDir, "Requests")
            let shardFiles = Directory.GetFiles(requestsDir, "Base.*.g.fs")
            Assert.That(shardFiles.Length, Is.GreaterThan 1,
                NUnitString.op_Implicit "fixture should force multiple Base shards — this test proves nothing about compile order otherwise")

            let exitCode, output = compileGeneratedTree generatedDir
            if exitCode <> 0 then
                Assert.Fail($"generated {shardFiles.Length}-shard tree failed to compile (dotnet build exit {exitCode}):\n{output}")
        finally
            if Directory.Exists tempRoot then
                Directory.Delete(tempRoot, true)
