namespace TDesu.Telegram.TL.Generator

open System.IO
open Microsoft.Extensions.Logging
open TDesu.FSharp
open TDesu.FSharp.Operators
open TDesu.FSharp.Utilities
open TDesu.Telegram.TL
open TDesu.Telegram.TL.AST
open TDesu.Telegram.TL.Generator.Overrides

module Program =

    let private usage = """
td-tl-gen — F# code generator for Telegram TL schemas

Usage:
  td-tl-gen --schema <path> --output <dir> --namespace <ns> --overrides <toml> --target <names>

Required flags:
  --schema <path>             Path to .tl schema file (e.g. cached/api.tl)
  --output <dir>              Directory where generated .g.fs files are written
  --namespace <ns>            F# namespace for emitted code (e.g. MyApp.Serialization)
  --overrides <toml>          Path to TOML override config (no embedded default in 0.1.0+)
  --target <names>            Comma-separated list of targets to generate

Available targets:
  cid                Constructor ID literals (GeneratedCid module)
  types              Whitelist-filtered request types with Serialize/Deserialize
  writers            Standalone write{X} functions and Write* DUs
  coverage           Handler coverage validator (GeneratedCoverageValidator)
  return-types       CID → return type lookup (GeneratedReturnTypes)
  tests              Round-trip tests for whitelisted request types
  layer-aliases      L_old ↔ L_new function CID aliases (requires --layer-base-schema)
  client-cids        Flat literal table of all function/constructor CIDs
  client-parsers     Response parsers for client.parsers whitelist
  all                Equivalent to: cid,types,writers,coverage,return-types

Optional flags:
  --mtproto-schema <path>     MTProto-level schema (e.g. schema/mtproto.tl).
                              Required by `cid`; optional for `csharp`, where it
                              merges the mtproto combinators
                              (bind_auth_key_inner, pong, msgs_ack, …) into the
                              emitted C# surface. api.tl wins every name
                              collision — the mtproto declaration is skipped and
                              logged. Omit it and `csharp` emits api.tl only.
  --layer-base-schema <path>  Required only by `layer-aliases` target
  --tests-namespace <ns>      Override namespace for `tests` (defaults to <namespace>.Tests)
  --client-namespace <ns>     Override namespace for client-cids/client-parsers
                              (defaults to <namespace>.Client.Api)
  --split-by-domain           For `types` target: emit one F# file per TL domain
                              under <output>/Requests/, plus a Requests.targets
                              per-domain <Domain>.g.fs files under
                              <output>/Requests/ plus a Requests.targets
                              MSBuild manifest, instead of a single
                              GeneratedTlRequests.g.fs.
  --split-domains <list>      Override the default domain prefix list when
                              --split-by-domain is set (comma-separated, e.g.
                              "Account,Auth,Channels"; default lists all known
                              TL domains).
  --split-by-scc              For `types` target with --split-by-domain: further
                              split the "Base" domain bucket into
                              Base.NN.g.fs shards bounded by ~400 types /
                              ~1 MB each, bin-packed by Tarjan SCC so a
                              mutually-recursive cluster never straddles two
                              files (§2.4). Plus a Base.targets sub-manifest,
                              imported from Requests.targets in place of a
                              single Base.g.fs entry. Opt-in; requires
                              --split-by-domain. Default off.
  --no-whitelist              Ignore the type/writer/client-parser whitelists and
                              emit the FULL schema for whichever of the
                              `types`/`writers`/`tests`/`client-parsers` targets
                              run in this invocation. CIDs are unchanged (derived
                              from the TL), so wire compatibility is preserved;
                              `stub_types`, aliases, layer-variants and
                              structural-overlays from the overrides still apply.
  --split-by-class            For `csharp` target: emit one .g.cs file per
                              top-level TL declaration (Record → its own file,
                              Union + all case classes → one file named after the
                              base type) instead of the single GeneratedTl.g.cs.
  --clean                     Delete all *.g.cs files in the output directory
                              before writing. Recommended with --split-by-class
                              to remove files for types deleted from the schema.

Sample overrides config: samples/SedBotOverrides/sedbot-overrides.toml
"""

    let private tryGetArg (name: string) (argv: string[]) =
        argv
        |> Array.tryFindIndex (fun s -> s = name)
        |> Option.bind (fun i -> if i + 1 < argv.Length then Some argv[i + 1] else None)

    let private fail (log: ILogger) (msg: string) =
        log.LogError("{Message}", msg)
        eprintfn "%s" usage
        1

    /// Every flag the generator understands, and whether it takes a value.
    /// `tryGetArg` answers "is this flag present" and nothing else, so before
    /// this table a mistyped flag was a silent nil: `--mtproto-schemaa`
    /// deleted the 37 transport types the `cid` target needs and exited 0
    /// (#65's family). Anything not named here is now a hard error.
    let private knownFlags : Map<string, bool> =
        Map.ofList [
            "--schema", true
            "--mtproto-schema", true
            "--layer-base-schema", true
            "--output", true
            "--namespace", true
            "--overrides", true
            "--target", true
            "--tests-namespace", true
            "--client-namespace", true
            "--split-domains", true
            "--split-by-domain", false
            "--split-by-scc", false
            "--no-whitelist", false
            "--split-by-class", false
            "--clean", false
        ]

    /// Reject an argument the generator does not recognise, and any bare word
    /// that is not the value of a flag. Returns the list of complaints; empty
    /// means the command line is well formed.
    let internal unknownArguments (argv: string[]) : string list =
        let complaints = ResizeArray<string>()
        let mutable i = 0
        while i < argv.Length do
            let a = argv[i]
            match knownFlags.TryFind a with
            | Some takesValue ->
                if takesValue then
                    if i + 1 >= argv.Length then
                        complaints.Add $"%s{a} requires a value"
                        i <- i + 1
                    else
                        i <- i + 2
                else
                    i <- i + 1
            | None ->
                // Name the nearest known flag: the whole point of this check is
                // the one-character typo, and "did you mean" is the difference
                // between a five-second fix and an afternoon.
                let suggestion =
                    knownFlags.Keys
                    |> Seq.filter (fun k ->
                        k.StartsWith(a, System.StringComparison.Ordinal)
                        || a.StartsWith(k, System.StringComparison.Ordinal))
                    |> Seq.sortBy String.length
                    |> Seq.tryHead
                if a.StartsWith("--", System.StringComparison.Ordinal) then
                    match suggestion with
                    | Some s -> complaints.Add $"unknown flag %s{a} (did you mean %s{s}?)"
                    | None -> complaints.Add $"unknown flag %s{a}"
                else
                    complaints.Add $"unexpected argument '%s{a}' — it is not a flag and no flag takes it as a value"
                i <- i + 1
        List.ofSeq complaints

    /// What reads a given input.
    type internal ReadBy =
        /// Every target folds it in (`[[extra_combinators]]` joins the schema
        /// before any target runs), so it is never discarded.
        | AllTargets
        /// Only these targets read it.
        | Targets of string list
        /// Nothing in the generator reads it. Parsed, validated, dropped.
        | NoTarget

    /// Inputs the command line supplied that NONE of the selected targets
    /// reads. Accepting one and then discarding it in silence is the same
    /// failure mode as the mistyped flag — the operator believes something
    /// took effect.
    let internal ignoredInputs (argv: string[]) (config: OverrideConfig) (targets: Set<string>) : (string * ReadBy) list =
        let flagGiven name = (argv |> tryGetArg name).IsSome
        let switchGiven name = argv |> Array.exists (fun s -> s = name)

        let flags =
            [ "--mtproto-schema", flagGiven "--mtproto-schema", Targets [ "cid"; "csharp" ]
              "--layer-base-schema", flagGiven "--layer-base-schema", Targets [ "layer-aliases" ]
              "--tests-namespace", flagGiven "--tests-namespace", Targets [ "tests" ]
              "--client-namespace", flagGiven "--client-namespace", Targets [ "client-cids"; "client-parsers" ]
              "--split-by-domain", switchGiven "--split-by-domain", Targets [ "types" ]
              "--split-by-scc", switchGiven "--split-by-scc", Targets [ "types" ]
              "--split-domains", flagGiven "--split-domains", Targets [ "types" ]
              "--no-whitelist", switchGiven "--no-whitelist", Targets [ "types"; "writers"; "tests"; "client-parsers" ]
              "--split-by-class", switchGiven "--split-by-class", Targets [ "csharp" ]
              "--clean", switchGiven "--clean", Targets [ "csharp" ] ]

        // The overrides file is one `--overrides` argument carrying twelve
        // independent channels. A target set that reads none of a populated
        // channel discards it just as silently as a mistyped flag does.
        let sections =
            [ "[[layer_variants]]", not config.LayerVariants.IsEmpty,
              Targets [ "cid"; "types"; "tests"; "client-parsers"; "writers" ]
              "[[structural_overlays]]", not config.StructuralOverlays.IsEmpty, Targets [ "writers" ]
              "[[aliases]]", not config.Aliases.IsEmpty,
              Targets [ "cid"; "types"; "tests"; "client-parsers"; "coverage"; "return-types" ]
              "[[extras]]", not config.Extras.IsEmpty, Targets [ "cid" ]
              "[[extra_combinators]]", not config.ExtraCombinators.IsEmpty, AllTargets
              "[layer_type_info]", not config.LayerTypeInfo.IsEmpty, NoTarget
              "[whitelists].types", not config.TypeWhitelist.IsEmpty, Targets [ "types"; "tests"; "client-parsers" ]
              "[whitelists].writers", not config.WriterWhitelist.IsEmpty, Targets [ "writers" ]
              "[whitelists].writer_layer_types", not config.WriterLayerTypes.IsEmpty, Targets [ "writers" ]
              "[whitelists].stub_types", not config.StubTypes.IsEmpty, Targets [ "types"; "tests"; "client-parsers" ]
              "[whitelists].client_parsers", not config.ClientParserWhitelist.IsEmpty, Targets [ "client-parsers" ]
              "[whitelists].writer_record_per_case_unions", not config.WriterRecordPerCaseUnions.IsEmpty,
              Targets [ "writers" ] ]

        [ for name, given, readBy in flags @ sections do
              if given then
                  match readBy with
                  | AllTargets -> ()
                  | NoTarget -> yield name, NoTarget
                  | Targets consumers ->
                      if not (consumers |> List.exists targets.Contains) then
                          yield name, Targets consumers ]

    let private parseTargets (raw: string) : Set<string> =
        let normalised =
            raw.Split([| ','; ' ' |], System.StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (fun s -> s.Trim().ToLowerInvariant())
            |> Set.ofArray
        if normalised.Contains "all" then
            Set.ofList [ "cid"; "types"; "writers"; "coverage"; "return-types" ]
            |> Set.union (normalised |> Set.remove "all")
        else
            normalised

    let private parseSchema (log: ILogger) (kind: string) (path: string) : TlSchema option =
        if not (File.Exists path) then
            log.LogError("{Kind} schema not found at {Path}", kind, path)
            None
        else
            let text = File.ReadAllText(path) |> Downloader.preprocess
            match AstFactory.parse text with
            | Ok schema ->
                log.LogInformation(
                    "{Kind}: {Ctors} constructors, {Funcs} functions",
                    kind, schema.Constructors.Length, schema.Functions.Length)
                Some schema
            | Error err ->
                log.LogError("Failed to parse {Kind} schema: {Error}", kind, err)
                None

    [<EntryPoint>]
    let main argv =
        let log = Logger.get "td-tl-gen"

        let schemaPath = argv |> tryGetArg "--schema"
        let mtprotoSchemaPath = argv |> tryGetArg "--mtproto-schema"
        let layerBasePath = argv |> tryGetArg "--layer-base-schema"
        let outputDir = argv |> tryGetArg "--output"
        let nsOpt = argv |> tryGetArg "--namespace"
        let overridesPath = argv |> tryGetArg "--overrides"
        let targetRaw = argv |> tryGetArg "--target"
        let testsNs = argv |> tryGetArg "--tests-namespace"
        let clientNs = argv |> tryGetArg "--client-namespace"
        let splitByDomain = argv |> Array.exists (fun s -> s = "--split-by-domain")
        let splitByScc = argv |> Array.exists (fun s -> s = "--split-by-scc")
        let domainsOverride = argv |> tryGetArg "--split-domains"
        let noWhitelist = argv |> Array.exists (fun s -> s = "--no-whitelist")
        let splitByClass = argv |> Array.exists (fun s -> s = "--split-by-class")
        let clean = argv |> Array.exists (fun s -> s = "--clean")

        match unknownArguments argv with
        | _ :: _ as complaints ->
            // Before every other check: a mistyped flag means the command line
            // does not say what the operator thinks it says, and running the
            // rest of it produces a confidently wrong tree and exit 0.
            for c in complaints do
                log.LogError("{Complaint}", c)
            eprintfn "%s" usage
            1
        | [] ->

        match schemaPath, outputDir, nsOpt, overridesPath, targetRaw with
        | None, _, _, _, _ -> fail log "--schema is required"
        | _, None, _, _, _ -> fail log "--output is required"
        | _, _, None, _, _ -> fail log "--namespace is required"
        | _, _, _, None, _ -> fail log "--overrides is required"
        | _, _, _, _, None -> fail log "--target is required"
        | Some schemaPath, Some outputDir, Some ns, Some overridesPath, Some targetRaw ->
            let targets = parseTargets targetRaw
            log.LogInformation("Targets: {Targets}", String.concat "," targets)

            if not (File.Exists overridesPath) then
                fail log $"overrides file not found: {overridesPath}"
            else
                log.LogInformation("Loading overrides from {Path}...", overridesPath)
                let config = Config.load overridesPath

                // Accepted, parsed, and then read by nothing that runs. Warned
                // on stderr rather than discarded: the operator supplied it
                // because they expected it to do something.
                for name, readBy in ignoredInputs argv config targets do
                    match readBy with
                    | Targets consumers ->
                        eprintfn
                            "td-tl-gen: warning: %s is ignored — it is read only by the %s target(s), and this run selected %s"
                            name
                            (String.concat ", " consumers)
                            (targets |> Set.toList |> String.concat ", ")
                    | NoTarget ->
                        eprintfn
                            "td-tl-gen: warning: %s is populated but no target in this generator reads it"
                            name
                    | AllTargets -> ()

                match parseSchema log "API" schemaPath with
                | None -> 1
                | Some baseApiSchema ->
                    // Fold `[[extra_combinators]]` from the overrides into the
                    // parsed schema so downstream targets see them uniformly.
                    let apiSchema = SchemaAugment.fold config baseApiSchema

                    // `--no-whitelist`: seed the type/writer/client-parser
                    // whitelists with every name in the (augmented) schema, so
                    // the whitelist-filtered targets emit the full surface. The
                    // BFS resolver still skips `stub_types` (kept opaque), and
                    // aliases / layer-variants / overlays are untouched. CIDs
                    // are schema-derived, so this only widens coverage.
                    let config =
                        if noWhitelist then
                            let allTypes, allFuncs = SchemaMapper.mapSchema apiSchema
                            // `types`/`client-parsers` match Pascal result-type
                            // names; `writers` filter on the TL combinator name
                            // (snake_case) — seed each with the right projection.
                            let typeNames =
                                allTypes
                                |> List.map (function
                                    | Record(n, _, _) -> n
                                    | Union(n, _) -> n)
                                |> Set.ofList
                            let funcNames = allFuncs |> List.map (fun f -> f.Name) |> Set.ofList
                            let ctorTlNames =
                                apiSchema.Constructors |> List.map Combinator.tlName |> Set.ofList
                            log.LogInformation(
                                "--no-whitelist: seeding full schema ({Types} types, {Funcs} functions, {Ctors} writer ctors)",
                                typeNames.Count, funcNames.Count, ctorTlNames.Count)
                            { config with
                                TypeWhitelist = Set.union typeNames funcNames
                                WriterWhitelist = ctorTlNames
                                ClientParserWhitelist = typeNames }
                        else
                            config

                    if not (Directory.Exists outputDir) then
                        Directory.CreateDirectory(outputDir) |> ignore

                    let path name = Managed.outputPath outputDir name
                    let resolvedTestsNs = defaultArg testsNs $"{ns}.Tests.GeneratedRoundTripTests"
                    let resolvedClientNs = defaultArg clientNs $"{ns}.Client.Api"

                    let mutable failed = false

                    // Parsed at most once, and only if `cid` or `csharp` asks
                    // for it — an unused --mtproto-schema must stay silent.
                    let mtprotoSchema = lazy (mtprotoSchemaPath |> Option.bind (parseSchema log "MTProto"))

                    if targets.Contains "cid" then
                        match mtprotoSchemaPath with
                        | None ->
                            log.LogError("`cid` target requires --mtproto-schema")
                            failed <- true
                        | Some _ ->
                            match mtprotoSchema.Value with
                            | None -> failed <- true
                            | Some mtSchema ->
                                let code = EmitTemplates.generateCidModule ns config mtSchema apiSchema
                                let outPath = path "GeneratedCid"
                                File.WriteAllText(outPath, code)
                                log.LogInformation("Wrote {Path} ({Bytes} bytes)", outPath, code.Length)

                    if targets.Contains "types" then
                        if splitByDomain then
                            let domains =
                                match domainsOverride with
                                | Some raw ->
                                    raw.Split([| ','; ' ' |], System.StringSplitOptions.RemoveEmptyEntries)
                                    |> Array.map (fun s -> s.Trim())
                                    |> Array.toList
                                | None -> EmitTypes.defaultRequestDomains
                            let sccSplit = if splitByScc then Some Pipeline.defaultSccSplitConfig else None
                            Pipeline.generateSerializationTypesSplit ns config apiSchema outputDir domains sccSplit
                        elif splitByScc then
                            log.LogError("--split-by-scc requires --split-by-domain (it further splits that mode's Base bucket)")
                            failed <- true
                        else
                            Pipeline.generateSerializationTypes ns config apiSchema (path "GeneratedTlRequests")

                    if targets.Contains "writers" then
                        Pipeline.generateWriterModule ns config apiSchema (path "GeneratedTlWriters")

                    if targets.Contains "coverage" then
                        EmitTemplates.generateCoverageValidator ns config apiSchema (path "GeneratedCoverageValidator")

                    if targets.Contains "return-types" then
                        EmitTemplates.generateReturnTypeMap ns config apiSchema (path "GeneratedReturnTypes")

                    if targets.Contains "tests" then
                        EmitTemplates.generateRoundTripTests resolvedTestsNs ns config apiSchema (path "GeneratedRoundTripTests")

                    if targets.Contains "layer-aliases" then
                        match layerBasePath with
                        | None ->
                            log.LogError("`layer-aliases` target requires --layer-base-schema")
                            failed <- true
                        | Some basePath ->
                            match parseSchema log "layer-base" basePath with
                            | None -> failed <- true
                            | Some baseSchema ->
                                EmitTemplates.generateLayerAliases ns baseSchema apiSchema (path "GeneratedLayerAliases")

                    if targets.Contains "client-cids" then
                        EmitTemplates.generateClientCids resolvedClientNs apiSchema (path "GeneratedClientCid")


                    if targets.Contains "ergonomics" then
                        Pipeline.generateErgonomics ns config apiSchema (path "GeneratedErgonomics")
                    if targets.Contains "client-parsers" then
                        Pipeline.generateClientParsers resolvedClientNs config apiSchema (path "GeneratedResponseParsers")

                    if targets.Contains "csharp" then
                        // Single-layer C# backend: full schema surface, no whitelist.
                        // `--mtproto-schema` is opt-in; without it the emitted set is
                        // exactly api.tl's, byte for byte as before.
                        let csTypes, csFuncs =
                            match mtprotoSchemaPath with
                            | None -> SchemaMapper.mapSchema apiSchema
                            | Some _ ->
                                match mtprotoSchema.Value with
                                | None ->
                                    failed <- true
                                    SchemaMapper.mapSchema apiSchema
                                | Some mtSchema ->
                                    let types, funcs, skipped =
                                        SchemaMapper.mergeMtprotoForCSharp apiSchema mtSchema
                                    for s in skipped do
                                        log.LogInformation(
                                            "csharp: skipping mtproto '{Declaration}' — C# name '{CsName}' is already declared by {Owner}",
                                            s.Declaration, s.CSharpName, s.Owner)
                                    log.LogInformation(
                                        "csharp: merged MTProto schema (api.tl wins; {Skipped} declaration(s) skipped)",
                                        skipped.Length)
                                    types, funcs
                        if clean then
                            let deleted =
                                Directory.GetFiles(outputDir, "*.g.cs")
                                |> Array.filter (fun f -> not (Path.GetFileName f = "GeneratedTl.g.cs") || splitByClass)
                            for f in deleted do File.Delete f
                            log.LogInformation("Cleaned {N} .g.cs file(s) from {Dir}", deleted.Length, outputDir)
                        if splitByClass then
                            let files = EmitCSharp.buildFiles ns csTypes csFuncs
                            for (name, code) in files do
                                File.WriteAllText(Path.Combine(outputDir, name), code)
                            log.LogInformation(
                                "Wrote {N} .g.cs files to {Dir} ({Types} types, {Funcs} functions)",
                                files.Length, outputDir, csTypes.Length, csFuncs.Length)
                        else
                            let code = EmitCSharp.buildModule ns csTypes csFuncs
                            let outPath = path "GeneratedTl.g.cs"
                            File.WriteAllText(outPath, code)
                            log.LogInformation(
                                "Wrote {Path} ({Bytes} bytes, {Types} types, {Funcs} functions)",
                                outPath, code.Length, csTypes.Length, csFuncs.Length)

                    let unknown =
                        let known =
                            Set.ofList [
                                "cid"; "types"; "writers"; "coverage"; "return-types"
                                "tests"; "layer-aliases"; "client-cids"; "client-parsers"
                                "ergonomics"; "csharp"
                            ]
                        targets |> Set.filter (fun t -> not (known.Contains t))
                    if not unknown.IsEmpty then
                        log.LogError("Unknown target(s): {Unknown}", String.concat "," unknown)
                        failed <- true

                    if failed then 1
                    else
                        log.LogInformation("Code generation complete.")
                        0
