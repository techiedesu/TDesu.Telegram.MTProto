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
  --mtproto-schema <path>     Required only by `cid` target (mtproto-level schema)
  --layer-base-schema <path>  Required only by `layer-aliases` target
  --tests-namespace <ns>      Override namespace for `tests` (defaults to <namespace>.Tests)
  --client-namespace <ns>     Override namespace for client-cids/client-parsers
                              (defaults to <namespace>.Client.Api)
  --split-by-domain           For `types` target: emit one F# file per TL domain
                              under <output>/Requests/, plus a Requests.targets
                              MSBuild manifest, instead of a single
                              GeneratedTlRequests.g.fs.
  --split-domains <list>      Override the default domain prefix list when
                              --split-by-domain is set (comma-separated, e.g.
                              "Account,Auth,Channels"; default lists all known
                              TL domains).
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
        let domainsOverride = argv |> tryGetArg "--split-domains"
        let noWhitelist = argv |> Array.exists (fun s -> s = "--no-whitelist")
        let splitByClass = argv |> Array.exists (fun s -> s = "--split-by-class")
        let clean = argv |> Array.exists (fun s -> s = "--clean")

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

                    let path name = Path.Combine(outputDir, name)
                    let resolvedTestsNs = defaultArg testsNs $"{ns}.Tests.GeneratedRoundTripTests"
                    let resolvedClientNs = defaultArg clientNs $"{ns}.Client.Api"

                    let mutable failed = false

                    if targets.Contains "cid" then
                        match mtprotoSchemaPath with
                        | None ->
                            log.LogError("`cid` target requires --mtproto-schema")
                            failed <- true
                        | Some mtPath ->
                            match parseSchema log "MTProto" mtPath with
                            | None -> failed <- true
                            | Some mtSchema ->
                                let code = EmitTemplates.generateCidModule ns config mtSchema apiSchema
                                let outPath = path "GeneratedCid.g.fs"
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
                            Pipeline.generateSerializationTypesSplit ns config apiSchema outputDir domains
                        else
                            Pipeline.generateSerializationTypes ns config apiSchema (path "GeneratedTlRequests.g.fs")

                    if targets.Contains "writers" then
                        Pipeline.generateWriterModule ns config apiSchema (path "GeneratedTlWriters.g.fs")

                    if targets.Contains "coverage" then
                        EmitTemplates.generateCoverageValidator ns config apiSchema (path "GeneratedCoverageValidator.g.fs")

                    if targets.Contains "return-types" then
                        EmitTemplates.generateReturnTypeMap ns config apiSchema (path "GeneratedReturnTypes.g.fs")

                    if targets.Contains "tests" then
                        EmitTemplates.generateRoundTripTests resolvedTestsNs ns config apiSchema (path "GeneratedRoundTripTests.g.fs")

                    if targets.Contains "layer-aliases" then
                        match layerBasePath with
                        | None ->
                            log.LogError("`layer-aliases` target requires --layer-base-schema")
                            failed <- true
                        | Some basePath ->
                            match parseSchema log "layer-base" basePath with
                            | None -> failed <- true
                            | Some baseSchema ->
                                EmitTemplates.generateLayerAliases ns baseSchema apiSchema (path "GeneratedLayerAliases.g.fs")

                    if targets.Contains "client-cids" then
                        EmitTemplates.generateClientCids resolvedClientNs apiSchema (path "GeneratedClientCid.g.fs")

                    if targets.Contains "client-parsers" then
                        Pipeline.generateClientParsers resolvedClientNs config apiSchema (path "GeneratedResponseParsers.g.fs")

                    if targets.Contains "csharp" then
                        // Single-layer C# backend: full schema surface, no whitelist.
                        let csTypes, csFuncs = SchemaMapper.mapSchema apiSchema
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
                                "tests"; "layer-aliases"; "client-cids"; "client-parsers"; "csharp"
                            ]
                        targets |> Set.filter (fun t -> not (known.Contains t))
                    if not unknown.IsEmpty then
                        log.LogError("Unknown target(s): {Unknown}", String.concat "," unknown)
                        failed <- true

                    if failed then 1
                    else
                        log.LogInformation("Code generation complete.")
                        0
