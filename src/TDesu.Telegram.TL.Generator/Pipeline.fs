namespace TDesu.Telegram.TL.Generator

open System.IO
open Microsoft.Extensions.Logging
open TDesu.FSharp
open TDesu.FSharp.Operators
open TDesu.FSharp.IO
open TDesu.FSharp.Utilities
open TDesu.Telegram.TL.AST
open TDesu.Telegram.TL.Generator.Overrides

/// High-level codegen entry points used by the CLI. Each function takes the
/// target F# `namespace` plus an output file path; the underlying emitters
/// are pure functions of (namespace × schema × overrides).
module Pipeline =

    let private log = Logger.get "Pipeline"

    /// Generate whitelist-filtered request/response types for use in handlers.
    /// `runtimeNs` is the namespace where the runtime types live (e.g. `MyApp.Serialization`);
    /// the generated types live in `<runtimeNs>.Requests`.
    let generateSerializationTypes
        (runtimeNs: string)
        (config: OverrideConfig)
        (apiSchema: TlSchema)
        (outputPath: string) =

        let aliasMap = EmitTemplates.buildAliasMap config
        let seeds =
            SchemaMapper.deriveTypeSeeds apiSchema config.TypeWhitelist config.WriterWhitelist config.WriterLayerTypes
        let types, functions =
            SchemaMapper.mapSchemaWhitelisted
                apiSchema
                seeds
                config.StubTypes
                aliasMap

        log.LogInformation(
            "Whitelist resolved: {types} types, {funcs} functions",
            types.Length, functions.Length)

        let allNames =
            (types |> List.map (fun t -> match t with Record(n,_,_) -> n | Union(n,_) -> n))
            @ (functions |> List.map (fun f -> f.Name))
            |> Set.ofList
        let missing = config.TypeWhitelist |> Set.filter (fun n -> not (allNames.Contains n))
        if not missing.IsEmpty then
            log.LogWarning("Whitelist entries not found in schema: {missing}", missing)

        let code =
            EmitTypes.buildModule
                $"{runtimeNs}.Requests"
                "GeneratedTlRequests"
                types
                functions

        let dir = System.IO.Path.GetDirectoryName(outputPath)
        if Directory.notExists dir then Directory.create dir
        File.WriteAllText(outputPath, code)
        log.LogInformation("Wrote {Path} ({Bytes} bytes)", outputPath, code.Length)

    /// Which per-domain bucket `generateSerializationTypesSplit` should
    /// further split into Tarjan-SCC-bounded shards (§2.4 of
    /// docs/design/td-tl-gen-improvements.md, SedBot repo), and the size
    /// targets to bin-pack by. Passing `None` (the default — `--split-by-scc`
    /// not given) leaves every domain, including an oversized `Base`, as
    /// one file: unchanged from pre-§2.4 behaviour.
    type SccSplitConfig = {
        /// The domain bucket to shard. Always "Base" in practice today —
        /// it is the one bucket with no recognized TL-request prefix, so
        /// it is where an unbounded whitelist closure (and the
        /// cross-domain cycle promotion in `EmitTypes.buildPerDomainModules`)
        /// lands.
        Domain: string
        MaxTypesPerShard: int
        MaxBytesPerShard: int
    }

    /// `--split-by-scc`'s default target: shard "Base" using `EmitTypes`'s
    /// tuned size constants.
    let defaultSccSplitConfig : SccSplitConfig =
        { Domain = "Base"
          MaxTypesPerShard = EmitTypes.defaultMaxTypesPerShard
          MaxBytesPerShard = EmitTypes.defaultMaxBytesPerShard }

    /// Same content as `generateSerializationTypes`, but split into per-domain
    /// files under `<outputDir>/Requests/`. Domain detection is by leading
    /// PascalCase prefix (`MessagesSendMessage` → "Messages"); types without
    /// a recognized prefix go into `Base.g.fs`.
    ///
    /// Cycle resolution: any non-Base type referenced from a Base type is
    /// promoted to Base (transitively). On the current schema this only moves
    /// `MessagesEmojiGameOutcome` (referenced by `Base.MessageMedia`).
    ///
    /// `sccSplit`: when set (`--split-by-scc`), the named domain (default
    /// "Base") is further split into `<Domain>.NN.g.fs` shards instead of a
    /// single `<Domain>.g.fs`, via `EmitTypes.buildSccShardedModule` — see
    /// §2.4. `None` reproduces the exact pre-§2.4 output byte for byte.
    ///
    /// Side effects:
    /// * Wipes managed files in `<outputDir>/Requests/` before writing —
    ///   safe-wipe by the `//# td-tl-gen-managed` marker. Hand-added `.fs`
    ///   files without the marker survive.
    /// * Writes `<outputDir>/Requests/Requests.targets` (MSBuild manifest
    ///   with `<Compile Include>` entries in topological compile order).
    ///   `<Compile Include>` paths stay `Generated\Requests\<file>` — the
    ///   F# SDK's compile task resolves a relative `%(Identity)` against
    ///   the ENTRY project's directory regardless of which imported
    ///   `.targets` file declared the item, so this must match what's
    ///   correct from there, however deep the import nesting goes.
    ///   `<Import Project>`, by contrast, is pure MSBuild evaluation and
    ///   resolves against the file that WRITES it — so a shard-split
    ///   domain's `<Import Project="<Domain>.targets">` (at its position,
    ///   instead of a direct `<Compile Include>`) is a BARE filename: a
    ///   `Generated\Requests\`-prefixed one here double-nests, since
    ///   `Requests.targets` already lives in that directory. Both halves
    ///   verified empirically — see the integration test in
    ///   `SccSplitTests.fs`.
    let generateSerializationTypesSplit
        (runtimeNs: string)
        (config: OverrideConfig)
        (apiSchema: TlSchema)
        (outputDir: string)
        (domains: string list)
        (sccSplit: SccSplitConfig option) =

        let aliasMap = EmitTemplates.buildAliasMap config
        let seeds =
            SchemaMapper.deriveTypeSeeds apiSchema config.TypeWhitelist config.WriterWhitelist config.WriterLayerTypes
        let types, functions =
            SchemaMapper.mapSchemaWhitelisted
                apiSchema
                seeds
                config.StubTypes
                aliasMap

        log.LogInformation(
            "Whitelist resolved: {types} types, {funcs} functions",
            types.Length, functions.Length)

        let allNames =
            (types |> List.map (fun t -> match t with Record(n,_,_) -> n | Union(n,_) -> n))
            @ (functions |> List.map (fun f -> f.Name))
            |> Set.ofList
        let missing = config.TypeWhitelist |> Set.filter (fun n -> not (allNames.Contains n))
        if not missing.IsEmpty then
            log.LogWarning("Whitelist entries not found in schema: {missing}", missing)

        let requestsDir = Path.Combine(outputDir, "Requests")
        if Directory.notExists requestsDir then Directory.create requestsDir

        // Safe-wipe: delete files WE wrote (marker present). Hand-added
        // .fs alongside our output survive because they never carry the marker.
        Managed.sweep requestsDir

        let outputs =
            EmitTypes.buildPerDomainModules
                $"{runtimeNs}.Requests"
                []
                domains
                types
                functions

        // §2.4: the domain named by `sccSplit`, if any, is written as shards
        // below instead of a single file — resolve it against the domains
        // this run actually produced before anything is written, so a
        // typo'd/absent domain name warns instead of silently doing nothing
        // (same philosophy as `Program.ignoredInputs`).
        let shardedDomain =
            match sccSplit with
            | None -> None
            | Some cfg ->
                match outputs |> List.tryFind (fun o -> o.Domain = cfg.Domain) with
                | Some o -> Some(cfg, o)
                | None ->
                    log.LogWarning(
                        "--split-by-scc: domain '{Domain}' has no output in this run (schema/whitelist has nothing there) — nothing to shard",
                        cfg.Domain)
                    None

        for o in outputs do
            if shardedDomain |> Option.forall (fun (cfg, _) -> cfg.Domain <> o.Domain) then
                let path = Path.Combine(requestsDir, o.Filename)
                File.WriteAllText(path, o.Code)
                log.LogInformation(
                    "Wrote {Path} ({Bytes} bytes, domain={Domain})",
                    path, o.Code.Length, o.Domain)

        // `(domain name, "<Domain>.targets" filename)` for the sharded
        // domain, if any — used both to write its shards + sub-manifest
        // here and, below, to know which domain gets an <Import> instead
        // of a <Compile Include> in the outer Requests.targets.
        let shardManifestFilename =
            shardedDomain
            |> Option.map (fun (cfg, domainOutput) ->
                let shards =
                    EmitTypes.buildSccShardedModule
                        $"{runtimeNs}.Requests"
                        []
                        cfg.Domain
                        cfg.MaxTypesPerShard
                        cfg.MaxBytesPerShard
                        domainOutput.Types
                        domainOutput.Functions

                for s in shards do
                    let path = Path.Combine(requestsDir, s.Filename)
                    File.WriteAllText(path, s.Code)
                    log.LogInformation(
                        "Wrote {Path} ({Bytes} bytes, shard {Index} of {Domain})",
                        path, s.Code.Length, s.Index, cfg.Domain)

                let manifestFilename = cfg.Domain + ".targets"
                let shardSb = System.Text.StringBuilder()
                shardSb.Append(Managed.xmlBanner "td-tl-gen") |> ignore
                shardSb.Append("<Project>\n") |> ignore
                shardSb.Append("    <ItemGroup>\n") |> ignore
                for s in shards do
                    shardSb.AppendFormat("        <Compile Include=\"Generated\\Requests\\{0}\" />\n", s.Filename) |> ignore
                shardSb.Append("    </ItemGroup>\n") |> ignore
                shardSb.Append("</Project>\n") |> ignore
                let manifestPath = Path.Combine(requestsDir, manifestFilename)
                File.WriteAllText(manifestPath, shardSb.ToString())
                log.LogInformation(
                    "Wrote {Path} ({Shards} shard(s) for domain {Domain})",
                    manifestPath, shards.Length, cfg.Domain)

                cfg.Domain, manifestFilename)

        // Manifest .targets with <Compile Include> entries in the same order
        // as `outputs` (already topologically sorted, Base-first). Unchanged
        // shape when nothing was shard-split — a domain that WAS shard-split
        // contributes an <Import> of its own sub-manifest at its position
        // instead of a direct <Compile Include>.
        let manifestPath = Path.Combine(requestsDir, "Requests.targets")
        let sb = System.Text.StringBuilder()
        sb.Append(Managed.xmlBanner "td-tl-gen") |> ignore
        sb.Append("<Project>\n") |> ignore
        match shardManifestFilename with
        | None ->
            sb.Append("    <ItemGroup>\n") |> ignore
            for o in outputs do
                sb.AppendFormat("        <Compile Include=\"Generated\\Requests\\{0}\" />\n", o.Filename) |> ignore
            sb.Append("    </ItemGroup>\n") |> ignore
        | Some(shardedDomainName, manifestFilename) ->
            for o in outputs do
                if o.Domain = shardedDomainName then
                    sb.AppendFormat("    <Import Project=\"{0}\" />\n", manifestFilename) |> ignore
                else
                    sb.Append("    <ItemGroup>\n") |> ignore
                    sb.AppendFormat("        <Compile Include=\"Generated\\Requests\\{0}\" />\n", o.Filename) |> ignore
                    sb.Append("    </ItemGroup>\n") |> ignore
        sb.Append("</Project>\n") |> ignore
        File.WriteAllText(manifestPath, sb.ToString())
        log.LogInformation("Wrote {Path} ({Files} domains)", manifestPath, outputs.Length)

    /// Generate writer functions module from API schema + overrides.
    let generateWriterModule
        (runtimeNs: string)
        (config: OverrideConfig)
        (apiSchema: TlSchema)
        (outputPath: string) =

        let code =
            EmitWriters.generateWriterModule
                runtimeNs
                apiSchema
                config.WriterWhitelist
                config.WriterLayerTypes
                config.LayerVariants
                config.StructuralOverlays
                config.WriterRecordPerCaseUnions

        let dir = System.IO.Path.GetDirectoryName(outputPath)
        if Directory.notExists dir then Directory.create dir
        // Skip Fantomas: generated DU cases with many fields produce very long lines
        // that break Fantomas. Raw output is readable enough for generated code.
        File.WriteAllText(outputPath, code)
        log.LogInformation("Wrote {Path} ({Bytes} bytes, {Lines} lines)",
            outputPath, code.Length, code.Split('\n').Length)

    /// Generate response parsers for an arbitrary list of constructor types.
    /// The type list is driven by `config.ClientParserWhitelist` (TOML field
    /// `[whitelists].client_parsers`). When empty, the target produces an
    /// effectively empty module — no implicit defaults.
    let generateClientParsers
        (clientNs: string)
        (config: OverrideConfig)
        (apiSchema: TlSchema)
        (outputPath: string) =

        // Build alias map from layer variants — so Deserialize accepts all layer CIDs
        let aliasMap =
            config.LayerVariants
            |> List.map (fun lv -> lv.Name, lv.Variants |> List.map snd)
            |> Map.ofList

        // Subtract types that will already be emitted in the shared Requests
        // module — previous behaviour emitted e.g. `Peer` / `ChatAdminRights`
        // in BOTH namespaces, so downstream files that needed both Request-
        // side and Response-side types hit F# DU-case-name ambiguity and had
        // to wrap one side in its own submodule. BFS-resolve both whitelists,
        // emit only the response-only residual in ResponseParsers.
        let requestSeeds =
            SchemaMapper.deriveTypeSeeds apiSchema config.TypeWhitelist config.WriterWhitelist config.WriterLayerTypes
        let (requestTypes, _) =
            SchemaMapper.mapSchemaWhitelisted apiSchema requestSeeds config.StubTypes aliasMap
        let requestTypeNames =
            requestTypes
            |> List.map (fun t ->
                match t with
                | Record(name, _, _) -> name
                | Union(name, _) -> name)
            |> Set.ofList

        let (allParserTypes, functions) =
            SchemaMapper.mapSchemaWhitelisted apiSchema config.ClientParserWhitelist Set.empty aliasMap

        let types =
            allParserTypes
            |> List.filter (fun t ->
                let n =
                    match t with
                    | Record(name, _, _) -> name
                    | Union(name, _) -> name
                not (requestTypeNames.Contains n))

        // Also open the Requests namespace so the response-only types can
        // reference shared types (Peer / ChatAdminRights / …) that live there.
        let code =
            EmitTypes.buildModuleWithOpens
                clientNs "ResponseParsers" [ "TDesu.Serialization.Requests" ]
                types functions
        let header =
            Managed.banner "dotnet fsi tools/regen-tl.fsx (or td-tl-gen --target client-parsers)"
            + $"// Source: {types.Length} types, {functions.Length} functions\n\n"

        let dir = System.IO.Path.GetDirectoryName(outputPath)
        if Directory.notExists dir then Directory.create dir
        File.WriteAllText(outputPath, header + code)
        log.LogInformation("Wrote {Path} ({Types} types, {Funcs} functions)",
            outputPath, types.Length, functions.Length)


    /// Generate ergonomics file — Create factories on records + DU cases
    /// plus struct field-extractor active patterns. Scoped to
    /// `config.TypeWhitelist` only (unlike `generateSerializationTypes`,
    /// which also closes over the writer whitelist — see
    /// `SchemaMapper.deriveTypeSeeds`) so the Create/Pattern surface exactly
    /// matches what a caller who whitelists only `types` gets; the wider
    /// writer-implied types the `types` target additionally emits still
    /// resolve fine without a factory.
    let generateErgonomics
        (runtimeNs: string)
        (config: OverrideConfig)
        (apiSchema: TlSchema)
        (outputPath: string) =

        let aliasMap = EmitTemplates.buildAliasMap config
        let types, functions =
            SchemaMapper.mapSchemaWhitelisted
                apiSchema
                config.TypeWhitelist
                config.StubTypes
                aliasMap

        let code = EmitErgonomics.generate $"{runtimeNs}.Requests" types functions

        let dir = System.IO.Path.GetDirectoryName(outputPath)
        if Directory.notExists dir then Directory.create dir
        File.WriteAllText(outputPath, code)
        log.LogInformation("Wrote {Path} ({Bytes} bytes)", outputPath, code.Length)