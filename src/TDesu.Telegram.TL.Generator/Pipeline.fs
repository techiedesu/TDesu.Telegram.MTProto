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
        let types, functions =
            SchemaMapper.mapSchemaWhitelisted
                apiSchema
                config.TypeWhitelist
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

    /// Same content as `generateSerializationTypes`, but split into per-domain
    /// files under `<outputDir>/Requests/`. Domain detection is by leading
    /// PascalCase prefix (`MessagesSendMessage` → "Messages"); types without
    /// a recognized prefix go into `Base.g.fs`.
    ///
    /// Cycle resolution: any non-Base type referenced from a Base type is
    /// promoted to Base (transitively). On the current schema this only moves
    /// `MessagesEmojiGameOutcome` (referenced by `Base.MessageMedia`).
    ///
    /// Side effects:
    /// * Wipes `<outputDir>/Requests/*.g.fs` before writing fresh files.
    /// * Writes `<outputDir>/Requests/Requests.targets` (MSBuild manifest with
    ///   `<Compile Include>` entries in topological compile order).
    /// * Does NOT touch a sibling monolithic `GeneratedTlRequests.g.fs` —
    ///   callers should delete that themselves once the split is in use.
    let generateSerializationTypesSplit
        (runtimeNs: string)
        (config: OverrideConfig)
        (apiSchema: TlSchema)
        (outputDir: string)
        (domains: string list) =

        let aliasMap = EmitTemplates.buildAliasMap config
        let types, functions =
            SchemaMapper.mapSchemaWhitelisted
                apiSchema
                config.TypeWhitelist
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

        // Wipe stale .g.fs so renamed/empty domains don't linger.
        for f in Directory.EnumerateFiles(requestsDir, "*.g.fs") do
            File.Delete f

        let outputs =
            EmitTypes.buildPerDomainModules
                $"{runtimeNs}.Requests"
                []
                domains
                types
                functions

        for o in outputs do
            let path = Path.Combine(requestsDir, o.Filename)
            File.WriteAllText(path, o.Code)
            log.LogInformation(
                "Wrote {Path} ({Bytes} bytes, domain={Domain})",
                path, o.Code.Length, o.Domain)

        // Manifest .targets with <Compile Include> entries in the same order
        // as `outputs` (already topologically sorted, Base-first).
        let manifestPath = Path.Combine(requestsDir, "Requests.targets")
        let sb = System.Text.StringBuilder()
        sb.Append("<!-- Auto-generated by td-tl-gen --target types --split-by-domain. Do not edit. -->\n") |> ignore
        sb.Append("<Project>\n") |> ignore
        sb.Append("    <ItemGroup>\n") |> ignore
        for o in outputs do
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
        let (requestTypes, _) =
            SchemaMapper.mapSchemaWhitelisted apiSchema config.TypeWhitelist config.StubTypes aliasMap
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
            "// Auto-generated response parsers. Do not edit manually.\n"
            + "// Re-generate with: td-tl-gen --target client-parsers --overrides <your.toml>\n"
            + $"// Source: {types.Length} types, {functions.Length} functions\n\n"

        let dir = System.IO.Path.GetDirectoryName(outputPath)
        if Directory.notExists dir then Directory.create dir
        File.WriteAllText(outputPath, header + code)
        log.LogInformation("Wrote {Path} ({Types} types, {Funcs} functions)",
            outputPath, types.Length, functions.Length)
