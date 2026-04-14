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

        let parserWhitelist = config.ClientParserWhitelist

        // Build alias map from layer variants — so Deserialize accepts all layer CIDs
        let aliasMap =
            config.LayerVariants
            |> List.map (fun lv -> lv.Name, lv.Variants |> List.map snd)
            |> Map.ofList

        let (types, functions) =
            SchemaMapper.mapSchemaWhitelisted apiSchema parserWhitelist Set.empty aliasMap

        let code = EmitTypes.buildModule clientNs "ResponseParsers" types functions
        let header =
            "// Auto-generated response parsers. Do not edit manually.\n"
            + "// Re-generate with: td-tl-gen --target client-parsers --overrides <your.toml>\n"
            + $"// Source: {types.Length} types, {functions.Length} functions\n\n"

        let dir = System.IO.Path.GetDirectoryName(outputPath)
        if Directory.notExists dir then Directory.create dir
        File.WriteAllText(outputPath, header + code)
        log.LogInformation("Wrote {Path} ({Types} types, {Funcs} functions)",
            outputPath, types.Length, functions.Length)
