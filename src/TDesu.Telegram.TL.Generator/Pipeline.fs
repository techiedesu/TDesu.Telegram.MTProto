namespace TDesu.Telegram.TL.Generator

open System.IO
open Microsoft.Extensions.Logging
open TDesu.FSharp
open TDesu.FSharp.Operators
open TDesu.FSharp.IO
open TDesu.FSharp.Utilities
open TDesu.Telegram.TL.AST
open TDesu.Telegram.TL.Generator.Overrides

module Pipeline =

    let private log = Logger.get "Pipeline"

    /// Generate whitelist-filtered serialization types for use in handlers.
    let generateSerializationTypes (config: OverrideConfig) (apiSchema: TlSchema) (outputPath: string) =
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

        // Debug: find whitelist names that didn't resolve
        let allNames = (types |> List.map (fun t -> match t with Record(n,_,_) -> n | Union(n,_) -> n)) @ (functions |> List.map (fun f -> f.Name)) |> Set.ofList
        let missing = config.TypeWhitelist |> Set.filter (fun n -> not (allNames.Contains n))
        if not missing.IsEmpty then
            log.LogWarning("Whitelist entries not found in schema: {missing}", missing)

        let code =
            EmitTypes.buildModule
                "TDesu.Serialization.Requests"
                "GeneratedTlRequests"
                types
                functions

        let dir = System.IO.Path.GetDirectoryName(outputPath)
        if Directory.notExists dir then
            Directory.create dir

        File.WriteAllText(outputPath, code)
        log.LogInformation("Wrote GeneratedTlRequests.g.fs ({count} bytes)", code.Length)

    /// Generate writer functions module from API schema + overrides.
    let generateWriterModule (config: OverrideConfig) (apiSchema: TlSchema) (outputPath: string) =
        let code =
            EmitWriters.generateWriterModule
                apiSchema
                config.WriterWhitelist
                config.WriterLayerTypes
                config.LayerVariants

        let dir = System.IO.Path.GetDirectoryName(outputPath)
        if Directory.notExists dir then
            Directory.create dir

        // Skip Fantomas: generated DU cases with many fields produce very long lines
        // that break Fantomas. Raw output is readable enough for generated code.
        File.WriteAllText(outputPath, code)
        log.LogInformation("Wrote GeneratedTlWriters.g.fs ({count} bytes, {lines} lines)",
            code.Length, code.Split('\n').Length)

    /// Generate response parsers for MTProto.Client by reusing the function code generator.
    /// Treats whitelisted constructors as "functions" to get record types with Serialize/Deserialize.
    let generateClientParsers (config: OverrideConfig) (apiSchema: TlSchema) (outputPath: string) =
        // Response constructor types to generate parsers for (client namespace, no conflicts)
        let parserWhitelist = Set.ofList [
            "Message"; "User"; "Chat"
        ]

        // Build alias map from layer variants -- so Deserialize accepts all layer CIDs
        let aliasMap =
            config.LayerVariants
            |> List.map (fun lv ->
                lv.Name, lv.Variants |> List.map snd)
            |> Map.ofList

        // Use mapSchemaWhitelisted to generate types+functions for whitelisted constructors
        let (types, functions) = SchemaMapper.mapSchemaWhitelisted apiSchema parserWhitelist Set.empty aliasMap

        // Build code using existing AstBuilder module builder
        let code = EmitTypes.buildModule "TDesu.MTProto.Client.Api.ResponseTypes" "ResponseParsers" types functions
        let header =
            "// Auto-generated response parsers for MTProto.Client. Do not edit manually.\n" +
            "// Re-generate with: dotnet run --project src/MTProto.TL.Generator -- --client-parsers\n" +
            $"// Source: cached/api.tl ({types.Length} types, {functions.Length} functions)\n\n"

        let dir = System.IO.Path.GetDirectoryName(outputPath)
        if Directory.notExists dir then Directory.create dir
        File.WriteAllText(outputPath, header + code)
        log.LogInformation("Wrote {path} ({types} types, {funcs} functions)", outputPath, types.Length, functions.Length)

    let generate (config: OverrideConfig) (mtprotoSchema: TlSchema) (apiSchema: TlSchema) (outputDir: string) =
        if Directory.notExists outputDir then
            Directory.create outputDir

        // MTProto types (EmitTypes.buildModule returns already-formatted code via FormatASTAsync)
        let mtprotoTypes, _ = SchemaMapper.mapSchema mtprotoSchema
        let mtprotoCode =
            EmitTypes.buildModule
                "TDesu.MTProto.Types"
                "MTProtoTypes"
                mtprotoTypes
                []

        File.WriteAllText(Path.Combine(outputDir, "MTProtoTypes.g.fs"), mtprotoCode)
        log.LogInformation("Wrote MTProtoTypes.g.fs")

        // API types
        let apiTypes, apiFunctions = SchemaMapper.mapSchema apiSchema
        let apiTypesCode =
            EmitTypes.buildModule
                "TDesu.MTProto.Types"
                "ApiTypes"
                apiTypes
                []

        File.WriteAllText(Path.Combine(outputDir, "ApiTypes.g.fs"), apiTypesCode)
        log.LogInformation("Wrote ApiTypes.g.fs")

        // API functions
        let apiFunctionsCode =
            EmitTypes.buildModule
                "TDesu.MTProto.Functions"
                "ApiFunctions"
                []
                apiFunctions

        File.WriteAllText(Path.Combine(outputDir, "ApiFunctions.g.fs"), apiFunctionsCode)
        log.LogInformation("Wrote ApiFunctions.g.fs")

        // CID constants module
        let cidCode = EmitTemplates.generateCidModule config mtprotoSchema apiSchema
        let cidOutputDir = Path.Combine(Path.GetDirectoryName(outputDir), "MTProto.Serialization")
        let cidPath = Path.Combine(cidOutputDir, "GeneratedCid.g.fs")
        log.LogInformation("Writing GeneratedCid.g.fs to {path}...", cidPath)
        File.WriteAllText(cidPath, cidCode)
        log.LogInformation("Wrote GeneratedCid.g.fs ({count} bytes)", cidCode.Length)
