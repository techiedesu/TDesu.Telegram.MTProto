namespace TDesu.Telegram.TL.Generator

open System.IO
open Microsoft.Extensions.Logging
open TDesu.FSharp
open TDesu.FSharp.Operators
open TDesu.FSharp.Tasks
open TDesu.FSharp.Utilities
open TDesu.Telegram.TL
open TDesu.Telegram.TL.Generator.Overrides

module Program =

    let private srcDir = System.IO.Path.GetFullPath(System.IO.Path.Combine(__SOURCE_DIRECTORY__, ".."))

    let private tryGetArgValue (name: string) (argv: string array) =
        argv
        |> Array.tryFindIndex (fun s -> s = name)
        |> Option.bind (fun i -> if i + 1 < argv.Length then Some argv[i + 1] else None)

    [<EntryPoint>]
    let main argv =
        let log = Logger.get "MTProto.TL.Generator"
        let cidOnly = argv |> Array.contains "--cid-only"
        let serializationTypes = argv |> Array.contains "--serialization-types"
        let writers = argv |> Array.contains "--writers"

        // Load override config: embedded defaults + optional user overrides
        let config =
            match argv |> tryGetArgValue "--overrides" with
            | Some path ->
                log.LogInformation("Loading overrides from {path}...", path)
                Config.loadWithOverlay path
            | None ->
                Config.loadDefaults ()

        Downloader.downloadIfNotCached () |> Task.runSynchronously

        let mtprotoText = Downloader.getMtprotoSchema ()
        let apiText = Downloader.getApiSchema ()

        log.LogInformation("Parsing MTProto schema...")
        let mtprotoSchema =
            match AstFactory.parse mtprotoText with
            | Ok schema -> schema
            | Error err -> failwith $"Failed to parse MTProto schema: %s{err}"

        log.LogInformation("Parsing API schema...")
        let apiSchema =
            match AstFactory.parse apiText with
            | Ok schema -> schema
            | Error err -> failwith $"Failed to parse API schema: %s{err}"

        log.LogInformation(
            "MTProto: {ctors} constructors, {funcs} functions",
            mtprotoSchema.Constructors.Length,
            mtprotoSchema.Functions.Length
        )
        log.LogInformation(
            "API: {ctors} constructors, {funcs} functions",
            apiSchema.Constructors.Length,
            apiSchema.Functions.Length
        )

        // Always generate CID module
        let cidCode = EmitTemplates.generateCidModule config mtprotoSchema apiSchema
        let cidPath = Path.Combine(srcDir, "MTProto.Serialization", "GeneratedCid.g.fs")
        File.WriteAllText(cidPath, cidCode)
        log.LogInformation("Wrote GeneratedCid.g.fs ({count} bytes, {overrides} overrides)",
            cidCode.Length, config.Aliases.Length + config.Extras.Length)

        // Generate whitelist-filtered serialization types
        if serializationTypes then
            let serPath = Path.Combine(srcDir, "MTProto.Serialization", "GeneratedTlRequests.g.fs")
            log.LogInformation("Generating serialization types to {path}...", serPath)
            Pipeline.generateSerializationTypes config apiSchema serPath

        // Generate writer functions
        if writers then
            let writerPath = Path.Combine(srcDir, "MTProto.Serialization", "GeneratedTlWriters.g.fs")
            log.LogInformation("Generating writer functions to {path}...", writerPath)
            Pipeline.generateWriterModule config apiSchema writerPath

        // Generate coverage validator
        if argv |> Array.contains "--coverage" then
            let covPath = Path.Combine(srcDir, "MTProto.Serialization", "GeneratedCoverageValidator.g.fs")
            log.LogInformation("Generating coverage validator to {path}...", covPath)
            EmitTemplates.generateCoverageValidator config apiSchema covPath

        // Generate return type mapping
        if argv |> Array.contains "--return-types" then
            let rtPath = Path.Combine(srcDir, "MTProto.Serialization", "GeneratedReturnTypes.g.fs")
            log.LogInformation("Generating return type map to {path}...", rtPath)
            EmitTemplates.generateReturnTypeMap config apiSchema rtPath

        // Generate round-trip tests
        if argv |> Array.contains "--tests" then
            let testPath = Path.Combine(srcDir, "MTProto.Tests", "Serialization", "GeneratedRoundTripTests.g.fs")
            log.LogInformation("Generating round-trip tests to {path}...", testPath)
            EmitTemplates.generateRoundTripTests config apiSchema testPath

        // Generate layer CID aliases (compare L216 base with L223 new)
        if argv |> Array.contains "--layer-aliases" then
            let newSchemaPath = Path.Combine(srcDir, "cached", "api.tl")
            if File.Exists(newSchemaPath) then
                let newText = File.ReadAllText(newSchemaPath) |> Downloader.preprocess
                match AstFactory.parse newText with
                | Ok newSchema ->
                    let aliasPath = Path.Combine(srcDir, "MTProto.Serialization", "GeneratedLayerAliases.g.fs")
                    log.LogInformation("Generating layer aliases to {path}...", aliasPath)
                    EmitTemplates.generateLayerAliases apiSchema newSchema aliasPath
                | Error err ->
                    log.LogError("Failed to parse new schema: {Error}", err)
            else
                log.LogWarning("New schema not found at {Path}, skipping layer aliases", newSchemaPath)

        // Generate client CID constants
        if argv |> Array.contains "--client-cids" then
            let clientCidPath = Path.Combine(srcDir, "MTProto.Client", "Api", "GeneratedClientCid.g.fs")
            log.LogInformation("Generating client CIDs to {path}...", clientCidPath)
            EmitTemplates.generateClientCids apiSchema clientCidPath

        let tests = argv |> Array.contains "--tests"
        let coverage = argv |> Array.contains "--coverage"
        let returnTypes = argv |> Array.contains "--return-types"
        let layerAliases = argv |> Array.contains "--layer-aliases"
        // Generate client response parsers
        if argv |> Array.contains "--client-parsers" then
            let parserPath = Path.Combine(srcDir, "MTProto.Client", "Api", "GeneratedResponseParsers.g.fs")
            log.LogInformation("Generating client response parsers to {path}...", parserPath)
            Pipeline.generateClientParsers config apiSchema parserPath

        let clientCids = argv |> Array.contains "--client-cids"
        let clientParsers = argv |> Array.contains "--client-parsers"
        if not cidOnly && not serializationTypes && not writers && not tests && not coverage && not returnTypes && not layerAliases && not clientCids && not clientParsers then
            let outputDir = Path.Combine(srcDir, "MTProto", "Generated")
            log.LogInformation("Output directory: {dir}", outputDir)
            Pipeline.generate config mtprotoSchema apiSchema outputDir

        log.LogInformation("Code generation complete.")
        0
