namespace TDesu.Telegram.TL.Generator

open System.IO
open Microsoft.Extensions.Logging
open TDesu.FSharp
open TDesu.FSharp.Operators
open TDesu.FSharp.IO
open TDesu.FSharp.Utilities
open TDesu.Telegram.TL.AST
open TDesu.Telegram.TL.Generator.Overrides

module EmitTemplates =

    let private log = Logger.get "EmitTemplates"

    let private pascalCase = Naming.pascalCase
    let private combinatorName = Combinator.pascalName
    let private getCombinatorId = Combinator.id

    /// Build alias map from config for union case augmentation.
    let buildAliasMap (config: OverrideConfig) : Map<string, uint32 list> =
        config.Aliases
        |> List.map (fun a -> a.Name, a.Cids)
        |> Map.ofList

    /// Generate the Cid + LayerCid modules with overrides applied.
    let generateCidModule (ns: string) (config: OverrideConfig) (mtprotoSchema: TlSchema) (apiSchema: TlSchema) : string =
        let sb = System.Text.StringBuilder()
        let ln (s: string) = sb.AppendLine(s) |> ignore
        let ln0 () = sb.AppendLine() |> ignore

        ln "// Auto-generated TL constructor IDs. Do not edit manually."
        ln "// Re-generate with: td-tl-gen --target cid --overrides <your.toml>"
        ln0 ()
        ln $"namespace {ns}"
        ln0 ()

        // --- GeneratedCid module ---
        ln "/// Auto-generated constructor IDs from TL schema + client overrides."
        ln "[<RequireQualifiedAccess>]"
        ln "module GeneratedCid ="
        ln0 ()

        // Collect all schema CIDs
        let seen = System.Collections.Generic.HashSet<string>()

        let emitSection (label: string) (combinators: TlCombinator list) =
            ln $"    // --- %s{label} ---"
            for c in combinators do
                let name = combinatorName c
                if seen.Add(name) then
                    let cid = getCombinatorId c
                    ln $"    [<Literal>]"
                    ln $"    let %s{name} = 0x%08X{cid}u"
            ln0 ()

        emitSection "MTProto Constructors" mtprotoSchema.Constructors
        emitSection "MTProto Functions" mtprotoSchema.Functions
        emitSection "API Constructors" apiSchema.Constructors
        emitSection "API Functions" apiSchema.Functions

        // Aliases: emit numbered suffixes for extra CIDs
        ln "    // --- Aliases (multi-CID methods from different client layers) ---"
        for a in config.Aliases do
            for i, cid in a.Cids |> List.indexed do
                let suffix = if i = 0 then "" else string (i + 1)
                let name = $"%s{a.Name}%s{suffix}"
                if seen.Add(name) then
                    ln $"    [<Literal>]"
                    ln $"    let %s{name} = 0x%08X{cid}u"
        ln0 ()

        // Extra undocumented CIDs
        ln "    // --- Extra (undocumented / client-specific) ---"
        for e in config.Extras do
            if seen.Add(e.Name) then
                ln $"    /// %s{e.Comment}"
                ln $"    [<Literal>]"
                ln $"    let %s{e.Name} = 0x%08X{e.Cid}u"
        ln0 ()

        // --- LayerCid module ---
        ln "/// Layer-dependent constructor IDs. Resolved at runtime by negotiated layer."
        ln "[<RequireQualifiedAccess>]"
        ln "module GeneratedLayerCid ="
        ln0 ()
        ln "    /// Default layer when client hasn't sent invokeWithLayer yet."
        ln "    [<Literal>]"
        ln "    let DefaultLayer = 223"
        ln0 ()
        ln "    /// Minimum supported layer."
        ln "    [<Literal>]"
        ln "    let MinSupportedLayer = 190"
        ln0 ()

        for v in config.LayerVariants do
            // Emit private literals for each variant
            for _, (maxLayer, cid) in v.Variants |> List.indexed do
                let suffix =
                    match maxLayer with
                    | Some l -> $"Layer%d{l}"
                    | None -> "Default"
                ln $"    [<Literal>]"
                ln $"    let private %s{v.Name}%s{suffix} = 0x%08X{cid}u"

            // Emit resolver function
            ln $"    /// Get the %s{v.Name} constructor ID for the given layer."
            let fnName = let s = v.Name in (string (System.Char.ToLowerInvariant(s[0]))) + s[1..]
            ln $"    let %s{fnName} (layer: int) ="

            // Sort variants by maxLayer ascending, None (default) last
            let sorted =
                v.Variants
                |> List.sortBy (fun (ml, _) -> match ml with Some l -> l | None -> System.Int32.MaxValue)

            let mutable isFirst = true
            for maxLayer, _ in sorted do
                let suffix =
                    match maxLayer with
                    | Some l -> $"Layer%d{l}"
                    | None -> "Default"
                match maxLayer with
                | Some l ->
                    let kw = if isFirst then "if" else "elif"
                    ln $"        %s{kw} layer <= %d{l} then %s{v.Name}%s{suffix}"
                    isFirst <- false
                | None ->
                    ln $"        else %s{v.Name}%s{suffix}"
            ln0 ()

        // --- MethodNames module: CID → TL method name ---
        ln "/// Runtime CID → TL method name lookup for logging and dispatch."
        ln "[<RequireQualifiedAccess>]"
        ln "module GeneratedMethodNames ="
        ln0 ()

        // Only functions (not constructors) -- these are the RPC methods
        let allFunctions =
            [| yield! mtprotoSchema.Functions |> List.map (fun f -> getCombinatorId f, f)
               yield! apiSchema.Functions |> List.map (fun f -> getCombinatorId f, f) |]

        ln "    /// Lookup table: CID → TL method name."
        ln $"    let private entries = [|"
        for cid, f in allFunctions do
            let tlName =
                match f.Id.Namespace with
                | Some ns -> $"%s{ns}.%s{f.Id.Name}"
                | None -> f.Id.Name
            ln $"        0x%08X{cid}u, \"%s{tlName}\""
        // Add aliases
        for a in config.Aliases do
            let baseName = a.Cids |> List.head
            // Find the TL name from the first CID
            let tlName =
                allFunctions
                |> Array.tryFind (fun (cid, _) -> cid = baseName)
                |> Option.map (fun (_, f) ->
                    match f.Id.Namespace with Some ns -> $"%s{ns}.%s{f.Id.Name}" | None -> f.Id.Name)
                |> Option.defaultValue a.Name
            for cid in a.Cids |> List.tail do
                ln $"        0x%08X{cid}u, \"%s{tlName}\""
        ln "    |]"
        ln0 ()
        ln "    let private lookup = System.Collections.Generic.Dictionary<uint32, string>(entries.Length)"
        ln "    do for cid, name in entries do lookup.[cid] <- name"
        ln0 ()
        ln "    /// Get TL method name for a CID, or None if unknown."
        ln "    let tryGet (cid: uint32) : string option ="
        ln "        match lookup.TryGetValue(cid) with"
        ln "        | true, name -> Some name"
        ln "        | false, _ -> None"
        ln0 ()
        ln "    /// Get TL method name, or hex fallback."
        ln "    let getOrHex (cid: uint32) : string ="
        ln "        match lookup.TryGetValue(cid) with"
        ln "        | true, name -> name"
        ln $"        | false, _ -> $\"0x%%08X{{cid}}\""
        ln0 ()

        sb.ToString()

    /// Generate round-trip tests for all request types with Serialize+Deserialize.
    /// `ns` is the F# module name to emit (e.g. `MyApp.Tests.GeneratedRoundTripTests`).
    /// `runtimeNs` is the namespace where the request types live (e.g. `MyApp.Serialization`).
    let generateRoundTripTests
        (ns: string) (runtimeNs: string)
        (config: OverrideConfig) (apiSchema: TlSchema) (outputPath: string) =
        let aliasMap = buildAliasMap config
        let _, functions =
            SchemaMapper.mapSchemaWhitelisted apiSchema config.TypeWhitelist config.StubTypes aliasMap

        let sb = System.Text.StringBuilder()
        let ln (s: string) = sb.AppendLine(s) |> ignore

        ln "// Auto-generated round-trip tests. Do not edit manually."
        ln "// Re-generate with: td-tl-gen --target tests --overrides <your.toml>"
        ln ""
        ln $"module {ns}"
        ln ""
        ln "open NUnit.Framework"
        ln $"open {runtimeNs}"
        ln $"open {runtimeNs}.Requests"
        ln ""
        ln "[<TestFixture>]"
        ln "type RoundTripTests() ="
        ln ""

        // Map known union types to their empty/simplest case (manual overrides)
        let duOverrides = Map.ofList [
            "InputReplyTo", "InputReplyTo.InputReplyToMessage(0, None, None, None, None, None, None, None)"
            "InputDialogPeer", "InputDialogPeer.InputDialogPeer(InputPeer.InputPeerEmpty)"
            "InputFileLocation", "InputFileLocation.InputFileLocation(0L, 0, 0L, Array.empty)"
            "EmailVerification", "Unchecked.defaultof<EmailVerification>"
        ]

        // Auto-build DU defaults from schema: pick the case with fewest fields (prefer Empty cases)
        let types, _ = SchemaMapper.mapSchemaWhitelisted apiSchema config.TypeWhitelist config.StubTypes aliasMap

        let rec simpleDefaultPrim (t: string) =
            match t with
            | "int32" -> "0" | "int64" -> "0L" | "double" -> "0.0"
            | "bool" -> "false" | "string" -> "\"\"" | "byte[]" -> "Array.empty"
            | t when t.EndsWith(" option") -> "None"
            | t when t.EndsWith(" array") -> "[||]"
            | _ -> $"Unchecked.defaultof<%s{t}>"

        let duDefaults =
            let auto =
                types |> List.choose (fun t ->
                    match t with
                    | Union(name, cases) ->
                        // Pick case with fewest fields (prefer fieldless/empty cases)
                        let sorted = cases |> List.sortBy (fun c -> c.Fields.Length)
                        match sorted with
                        | bestCase :: _ ->
                            if bestCase.Fields.IsEmpty then
                                Some(name, $"%s{name}.%s{bestCase.Name}")
                            else
                                let args = bestCase.Fields |> List.map (fun f -> simpleDefaultPrim f.FSharpType) |> String.concat ", "
                                Some(name, $"%s{name}.%s{bestCase.Name}(%s{args})")
                        | [] -> None
                    | _ -> None)
                |> Map.ofList
            // Manual overrides take precedence over auto-discovered
            auto |> Map.fold (fun acc k v -> if Map.containsKey k acc then acc else Map.add k v acc) duOverrides

        // Simple default (no record lookup)
        let simpleDefault (t: string) =
            match t with
            | "int32" -> "0" | "int64" -> "0L" | "double" -> "0.0"
            | "bool" -> "false" | "string" -> "\"\"" | "byte[]" -> "Array.empty"
            | t when t.EndsWith(" option") -> "None"
            | t when t.EndsWith(" array") -> "[||]"
            | t -> duDefaults |> Map.tryFind t |> Option.defaultValue $"Unchecked.defaultof<%s{t}>"

        // Auto-build record defaults from schema (2-pass: first simple, then with record refs)
        let recordDefaults =
            let records =
                types |> List.choose (fun t ->
                    match t with
                    | Record(name, fields, _) when not (duDefaults.ContainsKey name) ->
                        let flagFields = fields |> List.choose (fun f -> f.FlagField) |> List.distinct
                        let dataFields = fields |> List.filter (fun f ->
                            not (f.FSharpType = "int32" && flagFields |> List.contains f.Name))
                        Some(name, dataFields)
                    | _ -> None)
            // Pass 1: build with simpleDefault
            let pass1 =
                records |> List.map (fun (name, fields) ->
                    let inits = fields |> List.map (fun f -> $"%s{f.Name} = %s{simpleDefault f.FSharpType}") |> String.concat "; "
                    name, $"{{ %s{name}.%s{inits} }}")
                |> Map.ofList
            // Pass 2: rebuild using pass1 for nested record references
            let lookup t =
                match duDefaults |> Map.tryFind t with
                | Some e -> e
                | None -> pass1 |> Map.tryFind t |> Option.defaultValue (simpleDefault t)
            records |> List.map (fun (name, fields) ->
                let inits = fields |> List.map (fun f -> $"%s{f.Name} = %s{lookup f.FSharpType}") |> String.concat "; "
                name, $"{{ %s{name}.%s{inits} }}")
            |> Map.ofList

        for f in functions do
            // Build record construction with safe defaults
            // Exclude raw flags int fields (computed from optional/bool fields, not in generated record)
            let dataFields =
                let flagFields = f.Params |> List.choose (fun p -> p.FlagField) |> List.distinct
                f.Params |> List.filter (fun p ->
                    not (p.FSharpType = "int32" && flagFields |> List.contains p.Name))
            let fieldInits =
                dataFields
                |> List.map (fun p ->
                    let def =
                        match recordDefaults |> Map.tryFind p.FSharpType with
                        | Some expr -> expr
                        | None -> simpleDefault p.FSharpType
                    $"{p.Name} = {def}")
                |> String.concat "; "

            ln $"    [<Test>]"
            ln $"    member _.``{f.Name} round-trip``() ="
            ln $"        let v : {f.Name} = {{ {fieldInits} }}"
            ln $"        use w1 = new TlWriteBuffer()"
            ln $"        {f.Name}.Serialize(w1, v)"
            ln $"        let bytes1 = w1.ToArray()"
            ln $"        let v2 = {f.Name}.Deserialize(bytes1)"
            ln $"        use w2 = new TlWriteBuffer()"
            ln $"        {f.Name}.Serialize(w2, v2)"
            ln $"        let bytes2 = w2.ToArray()"
            ln $"        Assert.That(bytes2, Is.EqualTo(bytes1 :> obj), \"{f.Name} round-trip mismatch\")"
            ln ""

        let dir = System.IO.Path.GetDirectoryName(outputPath)
        if Directory.notExists dir then
            Directory.create dir
        File.WriteAllText(outputPath, sb.ToString())
        log.LogInformation("Wrote GeneratedRoundTripTests.g.fs ({count} tests)", functions.Length)

    /// Generate function CID alias map by comparing two schemas.
    /// Detects: same-name CID changes + namespace-moved functions (channels→messages).
    /// Use this when you need a server to accept method calls from clients on different layers.
    let generateLayerAliases (ns: string) (baseSchema: TlSchema) (newSchema: TlSchema) (outputPath: string) =
        let sb = System.Text.StringBuilder()
        let ln (s: string) = sb.AppendLine(s) |> ignore
        let ln0 () = sb.AppendLine() |> ignore

        ln "// Auto-generated layer CID aliases. Do not edit manually."
        ln "// Re-generate with: td-tl-gen --target layer-aliases --layer-base-schema <old.tl>"
        ln0 ()
        ln $"namespace {ns}"
        ln0 ()
        ln "/// L223→L216 function CID aliases for dual-layer compatibility."
        ln "[<RequireQualifiedAccess>]"
        ln "module GeneratedLayerAliases ="
        ln0 ()

        let baseFuncs =
            baseSchema.Functions |> List.map (fun f ->
                let name =
                    match f.Id.Namespace with
                    | Some ns -> $"{ns}.{f.Id.Name}"
                    | None -> f.Id.Name
                name, getCombinatorId f) |> Map.ofList

        let newFuncs =
            newSchema.Functions |> List.map (fun f ->
                let name =
                    match f.Id.Namespace with
                    | Some ns -> $"{ns}.{f.Id.Name}"
                    | None -> f.Id.Name
                name, getCombinatorId f) |> Map.ofList

        // Build basename -> (fullname, cid) for base schema (for namespace-move detection)
        let baseByBasename =
            baseFuncs |> Map.toList |> List.map (fun (name, cid) ->
                let basename = match name.LastIndexOf('.') with i when i >= 0 -> name.Substring(i + 1) | _ -> name
                basename, (name, cid))
            |> List.groupBy fst |> List.map (fun (k, vs) -> k, vs |> List.map snd) |> Map.ofList

        let mutable aliases = []

        // 1. Same name, different CID
        for KeyValue(name, newCid) in newFuncs do
            match baseFuncs |> Map.tryFind name with
            | Some baseCid when baseCid <> newCid ->
                aliases <- (newCid, baseCid, name, name) :: aliases
            | _ -> ()

        // 2. Namespace-moved functions (e.g., channels.createForumTopic -> messages.createForumTopic)
        for KeyValue(newName, newCid) in newFuncs do
            if not (baseFuncs.ContainsKey newName) then
                let basename = match newName.LastIndexOf('.') with i when i >= 0 -> newName.Substring(i + 1) | _ -> newName
                match baseByBasename |> Map.tryFind basename with
                | Some candidates ->
                    for (baseName, baseCid) in candidates do
                        if not (newFuncs.ContainsKey baseName) then // old name removed in new schema
                            aliases <- (newCid, baseCid, newName, baseName) :: aliases
                | None -> ()

        let sorted = aliases |> List.sortBy (fun (_, _, newName, _) -> newName)

        ln $"    /// All L223→L216 function CID aliases ({sorted.Length} total)."
        if sorted.IsEmpty then
            // F# parses `[|\n    |]` as offside; emit a single-line empty literal.
            ln "    let aliases : (uint32 * uint32) array = [||]"
        else
            ln "    let aliases : (uint32 * uint32) array = [|"
            for (newCid, baseCid, newName, baseName) in sorted do
                let comment = if newName = baseName then newName else $"{newName} -> {baseName}"
                ln $"        0x%08X{newCid}u, 0x%08X{baseCid}u // {comment}"
            ln "    |]"
        ln0 ()

        // 3. NotModified constructor CIDs -- server should avoid returning these on first request (hash=0)
        let notModifiedCids =
            baseSchema.Constructors
            |> List.filter (fun c ->
                let name = match c.Id.Namespace with Some ns -> $"{ns}.{c.Id.Name}" | None -> c.Id.Name
                name.Contains("NotModified") || name.Contains("notModified"))
            |> List.map (fun c -> getCombinatorId c, match c.Id.Namespace with Some ns -> $"{ns}.{c.Id.Name}" | None -> c.Id.Name)
            |> List.sortBy snd

        ln $"    /// NotModified constructor CIDs ({notModifiedCids.Length} total)."
        ln "    /// tdesktop rejects these when client sends hash=0 (first request)."
        ln "    /// Server should return full empty response instead."
        ln "    let notModifiedCids : System.Collections.Generic.HashSet<uint32> ="
        ln "        System.Collections.Generic.HashSet<uint32>([|"
        for (cid, name) in notModifiedCids do
            ln $"            0x%08X{cid}u // {name}"
        ln "        |])"
        ln0 ()

        let dir = System.IO.Path.GetDirectoryName(outputPath)
        if Directory.notExists dir then Directory.create dir
        File.WriteAllText(outputPath, sb.ToString())
        log.LogInformation("Wrote GeneratedLayerAliases.g.fs ({count} aliases, {notModCount} notModified CIDs)", sorted.Length, notModifiedCids.Length)

    /// Generate handler coverage validator: checks registered CIDs against known TL functions.
    let generateCoverageValidator (ns: string) (config: OverrideConfig) (apiSchema: TlSchema) (outputPath: string) =
        let sb = System.Text.StringBuilder()
        let ln (s: string) = sb.AppendLine(s) |> ignore
        let ln0 () = sb.AppendLine() |> ignore

        ln "// Auto-generated handler coverage validator. Do not edit manually."
        ln "// Re-generate with: td-tl-gen --target coverage --overrides <your.toml>"
        ln0 ()
        ln $"namespace {ns}"
        ln0 ()
        ln "/// Validates handler coverage: which TL functions have registered handlers."
        ln "[<RequireQualifiedAccess>]"
        ln "module GeneratedCoverageValidator ="
        ln0 ()

        // All API function CIDs
        let functions =
            apiSchema.Functions
            |> List.map (fun f ->
                let cid = getCombinatorId f
                let name = match f.Id.Namespace with Some ns -> $"%s{ns}.%s{f.Id.Name}" | None -> f.Id.Name
                cid, name)

        // Aliases in tl-overrides.toml cover both type constructors and function
        // variants. Only the function ones belong here; including type aliases
        // (e.g. InputPeerUser, InputReplyToMessage) inflates the missing count
        // with CIDs that no caller would ever register as a handler.
        // Build a Pascal-name → tlName map of real functions so aliases can be
        // resolved to functions by name and type aliases can be filtered out.
        let pascalOf (tlName: string) =
            // "messages.sendMessage" -> "MessagesSendMessage"
            tlName.Split('.')
            |> Array.map (fun s -> if s.Length = 0 then s else string (System.Char.ToUpperInvariant(s[0])) + s[1..])
            |> String.concat ""
        let funcByPascal =
            functions
            |> List.map (fun (_, name) -> pascalOf name, name)
            |> Map.ofList

        let functionAliases =
            config.Aliases
            |> List.choose (fun a ->
                Map.tryFind a.Name funcByPascal
                |> Option.map (fun tlName -> tlName, a.Cids))

        let entries =
            [ yield! functions
              for tlName, cids in functionAliases do
                  for cid in cids do
                      yield cid, tlName ]

        ln $"    /// All {functions.Length} API function CIDs (+{entries.Length - functions.Length} alias CIDs)."
        ln $"    let allFunctions : (uint32 * string)[] = [|"
        for cid, name in entries do
            ln $"        0x%08X{cid}u, \"%s{name}\""
        ln "    |]"
        ln0 ()

        ln "    /// Validate registered handlers. Returns (covered, missing, extra) method names."
        ln "    let validate (registeredCids: uint32 seq) : string list * string list * string list ="
        ln "        let registered = System.Collections.Generic.HashSet<uint32>(registeredCids)"
        ln "        let allSet = System.Collections.Generic.HashSet<uint32>()"
        ln "        let covered = System.Collections.Generic.List<string>()"
        ln "        let missing = System.Collections.Generic.List<string>()"
        ln "        for cid, name in allFunctions do"
        ln "            if allSet.Add(cid) then"
        ln "                if registered.Contains(cid) then covered.Add(name)"
        ln "                else missing.Add(name)"
        ln "        let extra = [for cid in registered do if not (allSet.Contains cid) then yield $\"0x%08X{cid}\"]"
        ln "        (List.ofSeq covered, List.ofSeq missing, extra)"
        ln0 ()

        let dir = System.IO.Path.GetDirectoryName(outputPath)
        if Directory.notExists dir then Directory.create dir
        File.WriteAllText(outputPath, sb.ToString())
        log.LogInformation("Wrote GeneratedCoverageValidator.g.fs ({count} functions)", functions.Length)

    /// Generate RPC function -> return type mapping for typed handler validation.
    let generateReturnTypeMap (ns: string) (config: OverrideConfig) (apiSchema: TlSchema) (outputPath: string) =
        let sb = System.Text.StringBuilder()
        let ln (s: string) = sb.AppendLine(s) |> ignore
        let ln0 () = sb.AppendLine() |> ignore

        ln "// Auto-generated RPC return type mapping. Do not edit manually."
        ln "// Re-generate with: td-tl-gen --target return-types --overrides <your.toml>"
        ln0 ()
        ln $"namespace {ns}"
        ln0 ()
        ln "/// Maps each RPC function CID to its expected TL return type name."
        ln "[<RequireQualifiedAccess>]"
        ln "module GeneratedReturnTypes ="
        ln0 ()

        let functions =
            apiSchema.Functions
            |> List.map (fun f ->
                let cid = getCombinatorId f
                let methodName =
                    match f.Id.Namespace with
                    | Some ns -> $"%s{ns}.%s{f.Id.Name}"
                    | None -> f.Id.Name
                let returnType =
                    match f.ResultType with
                    | TlTypeExpr.Bare id | TlTypeExpr.Boxed id ->
                        match id.Namespace with
                        | Some ns -> $"%s{ns}.%s{id.Name}"
                        | None -> id.Name
                    | _ -> "Unknown"
                cid, methodName, returnType)

        ln $"    /// Lookup: CID → (methodName, returnType)."
        ln $"    let private entries = [|"
        for cid, name, retType in functions do
            ln $"        0x%08X{cid}u, (\"%s{name}\", \"%s{retType}\")"
        // Add aliases
        for a in config.Aliases do
            for cid in a.Cids do
                let entry = functions |> List.tryFind (fun (c, _, _) -> c = cid)
                match entry with
                | Some(_, name, retType) -> ln $"        0x%08X{cid}u, (\"%s{name}\", \"%s{retType}\")"
                | None -> ()
        ln "    |]"
        ln0 ()

        ln "    let private lookup = System.Collections.Generic.Dictionary<uint32, string * string>(entries.Length)"
        ln "    do for cid, v in entries do lookup.[cid] <- v"
        ln0 ()
        ln "    /// Get (methodName, returnType) for a CID."
        ln "    let tryGet (cid: uint32) : (string * string) option ="
        ln "        match lookup.TryGetValue(cid) with"
        ln "        | true, v -> Some v"
        ln "        | false, _ -> None"
        ln0 ()
        ln "    /// Get expected return type name for a CID."
        ln "    let returnType (cid: uint32) : string option ="
        ln "        match lookup.TryGetValue(cid) with"
        ln "        | true, (_, rt) -> Some rt"
        ln "        | false, _ -> None"
        ln0 ()

        let dir = System.IO.Path.GetDirectoryName(outputPath)
        if Directory.notExists dir then Directory.create dir
        File.WriteAllText(outputPath, sb.ToString())
        log.LogInformation("Wrote GeneratedReturnTypes.g.fs ({count} functions)", functions.Length)

    /// Generate CID constants module from an API schema (every function + constructor).
    /// Useful for clients that need a flat literal table without going through the full
    /// override pipeline.
    let generateClientCids (ns: string) (apiSchema: TlSchema) (outputPath: string) =
        let sb = System.Text.StringBuilder()
        let ln (s: string) = sb.AppendLine(s) |> ignore
        let ln0 () = sb.AppendLine() |> ignore

        ln "// Auto-generated client CID constants. Do not edit manually."
        ln "// Re-generate with: td-tl-gen --target client-cids"
        ln $"// Source: {apiSchema.Functions.Length} functions, {apiSchema.Constructors.Length} constructors"
        ln0 ()
        ln $"namespace {ns}"
        ln0 ()
        ln "/// Auto-generated TL constructor and function CIDs from the API schema."
        ln "[<RequireQualifiedAccess>]"
        ln "module ClientCid ="
        ln0 ()

        let fullName (c: TlCombinator) =
            match c.Id.Namespace with
            | Some ns -> $"{ns}.{c.Id.Name}"
            | None -> c.Id.Name

        let getCid (c: TlCombinator) =
            match c.ConstructorId with
            | Some (TlConstructorId cid) -> Some cid
            | None -> None

        // Functions (RPC methods the client calls)
        ln "    // === Functions (RPC methods) ==="
        for f in apiSchema.Functions |> List.sortBy (fun f -> fullName f) do
            match getCid f with
            | Some cid ->
                let name = pascalCase (fullName f)
                ln $"    [<Literal>]"
                ln $"    let %s{name} = 0x%08X{cid}u // %s{fullName f}"
            | None -> ()

        ln0 ()
        ln "    // === Constructors (response/object types) ==="
        for c in apiSchema.Constructors |> List.sortBy (fun c -> fullName c) do
            match getCid c with
            | Some cid when cid <> 0x1cb5c415u ->
                let name = pascalCase (fullName c)
                ln $"    [<Literal>]"
                ln $"    let %s{name} = 0x%08X{cid}u // %s{fullName c}"
            | _ -> ()

        let dir = System.IO.Path.GetDirectoryName(outputPath)
        if Directory.notExists dir then Directory.create dir
        File.WriteAllText(outputPath, sb.ToString())
        log.LogInformation("Wrote {path} ({funcs} functions, {ctors} constructors)",
            outputPath, apiSchema.Functions.Length, apiSchema.Constructors.Length)
