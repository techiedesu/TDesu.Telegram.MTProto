namespace TDesu.Telegram.TL.Generator

open TDesu.Telegram.TL.AST

/// Maps TL schema to CodeModel types. Handles whitelist filtering and alias augmentation.
module SchemaMapper =

    let mapSchema (schema: TlSchema) : GeneratedType list * GeneratedFunction list =
        let grouped =
            schema.Constructors
            |> List.groupBy (fun c -> CodeModelMapping.getResultTypeName c.ResultType)

        let types = [
            for (resultTypeName, constructors) in grouped do
                match constructors with
                | [ single ] ->
                    let fields = single.Params |> List.map CodeModelMapping.mapParam
                    let ctorId = Combinator.id single

                    if fields.IsEmpty then
                        // F# forbids empty records (`type X = {}` → FS3863), so a
                        // single zero-field constructor (e.g. `true`, `null`) is
                        // emitted as a nullary single-case union (`type X = X`)
                        // instead. Reuses the union (de)serialize path, which
                        // already handles nullary cases. No currently-whitelisted
                        // type is zero-field (it would never have compiled), so
                        // this only affects the full-emit surface.
                        Union(
                            resultTypeName,
                            [ {
                                  Name = resultTypeName
                                  ConstructorId = ctorId
                                  AliasCids = []
                                  Fields = []
                              } ]
                        )
                    else
                        Record(resultTypeName, fields, ctorId)
                | multiple ->
                    let cases = [
                        for c in multiple do
                            let caseName = Combinator.pascalName c
                            {
                                Name = caseName
                                ConstructorId = Combinator.id c
                                AliasCids = []
                                Fields = c.Params |> List.map CodeModelMapping.mapParam
                            }
                    ]
                    Union(resultTypeName, cases)
        ]

        let functions = [
            for f in schema.Functions do
                let name = Combinator.pascalName f
                {
                    Name = name
                    ConstructorId = Combinator.id f
                    AliasCids = []
                    Params = f.Params |> List.map CodeModelMapping.mapParam
                    ReturnType = CodeModelMapping.mapTypeExprPublic f.ResultType
                }
        ]

        (types, functions)

    let private typeName = function Record(n, _, _) -> n | Union(n, _) -> n

    /// Every top-level C# identifier `EmitCSharp.setup` claims for a mapped
    /// surface: records, union bases (including the `…Base` rename applied when
    /// a case shares its union's name), union cases and functions.
    ///
    /// Mirrored here so collision filtering keys off the *emitted* identity and
    /// not the TL name — `pascalCase` folds `_` and `.` away, so `msgs_ack` and
    /// `msgsAck` (or `help.configSimple` and `helpConfigSimple`) are distinct in
    /// TL yet the same C# class.
    let emittedTopLevelNames (types: GeneratedType list) (functions: GeneratedFunction list) : Set<string> =
        let colliding =
            types
            |> List.choose (function
                | Union(name, cases) when cases |> List.exists (fun c -> c.Name = name) -> Some name
                | _ -> None)
            |> Set.ofList
        seq {
            for t in types do
                match t with
                | Record(name, _, _) -> yield name
                | Union(name, cases) ->
                    // Claim both the union name and its emitted base name: the
                    // latter is what fields are typed as, the former is what a
                    // foreign schema would reuse as a result type.
                    yield name
                    if colliding.Contains name then yield name + "Base"
                    for c in cases do yield c.Name
            for f in functions do yield f.Name
        }
        |> Set.ofSeq

    /// One mtproto declaration dropped by `mergeMtprotoForCSharp`.
    type MtprotoSkip = {
        /// TL name of the dropped declaration. A mapped type has no single TL
        /// name once constructors are grouped, so those report their C# name.
        Declaration: string
        /// The emitted C# name that was already taken.
        CSharpName: string
        /// Schema that owns that name: `api.tl` or `mtproto.tl`.
        Owner: string
    }

    /// Merge an mtproto schema into the api surface for the C# backend.
    ///
    /// `EmitCSharp.setup` hard-fails on a duplicate top-level name, and the two
    /// schemas overlap (`message`, `rpc_error`, `ping`, …). Policy: **api.tl
    /// wins** — an mtproto declaration whose emitted C# name (or result-type /
    /// union name) is already claimed by api.tl is dropped here so a collision
    /// can never reach the emitter.
    ///
    /// mtproto also collides with *itself* (`rpc_drop_answer` is both a function
    /// and, boxed, the `RpcDropAnswer` union). There the type wins: the emitted
    /// return-type map points functions at their result union, so dropping the
    /// union would leave a dangling reference, while dropping the request class
    /// leaves nothing pointing at it.
    ///
    /// Returns the merged surface plus one `MtprotoSkip` per dropped
    /// declaration, for the caller to log.
    let mergeMtprotoForCSharp
        (apiSchema: TlSchema)
        (mtSchema: TlSchema)
        : GeneratedType list * GeneratedFunction list * MtprotoSkip list =

        let apiTypes, apiFunctions = mapSchema apiSchema
        let claimed = emittedTopLevelNames apiTypes apiFunctions
        let skipped = ResizeArray<MtprotoSkip>()

        // A name outside the api claim set can only have been taken by an
        // earlier mtproto declaration.
        let ownerOf (csharpName: string) =
            if claimed.Contains csharpName then "api.tl" else "mtproto.tl"

        let skip (declaration: string) (csharpName: string) =
            skipped.Add
                { Declaration = declaration
                  CSharpName = csharpName
                  Owner = ownerOf csharpName }

        // Predict each mtproto constructor's emitted names the way `mapSchema`
        // groups them: sole constructor of a result type → a record named after
        // the result type; otherwise a case named after the combinator, under
        // the result-type base.
        let groupSizes =
            mtSchema.Constructors
            |> List.countBy (fun c -> CodeModelMapping.getResultTypeName c.ResultType)
            |> Map.ofList

        let keepConstructor (c: TlCombinator) =
            let resultName = CodeModelMapping.getResultTypeName c.ResultType
            let candidates =
                if groupSizes[resultName] = 1 then [ resultName ]
                else [ resultName; Combinator.pascalName c ]
            match candidates |> List.tryFind claimed.Contains with
            | Some hit ->
                skip (Combinator.tlName c) hit
                false
            | None -> true

        let keepFunction (f: TlCombinator) =
            let name = Combinator.pascalName f
            if claimed.Contains name then
                skip (Combinator.tlName f) name
                false
            else
                true

        let survivingFunctions = mtSchema.Functions |> List.filter keepFunction

        let mtTypes, mtFunctions =
            mapSchema
                { mtSchema with
                    Constructors = mtSchema.Constructors |> List.filter keepConstructor
                    Functions = survivingFunctions }

        // Second pass over the *mapped* surface. Dropping cases can rename a
        // declaration (a union reduced to one case becomes a record), and
        // mtproto declarations can also collide with each other, so the
        // prediction above is not the last word. A HashSet rather than a
        // `let mutable` because the accumulator is captured by the filters.
        let running = System.Collections.Generic.HashSet<string>(claimed)

        let tlNameOfFunction =
            survivingFunctions
            |> List.map (fun f -> Combinator.pascalName f, Combinator.tlName f)
            |> Map.ofList

        let acceptType (t: GeneratedType) =
            let names = emittedTopLevelNames [ t ] []
            match names |> Seq.tryFind running.Contains with
            | Some hit ->
                skip (typeName t) hit
                false
            | None ->
                running.UnionWith names
                true

        let acceptFunction (f: GeneratedFunction) =
            if running.Add f.Name then
                true
            else
                skip (defaultArg (tlNameOfFunction.TryFind f.Name) f.Name) f.Name
                false

        // Types before functions: see the `rpc_drop_answer` note above.
        let mergedTypes = apiTypes @ (mtTypes |> List.filter acceptType)
        let mergedFunctions = apiFunctions @ (mtFunctions |> List.filter acceptFunction)
        (mergedTypes, mergedFunctions, List.ofSeq skipped)

    module Whitelist =

        let private primitives = Set.ofList [
            "int32"; "int64"; "double"; "bool"; "string"; "byte[]"; "obj"
        ]

        let extractReferencedTypeName (fsharpType: string) : string option =
            let mutable t = fsharpType
            if t.EndsWith(" option") then t <- t.Substring(0, t.Length - 7)
            if t.EndsWith(" array") then t <- t.Substring(0, t.Length - 6)
            if primitives.Contains t then None else Some t

        let resolve
            (allTypes: Map<string, GeneratedType>)
            (allFunctions: Map<string, GeneratedFunction>)
            (seeds: Set<string>)
            (stubs: Set<string>)
            : Set<string> =

            let mutable visited = Set.empty
            let queue = System.Collections.Generic.Queue<string>(seeds)

            let extractDeps (fields: GeneratedField list) =
                fields |> List.choose (fun f -> extractReferencedTypeName f.FSharpType)

            while queue.Count > 0 do
                let name = queue.Dequeue()
                if not (visited.Contains name) && not (stubs.Contains name) then
                    visited <- visited.Add name
                    let deps =
                        match allTypes |> Map.tryFind name with
                        | Some(Record(_, fields, _)) -> extractDeps fields
                        | Some(Union(_, cases)) -> cases |> List.collect (fun c -> extractDeps c.Fields)
                        | None ->
                            match allFunctions |> Map.tryFind name with
                            | Some f -> extractDeps f.Params
                            | None -> []
                    for dep in deps do
                        if not (visited.Contains dep) && not (stubs.Contains dep) then
                            queue.Enqueue dep

            visited

    let private topSortTypes (types: GeneratedType list) : GeneratedType list =
        let nameOf = function Record(n,_,_) -> n | Union(n,_) -> n
        let depsOf t =
            let fieldsOf = function
                | Record(_, f, _) -> f
                | Union(_, cases) -> cases |> List.collect (fun c -> c.Fields)
            fieldsOf t
            |> List.choose (fun f -> Whitelist.extractReferencedTypeName f.FSharpType)
        TopSort.sort nameOf depsOf types

    let mapSchemaWhitelisted
        (schema: TlSchema)
        (whitelist: Set<string>)
        (stubs: Set<string>)
        (aliasMap: Map<string, uint32 list>)
        : GeneratedType list * GeneratedFunction list =

        let types, functions = mapSchema schema

        let allTypes =
            types |> List.map (fun t ->
                match t with
                | Record(name, _, _) -> name, t
                | Union(name, _) -> name, t
            ) |> Map.ofList

        let allFunctions =
            functions |> List.map (fun f -> f.Name, f) |> Map.ofList

        let resolvedNames = Whitelist.resolve allTypes allFunctions whitelist stubs

        let filteredTypes =
            types
            |> List.filter (fun t ->
                match t with
                | Record(name, _, _) -> resolvedNames.Contains name
                | Union(name, _) -> resolvedNames.Contains name)
            |> List.map (fun t ->
                match t with
                | Union(name, cases) ->
                    let augmentedCases = cases |> List.map (fun c ->
                        match aliasMap |> Map.tryFind c.Name with
                        | Some cids ->
                            let extras = cids |> List.filter (fun cid -> cid <> c.ConstructorId)
                            { c with AliasCids = extras }
                        | None -> c)
                    Union(name, augmentedCases)
                | other -> other)

        let rewriteStubField (f: GeneratedField) =
            let rec rewrite (t: string) =
                if t.EndsWith(" option") then
                    let inner = t.Substring(0, t.Length - 7)
                    $"%s{rewrite inner} option"
                elif t.EndsWith(" array") then
                    let inner = t.Substring(0, t.Length - 6)
                    $"%s{rewrite inner} array"
                elif t = "obj" then
                    // Unmapped TL type (typically a `!X` type variable on a
                    // polymorphic wrapper like invokeAfterMsg / invokeWithLayer).
                    // There's no generated type to (de)serialize, so treat it as
                    // opaque bytes — these wrappers are transport-level and never
                    // round-tripped as typed records.
                    "byte[]"
                else
                    let pascal = t.Substring(0, 1).ToUpperInvariant() + t.Substring(1)
                    if stubs.Contains pascal || stubs.Contains t then "byte[]" else t
            { f with FSharpType = rewrite f.FSharpType }

        let rewriteType = function
            | Record(n, fields, cid) -> Record(n, fields |> List.map rewriteStubField, cid)
            | Union(n, cases) -> Union(n, cases |> List.map (fun c -> { c with Fields = c.Fields |> List.map rewriteStubField }))

        let filteredTypes = filteredTypes |> List.map rewriteType

        let filteredFunctions =
            functions
            |> List.filter (fun f -> resolvedNames.Contains f.Name)
            |> List.map (fun f ->
                let withAliases =
                    match aliasMap |> Map.tryFind f.Name with
                    | Some cids ->
                        let extras = cids |> List.filter (fun cid -> cid <> f.ConstructorId)
                        { f with AliasCids = extras }
                    | None -> f
                { withAliases with Params = withAliases.Params |> List.map rewriteStubField })

        (topSortTypes filteredTypes, filteredFunctions)
