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
                    Params = f.Params |> List.map CodeModelMapping.mapParam
                    ReturnType = CodeModelMapping.mapTypeExprPublic f.ResultType
                }
        ]

        (types, functions)

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
            |> List.map (fun f -> { f with Params = f.Params |> List.map rewriteStubField })

        (topSortTypes filteredTypes, filteredFunctions)
