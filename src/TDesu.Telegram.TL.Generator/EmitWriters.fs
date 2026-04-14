namespace TDesu.Telegram.TL.Generator

open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Text
open TDesu.Telegram.TL.AST
open TDesu.Telegram.TL.Generator.Overrides
open TDesu.Telegram.TL.Generator.Fantomas.Extensions
open TDesu.Telegram.TL.Generator.Fantomas.ExprBuilder
open TDesu.Telegram.TL.Generator.Fantomas.TypeBuilder

/// Emits standalone write{X} functions via Fantomas AST.ы
module EmitWriters =

    let private r = Range.Zero
    let private wExpr = mkIdent "w"
    let private pExpr = mkIdent "p"

    // ----------------------------------------------------------------
    // Helpers
    // ----------------------------------------------------------------

    let private pascalCase = Naming.pascalCase
    let private camelCase = Naming.camelCase
    let private combinatorTlName = Combinator.tlName
    let private combinatorPascalName = Combinator.pascalName
    let private getCombinatorId = Combinator.id
    let private getResultTypePascalName = Combinator.resultTypePascalName

    let private primitives = set [ "int32"; "int64"; "double"; "bool"; "string"; "byte[]" ]
    let private isPrimitive t = primitives.Contains t

    let private flagFieldNames = FieldHelpers.flagFieldNames
    let private isFlagInt (f: GeneratedField) (ffs: string list) = FieldHelpers.isRawFlagField ffs f
    let private isPresenceFlag = FieldHelpers.isPresenceFlag
    let private dataFields = FieldHelpers.dataFields

    // ----------------------------------------------------------------
    // Type resolution (unchanged)
    // ----------------------------------------------------------------

    let private resolveFieldType
        (fsharpType: string)
        (unionTypes: Set<string>)
        (singleTypes: Map<string, string>)
        : string =

        let hasOption = fsharpType.EndsWith(" option")
        let afterOption =
            if hasOption then fsharpType[.. fsharpType.Length - 8] else fsharpType
        let hasArray = afterOption.EndsWith(" array")
        let baseType =
            if hasArray then afterOption[.. afterOption.Length - 7] else afterOption

        let resolved =
            if isPrimitive baseType then baseType
            elif unionTypes.Contains baseType then $"Write%s{baseType}"
            elif singleTypes.ContainsKey baseType then $"Write%s{singleTypes[baseType]}Params"
            else "byte[]"

        let withArray = if hasArray then $"%s{resolved} array" else resolved
        if hasOption then $"%s{withArray} option" else withArray

    // ----------------------------------------------------------------
    // Layer dependency analysis (unchanged)
    // ----------------------------------------------------------------

    let private computeNeedsLayer
        (groups: (string * TlCombinator list) list)
        (layerTypes: Set<string>) =
        let mutable needsLayer = Set.empty<string>

        for rt, cs in groups do
            if cs |> List.exists (fun c -> layerTypes.Contains(combinatorTlName c)) then
                needsLayer <- needsLayer.Add rt

        let mutable changed = true
        while changed do
            changed <- false
            for rt, cs in groups do
                if not (needsLayer.Contains rt) then
                    let refs =
                        cs
                        |> List.collect (fun c -> c.Params |> List.map CodeModelMapping.mapParam)
                        |> List.choose (fun f ->
                            let t = f.FSharpType
                            let stripped =
                                let s1 = if t.EndsWith(" option") then t[.. t.Length - 8] else t
                                if s1.EndsWith(" array") then s1[.. s1.Length - 7] else s1
                            if isPrimitive stripped then None else Some (pascalCase stripped))
                    if refs |> List.exists needsLayer.Contains then
                        needsLayer <- needsLayer.Add rt
                        changed <- true
        needsLayer

    // ----------------------------------------------------------------
    // Topological sort for record types (unchanged)
    // ----------------------------------------------------------------

    let private topSortRecordTypes
        (records: (string * GeneratedField list) list)
        (allTypeNames: Set<string>)
        (unionTypes: Set<string>)
        (singleTypes: Map<string, string>) =
        let nameOf (name, _) = name
        let depsOf (_, fields: GeneratedField list) =
            fields
            |> List.choose (fun f ->
                let resolved = resolveFieldType f.FSharpType unionTypes singleTypes
                let stripped =
                    let s1 = if resolved.EndsWith(" option") then resolved[.. resolved.Length - 8] else resolved
                    if s1.EndsWith(" array") then s1[.. s1.Length - 7] else s1
                if stripped.StartsWith "Write" && stripped.EndsWith "Params" then
                    let name = stripped[5 .. stripped.Length - 7]
                    if allTypeNames.Contains name then Some name else None
                else None)
            |> Set.ofList

        let mutable remaining = records |> List.map (fun r -> nameOf r, r) |> Map.ofList
        let mutable emitted = Set.empty<string>
        let mutable result = []

        while not remaining.IsEmpty do
            let next =
                remaining
                |> Map.toSeq
                |> Seq.tryFind (fun (_, r) -> depsOf r |> Set.forall emitted.Contains)
            match next with
            | Some(name, r) ->
                result <- r :: result
                emitted <- emitted.Add name
                remaining <- remaining.Remove name
            | None ->
                for KeyValue(_, r) in remaining do
                    result <- r :: result
                remaining <- Map.empty

        List.rev result

    // ----------------------------------------------------------------
    // AST helpers: type string → SynType
    // ----------------------------------------------------------------

    let rec private toSynType (t: string) : SynType =
        if t.EndsWith(" option") then
            SynType.App(
                typeName = SynType.LongIdent(SynLongIdent.Create([ Ident.Create "option" ])),
                typeArgs = [ toSynType (t[.. t.Length - 8]) ],
                commaRanges = [],
                isPostfix = true,
                lessRange = None,
                greaterRange = None,
                range = r
            )
        elif t.EndsWith(" array") then
            mkSynTypeArray (toSynType (t[.. t.Length - 7]))
        else
            SynLongIdent.CreateSingleIdent t

    // ----------------------------------------------------------------
    // AST helpers: write expressions
    // ----------------------------------------------------------------

    let private mkHexUInt32 (v: uint32) =
        mkConst (SynConst.UserNum($"0x%08X{v}", "u"))

    let private primitiveWriteExpr (t: string) (valueExpr: SynExpr) : SynExpr =
        let methodName =
            match t with
            | "int32" -> "WriteInt32"
            | "int64" -> "WriteInt64"
            | "double" -> "WriteDouble"
            | "bool" -> "WriteBool"
            | "string" -> "WriteString"
            | "byte[]" -> "WriteBytes"
            | _ -> failwith $"Unknown primitive: %s{t}"
        mkApp (mkDotGet wExpr methodName) (mkParen valueExpr)

    let private innerWriteAstExpr (resolvedType: string) (valueExpr: SynExpr) (needsLayer: Set<string>) : SynExpr =
        if isPrimitive resolvedType then
            primitiveWriteExpr resolvedType valueExpr
        elif resolvedType.StartsWith "Write" then
            let name =
                if resolvedType.EndsWith "Params" then resolvedType[5 .. resolvedType.Length - 7]
                else resolvedType[5..]
            let fnExpr = mkIdent $"write%s{name}"
            let applied = mkApp fnExpr wExpr
            let withLayer = if needsLayer.Contains name then mkApp applied (mkIdent "layer") else applied
            mkApp withLayer valueExpr
        else
            primitiveWriteExpr resolvedType valueExpr

    let private buildFlagConditions
        (flagField: string)
        (fields: GeneratedField list)
        (fieldAccess: string -> SynExpr)
        : SynExpr list =
        [
            for f in fields do
                match f.FlagField, f.FlagBit with
                | Some ff, Some bit when ff = flagField ->
                    let setExpr =
                        mkSet flagField (
                            mkInfixApp "|||"
                                (mkIdent flagField)
                                (mkParen (mkInfixApp "<<<" (mkInt32 1) (mkInt32 bit)))
                        )
                    if f.IsOptional then
                        mkIf (mkDotGet (fieldAccess f.Name) "IsSome") setExpr None
                    elif isPresenceFlag f then
                        mkIf (fieldAccess f.Name) setExpr None
                | _ -> ()
        ]

    let private buildFieldWriteExprs
        (f: GeneratedField)
        (resolvedType: string)
        (fieldAccess: string -> SynExpr)
        (needsLayer: Set<string>)
        : SynExpr list =
        if isPresenceFlag f then
            []
        elif f.IsOptional then
            let innerResolved = resolvedType[.. resolvedType.Length - 8]
            if innerResolved.EndsWith(" array") then
                let arrayInner = innerResolved[.. innerResolved.Length - 7]
                let itemExpr = innerWriteAstExpr arrayInner (mkIdent "item") needsLayer
                [ mkMatch (fieldAccess f.Name) [
                    mkMatchClause
                        (mkPatLongIdent [ "Some" ] [ mkPatNamed "arr" ])
                        (mkSeq [
                            mkApp (mkDotGet wExpr "WriteConstructorId") (mkParen (mkHexUInt32 0x1CB5C415u))
                            mkApp (mkDotGet wExpr "WriteInt32") (mkParen (mkDotGet (mkIdent "arr") "Length"))
                            mkForEach "item" (mkIdent "arr") itemExpr
                        ])
                    mkMatchClause (mkPatLongIdentSimple [ "None" ]) mkUnit
                ] ]
            else
                let wExprInner = innerWriteAstExpr innerResolved (mkIdent "v") needsLayer
                [ mkInfixApp "|>"
                    (fieldAccess f.Name)
                    (mkApp (mkLongIdent [ "Option"; "iter" ]) (mkParen (mkLambda [ "v" ] wExprInner)))
                ]
        else
            if resolvedType.EndsWith(" array") then
                let arrayInner = resolvedType[.. resolvedType.Length - 7]
                let itemExpr = innerWriteAstExpr arrayInner (mkIdent "item") needsLayer
                [
                    mkApp (mkDotGet wExpr "WriteConstructorId") (mkParen (mkHexUInt32 0x1CB5C415u))
                    mkApp (mkDotGet wExpr "WriteInt32") (mkParen (mkDotGet (fieldAccess f.Name) "Length"))
                    mkForEach "item" (fieldAccess f.Name) itemExpr
                ]
            else
                [ innerWriteAstExpr resolvedType (fieldAccess f.Name) needsLayer ]

    /// Wrap body expressions with mutable flag declarations (from innermost to outermost).
    let private buildBodyWithFlags
        (cidExpr: SynExpr)
        (flagGroups: (string * SynExpr list) list)
        (fieldExprs: SynExpr list)
        : SynExpr =
        let innermost = fieldExprs
        let wrapped =
            flagGroups
            |> List.rev
            |> List.fold (fun innerExprs (ff, conditions) ->
                let writeFlag = mkApp (mkDotGet wExpr "WriteInt32") (mkParen (mkIdent ff))
                [ mkLetMutable ff (mkInt32 0) (mkSeq (conditions @ [ writeFlag ] @ innerExprs)) ]
            ) innermost
        mkSeq (cidExpr :: wrapped)

    // ----------------------------------------------------------------
    // AST helpers: typed pattern for function params
    // ----------------------------------------------------------------

    let private mkParenTypedPat (name: string) (typeName: string) =
        SynPat.CreateParen(mkPatTyped (mkPatNamed name) (SynLongIdent.CreateSingleIdent typeName))

    let private mkBinding
        (fnName: string)
        (pats: SynPat list)
        (body: SynExpr)
        (keyword: SynLeadingKeyword)
        : SynBinding =
        SynBinding.Create(
            kind = SynBindingKind.Normal,
            valData = SynValData.Create(),
            headPat =
                SynPat.CreateLongIdent(
                    longDotId = SynLongIdent.Create([ Ident.Create fnName ]),
                    argPats = SynArgPats.Pats pats
                ),
            expr = body,
            trivia = SynBindingTriviaHelper.create (keyword, Some r, None)
        )

    // ----------------------------------------------------------------
    // Main: generateWriterModule
    // ----------------------------------------------------------------

    let generateWriterModule
        (ns: string)
        (schema: TlSchema)
        (whitelist: Set<string>)
        (layerTypes: Set<string>)
        (layerVariants: LayerVariant list)
        (recordPerCaseUnions: Set<string>)
        : string =

        // ---- Filter & group ----
        let whitelisted =
            schema.Constructors
            |> List.filter (fun c -> whitelist.Contains(combinatorTlName c))

        let groups =
            whitelisted
            |> List.groupBy (fun c -> getResultTypePascalName c.ResultType)
            |> List.sortBy fst

        let unionResultTypes =
            groups
            |> List.choose (fun (rt, cs) -> if cs.Length > 1 then Some rt else None)
            |> Set.ofList

        let singleResultTypes =
            groups
            |> List.choose (fun (rt, cs) ->
                if cs.Length = 1 then Some(rt, combinatorPascalName cs.Head) else None)
            |> Map.ofList

        let layerVariantMap =
            layerVariants |> List.map (fun v -> v.Name, v) |> Map.ofList

        let needsLayerSet = computeNeedsLayer groups layerTypes

        // Per-case record name for a union case in `recordPerCaseUnions`:
        // `Write{TypeName}{CaseName}Params`. For a multi-case union *not* in
        // the set, cases stay positional (the historical default).
        let perCaseRecordName (typeName: string) (caseName: string) =
            $"%s{typeName}%s{caseName}"

        let recordTypesToEmit =
            groups
            |> List.collect (fun (rt, cs) ->
                if cs.Length = 1 then
                    let c = cs.Head
                    let name = combinatorPascalName c
                    let fields = c.Params |> List.map CodeModelMapping.mapParam
                    let dfs = dataFields fields
                    if dfs.IsEmpty then [] else [ name, dfs ]
                elif recordPerCaseUnions.Contains rt then
                    // Emit one record per case so callers can use record-with
                    // syntax instead of positional union construction.
                    cs
                    |> List.map (fun c ->
                        let caseName = combinatorPascalName c
                        let fields = c.Params |> List.map CodeModelMapping.mapParam
                        let dfs = dataFields fields
                        perCaseRecordName rt caseName, dfs)
                else [])

        let allRecordNames = recordTypesToEmit |> List.map fst |> Set.ofList

        let sortedRecords =
            topSortRecordTypes recordTypesToEmit allRecordNames unionResultTypes singleResultTypes

        // ---- Build type definitions ----
        let mutable isFirstType = true

        let typeDefns = [
            // DU types
            for rt, constructors in groups do
                if constructors.Length > 1 then
                    let keyword =
                        if isFirstType then
                            isFirstType <- false
                            SynTypeDefnLeadingKeyword.Type r
                        else
                            SynTypeDefnLeadingKeyword.And r
                    let isRecordPerCase = recordPerCaseUnions.Contains rt
                    let cases = [
                        for c in constructors do
                            let caseName = combinatorPascalName c
                            let fields = c.Params |> List.map CodeModelMapping.mapParam |> dataFields
                            if isRecordPerCase && not fields.IsEmpty then
                                // Reference the per-case record type (emitted
                                // alongside in `recordTypesToEmit`).
                                let recName = perCaseRecordName rt caseName
                                let synField =
                                    SynField.Create(
                                        fieldType = toSynType $"Write%s{recName}Params",
                                        idOpt = Ident.Create "value")
                                mkUnionCase caseName [ synField ]
                            else
                                let synFields = [
                                    for f in fields do
                                        let resolved = resolveFieldType f.FSharpType unionResultTypes singleResultTypes
                                        SynField.Create(fieldType = toSynType resolved, idOpt = Ident.Create f.Name)
                                ]
                                mkUnionCase caseName synFields
                    ]
                    mkUnionType $"Write%s{rt}" cases [] keyword [ mkRequireQualifiedAccessAttr () ]

            // Record types
            for name, dFields in sortedRecords do
                let keyword =
                    if isFirstType then
                        isFirstType <- false
                        SynTypeDefnLeadingKeyword.Type r
                    else
                        SynTypeDefnLeadingKeyword.And r
                let fields = [
                    for f in dFields do
                        let resolved = resolveFieldType f.FSharpType unionResultTypes singleResultTypes
                        mkRecordField f.Name (toSynType resolved)
                ]
                mkRecordType $"Write%s{name}Params" fields [] keyword []
        ]

        // ---- Build function bindings ----
        let mutable isFirstFn = true

        let makeCidExpr (c: TlCombinator) (name: string) =
            let isLayer = layerTypes.Contains(combinatorTlName c)
            if isLayer then
                let layerFn =
                    layerVariantMap
                    |> Map.tryPick (fun k v -> if k = name then Some(camelCase v.Name) else None)
                    |> Option.defaultValue (camelCase name)
                mkApp (mkDotGet wExpr "WriteConstructorId")
                    (mkParen (mkApp (mkLongIdent [ "GeneratedLayerCid"; layerFn ]) (mkIdent "layer")))
            else
                mkApp (mkDotGet wExpr "WriteConstructorId") (mkParen (mkHexUInt32 (getCombinatorId c)))

        let bindings = [
            // Union dispatch writers
            for rt, constructors in groups do
                if constructors.Length > 1 then
                    let keyword =
                        if isFirstFn then
                            isFirstFn <- false
                            SynLeadingKeyword.LetRec(r, r)
                        else
                            SynLeadingKeyword.And r

                    let anyNeedsLayer = needsLayerSet.Contains rt

                    let pats = [
                        mkParenTypedPat "w" "TlWriteBuffer"
                        if anyNeedsLayer then mkParenTypedPat "layer" "int"
                        mkParenTypedPat "p" $"Write%s{rt}"
                    ]

                    let isRecordPerCase = recordPerCaseUnions.Contains rt

                    let clauses = [
                        for c in constructors do
                            let caseName = combinatorPascalName c
                            let fields = c.Params |> List.map CodeModelMapping.mapParam
                            let ffs = flagFieldNames fields
                            let dfs = dataFields fields

                            let pat, fieldAccess =
                                if dfs.IsEmpty then
                                    mkPatLongIdentSimple [ $"Write%s{rt}"; caseName ],
                                    (fun name -> mkIdent name)
                                elif isRecordPerCase then
                                    // Bind the per-case record value as `p_`
                                    // and access fields via `p_.{name}`.
                                    let bindName = "p_"
                                    mkPatLongIdent [ $"Write%s{rt}"; caseName ] [ mkPatNamed bindName ],
                                    (fun name -> mkDotGet (mkIdent bindName) name)
                                else
                                    mkPatLongIdent
                                        [ $"Write%s{rt}"; caseName ]
                                        (dfs |> List.map (fun f -> mkPatNamed f.Name)),
                                    (fun name -> mkIdent name)

                            let cidExpr = makeCidExpr c caseName

                            let flagGroups =
                                ffs |> List.map (fun ff -> ff, buildFlagConditions ff fields fieldAccess)

                            let fieldExprs = [
                                for f in fields do
                                    if not (isFlagInt f ffs) then
                                        let resolved = resolveFieldType f.FSharpType unionResultTypes singleResultTypes
                                        yield! buildFieldWriteExprs f resolved fieldAccess needsLayerSet
                            ]

                            let body = buildBodyWithFlags cidExpr flagGroups fieldExprs
                            mkMatchClause pat body
                    ]

                    let body = mkMatch pExpr clauses
                    mkBinding $"write%s{rt}" pats body keyword

            // Standalone writers
            for _, constructors in groups do
                if constructors.Length = 1 then
                    let c = constructors.Head
                    let name = combinatorPascalName c
                    let fields = c.Params |> List.map CodeModelMapping.mapParam
                    let ffs = flagFieldNames fields
                    let dfs = dataFields fields

                    let resultNeedsLayer = needsLayerSet.Contains(getResultTypePascalName c.ResultType)
                    let fnNeedsLayer = layerTypes.Contains(combinatorTlName c) || resultNeedsLayer

                    let keyword =
                        if isFirstFn then
                            isFirstFn <- false
                            SynLeadingKeyword.LetRec(r, r)
                        else
                            SynLeadingKeyword.And r

                    let pats = [
                        mkParenTypedPat "w" "TlWriteBuffer"
                        if fnNeedsLayer then mkParenTypedPat "layer" "int"
                        if dfs.IsEmpty then SynPat.Const(SynConst.Unit, r)
                        else mkParenTypedPat "p" $"Write%s{name}Params"
                    ]

                    let cidExpr = makeCidExpr c name

                    let fieldAccess name =
                        if dfs.IsEmpty then mkIdent name
                        else mkDotGet pExpr name

                    let flagGroups =
                        ffs |> List.map (fun ff -> ff, buildFlagConditions ff fields fieldAccess)

                    let fieldExprs = [
                        for f in fields do
                            if not (isFlagInt f ffs) then
                                let resolved = resolveFieldType f.FSharpType unionResultTypes singleResultTypes
                                yield! buildFieldWriteExprs f resolved fieldAccess needsLayerSet
                    ]

                    let body = buildBodyWithFlags cidExpr flagGroups fieldExprs
                    mkBinding $"write%s{name}" pats body keyword
        ]

        // ---- Assemble AST ----
        let moduleDecls = [
            if typeDefns.Length > 0 then
                SynModuleDecl.Types(typeDefns, r)
            if bindings.Length > 0 then
                SynModuleDecl.Let(true, bindings, r)
        ]

        let nestedModule =
            SynModuleDecl.NestedModule(
                moduleInfo = SynComponentInfo.Create(longId = [ Ident.Create "GeneratedTlWriters" ]),
                isRecursive = false,
                decls = moduleDecls,
                isContinuing = false,
                range = r,
                trivia = { SynModuleDeclNestedModuleTrivia.ModuleKeyword = Some r; EqualsRange = Some r }
            )

        let nsNode = mkNamespace ns [ nestedModule ]
        let parsed = mkParsedInput [ nsNode ]
        let formatted = formatAst parsed |> Async.RunSynchronously

        let header =
            "// Auto-generated TL writer functions. Do not edit manually.\n"
            + "// Re-generate with: dotnet run --project src/MTProto.TL.Generator -- --writers\n\n"

        header + formatted
