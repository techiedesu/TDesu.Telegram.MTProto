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

    let private primitives = set [ "int32"; "int64"; "double"; "bool"; "string"; "byte[]"; "rawBytes" ]
    let private isPrimitive t = primitives.Contains t

    let private flagFieldNames = FieldHelpers.flagFieldNames
    let private isFlagInt (f: GeneratedField) (ffs: string list) = FieldHelpers.isRawFlagField ffs f
    let private isPresenceFlag = FieldHelpers.isPresenceFlag
    let private dataFields = FieldHelpers.dataFields

    // ----------------------------------------------------------------
    // Shared-flag-bit bundling
    // ----------------------------------------------------------------

    /// A field or a bundle of fields that share the same flags.N? bit.
    /// Bundles are emitted as a single `(T1 * T2 * ...) option` in the DU case.
    type private FieldOrBundle =
        | Single of field: GeneratedField * resolvedType: string
        | Bundle of name: string * items: (GeneratedField * string) list * flagField: string * bit: int

    /// Detect optional fields sharing `(flagField, bit)` and merge them into bundles.
    /// Presence-flag fields (bare `true` types) are excluded — they map to DU bools.
    let private bundleFields
        (fields: GeneratedField list)
        (resolver: GeneratedField -> string)
        : FieldOrBundle list =
        let sharedBits =
            fields
            |> List.choose (fun f ->
                match f.FlagField, f.FlagBit with
                | Some ff, Some bit when f.IsOptional && not (isPresenceFlag f) -> Some ((ff, bit), f)
                | _ -> None)
            |> List.groupBy fst
            |> List.filter (fun (_, group) -> group.Length > 1)
            |> List.map (fun (key, group) -> key, group |> List.map snd)
            |> Map.ofList
        let consumed = System.Collections.Generic.HashSet<string>()
        [
            for f in fields do
                if not (consumed.Contains f.Name) then
                    match f.FlagField, f.FlagBit with
                    | Some ff, Some bit when f.IsOptional && not (isPresenceFlag f) && sharedBits.ContainsKey (ff, bit) ->
                        let group = sharedBits[(ff, bit)]
                        for gf in group do consumed.Add gf.Name |> ignore
                        let items = group |> List.map (fun gf -> gf, resolver gf)
                        let name = group |> List.map (fun gf -> gf.Name) |> String.concat "And"
                        Bundle(name, items, ff, bit)
                    | _ ->
                        Single(f, resolver f)
        ]

    /// Strip ` option` suffix from a resolved type string.
    let private stripOption (t: string) =
        if t.EndsWith " option" then t[.. t.Length - 8] else t

    /// Build a name remap: original field name → bundle name, for shared-bit fields.
    let private buildNameRemap (bundled: FieldOrBundle list) : Map<string, string> =
        bundled
        |> List.collect (fun fb ->
            match fb with
            | Single _ -> []
            | Bundle(bName, items, _, _) ->
                items |> List.map (fun (f, _) -> f.Name, bName))
        |> Map.ofList

    /// For each non-presence optional data field, find a presence-flag (`bare true`)
    /// field that shares its (flagField, bit). Returns Map<dataFieldName, presenceField>.
    /// Used by buildFieldWriteExprs to emit `if p.<bool> then w.WriteX(defaultArg p.<data>
    /// <default>)` instead of `p.<data> |> Option.iter`, since the spec couples the two:
    /// presence of the int (or whatever data) is gated by the bool, not by the F# Option
    /// being Some.
    let private findSharedPresenceFlags (fields: GeneratedField list)
        : Map<string, GeneratedField> =
        let presenceByBit =
            fields
            |> List.choose (fun f ->
                match f.FlagField, f.FlagBit with
                | Some ff, Some bit when isPresenceFlag f -> Some ((ff, bit), f)
                | _ -> None)
            |> Map.ofList
        fields
        |> List.choose (fun f ->
            match f.FlagField, f.FlagBit with
            | Some ff, Some bit when f.IsOptional && not (isPresenceFlag f) ->
                Map.tryFind (ff, bit) presenceByBit |> Option.map (fun pf -> f.Name, pf)
            | _ -> None)
        |> Map.ofList

    /// Default-value AST for a primitive resolved type (without ` option` suffix).
    /// Used when a data field's bit is set by a paired presence flag and the F#
    /// Option is None — we still need to write SOMETHING because the wire format
    /// (driven by the bit) expects bytes. Returns None for unsupported types so
    /// the caller can fall back to the old Option.iter behaviour.
    let private defaultValueForPrimitive (resolvedType: string) : SynExpr option =
        match resolvedType with
        | "int32" | "int" -> Some (mkInt32 0)
        | "int64" -> Some (mkInt64 0L)
        | "string" -> Some (mkString "")
        | "double" -> Some (mkConst (SynConst.Double 0.0))
        | "byte[]" | "rawBytes" -> Some (mkApp (mkLongIdent ["Array"; "empty"]) mkUnit)
        | _ -> None

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
            // TL union/single ref not in the writers whitelist — caller is expected
            // to pass a complete pre-serialized TL blob (constructor id + payload).
            // Use the `rawBytes` sentinel so the writer emits `WriteRawBytes`; the
            // F# field type still surfaces as `byte[]`. Pre-2026-04-17 this fell
            // back to `"byte[]"` which routed through `WriteBytes` (length-prefixed
            // TL `bytes` primitive) and corrupted every wire encoding — clients
            // would read the 4-zero-byte length envelope as the next constructor
            // id. SedBot's `documentAttributeSticker.stickerset:InputStickerSet`
            // hit this; the symptom was Telethon
            // `TypeNotFoundError(constructor=0x00000000)` in messages.getAvailableReactions.
            else "rawBytes"

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
        // The `rawBytes` sentinel only steers the writer-call dispatch
        // (WriteRawBytes vs WriteBytes); the F# type spelling is always
        // `byte[]`. Collapse upfront so the sentinel can't leak into
        // composite spellings — particularly bundled tuple strings like
        // `(string * rawBytes array) option` which `toSynType` only
        // recurses on at the option/array suffixes, not at tuple
        // separators. Pre-2026-04-17 this produced
        // `_b1: rawBytes array` which the F# compiler then inferred as
        // `obj` and broke `for item in _b1`.
        let t = t.Replace("rawBytes", "byte[]")
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
            // Collapse the `rawBytes` opaque-type-ref sentinel back to `byte[]`
            // for F# emission. The distinction only matters in primitiveWriteExpr.
            let asFSharp = if t = "rawBytes" then "byte[]" else t
            SynLongIdent.CreateSingleIdent asFSharp

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
            // Opaque type ref — caller provides a complete pre-serialized TL
            // value, write raw without the bytes-primitive length prefix.
            | "rawBytes" -> "WriteRawBytes"
            | _ -> failwith $"Unknown primitive: %s{t}"
        mkApp (mkDotGet wExpr methodName) (mkParen valueExpr)

    let rec private innerWriteAstExpr (resolvedType: string) (valueExpr: SynExpr) (needsLayer: Set<string>) : SynExpr =
        if resolvedType.EndsWith(" array") then
            // Vector<T> for any T (primitive or composite). Pre-2026-04-17 only
            // the top-level required/optional field paths handled `T array`;
            // when a bundled `Vector<long>` (e.g. `chatFull.recent_requesters`)
            // reached this dispatch via the bundle-tuple destructure, the
            // type "int64 array" fell through to primitiveWriteExpr → "Unknown
            // primitive: int64 array". Now handled uniformly here so any
            // call site (top-level, bundle, future paths) emits correct
            // vector-header + foreach + per-item write.
            let inner = resolvedType[.. resolvedType.Length - 7]
            let itemExpr = innerWriteAstExpr inner (mkIdent "item") needsLayer
            mkSeq [
                mkApp (mkDotGet wExpr "WriteConstructorId") (mkParen (mkHexUInt32 0x1CB5C415u))
                mkApp (mkDotGet wExpr "WriteInt32") (mkParen (mkDotGet valueExpr "Length"))
                mkForEach "item" valueExpr itemExpr
            ]
        elif isPrimitive resolvedType then
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

    /// Build flag-computation expressions. `nameRemap` maps original field names
    /// to their bundle name when shared-bit fields have been merged into tuples.
    let private buildFlagConditions
        (flagField: string)
        (fields: GeneratedField list)
        (fieldAccess: string -> SynExpr)
        (nameRemap: Map<string, string>)
        : SynExpr list =
        let emittedBits = System.Collections.Generic.HashSet<int>()
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
                        if emittedBits.Add(bit) then
                            let accessName = nameRemap |> Map.tryFind f.Name |> Option.defaultValue f.Name
                            mkIf (mkDotGet (fieldAccess accessName) "IsSome") setExpr None
                    elif isPresenceFlag f then
                        if emittedBits.Add(bit) then
                            mkIf (fieldAccess f.Name) setExpr None
                | _ -> ()
        ]

    let private buildFieldWriteExprs
        (f: GeneratedField)
        (resolvedType: string)
        (fieldAccess: string -> SynExpr)
        (needsLayer: Set<string>)
        (sharedPresenceFlag: GeneratedField option)
        : SynExpr list =
        if isPresenceFlag f then
            []
        elif f.IsOptional then
            let innerResolved = resolvedType[.. resolvedType.Length - 8]
            if innerResolved.EndsWith(" array") then
                let arrayInner = innerResolved[.. innerResolved.Length - 7]
                let itemExpr = innerWriteAstExpr arrayInner (mkIdent "item") needsLayer
                let writeArr arrName =
                    mkSeq [
                        mkApp (mkDotGet wExpr "WriteConstructorId") (mkParen (mkHexUInt32 0x1CB5C415u))
                        mkApp (mkDotGet wExpr "WriteInt32") (mkParen (mkDotGet (mkIdent arrName) "Length"))
                        mkForEach "item" (mkIdent arrName) itemExpr
                    ]
                match sharedPresenceFlag with
                | Some pf ->
                    // Bit is set by the presence flag — write empty vector when
                    // bool is true and Option is None so the wire matches the bit.
                    [ mkIf (fieldAccess pf.Name)
                        (mkMatch (fieldAccess f.Name) [
                            mkMatchClause
                                (mkPatLongIdent [ "Some" ] [ mkPatNamed "arr" ])
                                (writeArr "arr")
                            mkMatchClause (mkPatLongIdentSimple [ "None" ]) (mkSeq [
                                mkApp (mkDotGet wExpr "WriteConstructorId") (mkParen (mkHexUInt32 0x1CB5C415u))
                                mkApp (mkDotGet wExpr "WriteInt32") (mkParen (mkInt32 0))
                            ])
                        ]) None ]
                | None ->
                    [ mkMatch (fieldAccess f.Name) [
                        mkMatchClause
                            (mkPatLongIdent [ "Some" ] [ mkPatNamed "arr" ])
                            (writeArr "arr")
                        mkMatchClause (mkPatLongIdentSimple [ "None" ]) mkUnit
                    ] ]
            else
                let wExprInner = innerWriteAstExpr innerResolved (mkIdent "v") needsLayer
                match sharedPresenceFlag, defaultValueForPrimitive innerResolved with
                | Some pf, Some defVal ->
                    // Couple to presence flag: when bool=true, write the int (or
                    // string/etc.) using the supplied Option value, defaulting to
                    // the primitive zero/empty when None. This keeps the wire
                    // format honest: the bit is set, so reader expects bytes.
                    let writeWithDefault =
                        let defaulted =
                            mkApp
                                (mkApp (mkIdent "defaultArg") (fieldAccess f.Name))
                                (mkParen defVal)
                        innerWriteAstExpr innerResolved (mkParen defaulted) needsLayer
                    [ mkIf (fieldAccess pf.Name) writeWithDefault None ]
                | _ ->
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

        // Per-case record name for a union case in `recordPerCaseUnions`.
        // - Default: `Write{TypeName}{CaseName}Params`.
        // - Smart suffixing: if `caseName` starts with `typeName` (which is
        //   the common case — e.g. type `Message` has cases `Message` and
        //   `MessageService`), the prefix is dropped: `MessageService` →
        //   `Service`. So `Write{TypeName}{Suffix}Params`. This keeps the
        //   single-case name `WriteMessageParams` stable across a single→
        //   multi-case migration: existing record-with callsites still
        //   type-check, only the union-case wrap becomes necessary at the
        //   point of consumption.
        let perCaseRecordName (typeName: string) (caseName: string) =
            if caseName = typeName then
                typeName
            elif caseName.StartsWith(typeName) then
                typeName + caseName.Substring(typeName.Length)
            else
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
                    // syntax instead of positional union construction. Empty
                    // cases (no data fields) get no record — they stay as a
                    // bare `| CaseName` in the union.
                    cs
                    |> List.choose (fun c ->
                        let caseName = combinatorPascalName c
                        let fields = c.Params |> List.map CodeModelMapping.mapParam
                        let dfs = dataFields fields
                        if dfs.IsEmpty then None
                        else Some(perCaseRecordName rt caseName, dfs))
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
                                let resolve f = resolveFieldType f.FSharpType unionResultTypes singleResultTypes
                                let bundled = bundleFields fields resolve
                                let synFields = [
                                    for fb in bundled do
                                        match fb with
                                        | Single(f, resolved) ->
                                            SynField.Create(fieldType = toSynType resolved, idOpt = Ident.Create f.Name)
                                        | Bundle(name, items, _, _) ->
                                            let inner = items |> List.map (fun (_, rt) -> stripOption rt) |> String.concat " * "
                                            SynField.Create(fieldType = toSynType $"(%s{inner}) option", idOpt = Ident.Create name)
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
                let resolve f = resolveFieldType f.FSharpType unionResultTypes singleResultTypes
                let bundled = bundleFields dFields resolve
                let fields = [
                    for fb in bundled do
                        match fb with
                        | Single(f, resolved) ->
                            mkRecordField f.RecordName (toSynType resolved)
                        | Bundle(bName, items, _, _) ->
                            let inner = items |> List.map (fun (_, rt) -> stripOption rt) |> String.concat " * "
                            mkRecordField bName (toSynType $"(%s{inner}) option")
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

                            let resolve f = resolveFieldType f.FSharpType unionResultTypes singleResultTypes
                            let bundled = bundleFields dfs resolve

                            // Bundle-aware pattern + field access.
                            // For isRecordPerCase, field access must use the
                            // PascalCase record label (f.RecordName) even
                            // though callers pass f.Name (camelCase).
                            let dfsRecordMap =
                                dfs
                                |> List.map (fun f -> f.Name, f.RecordName)
                                |> Map.ofList
                            let pat, fieldAccess =
                                if dfs.IsEmpty then
                                    mkPatLongIdentSimple [ $"Write%s{rt}"; caseName ],
                                    (fun name -> mkIdent name)
                                elif isRecordPerCase then
                                    let bindName = "p_"
                                    mkPatLongIdent [ $"Write%s{rt}"; caseName ] [ mkPatNamed bindName ],
                                    (fun name ->
                                        let recordName =
                                            Map.tryFind name dfsRecordMap |> Option.defaultValue name
                                        mkDotGet (mkIdent bindName) recordName)
                                else
                                    let patFields = [
                                        for fb in bundled do
                                            match fb with
                                            | Single(f, _) -> mkPatNamed f.Name
                                            | Bundle(bName, _, _, _) -> mkPatNamed bName
                                    ]
                                    mkPatLongIdent [ $"Write%s{rt}"; caseName ] patFields,
                                    (fun name -> mkIdent name)

                            let cidExpr = makeCidExpr c caseName

                            let nameRemap = buildNameRemap bundled
                            let flagGroups =
                                ffs |> List.map (fun ff -> ff, buildFlagConditions ff fields fieldAccess nameRemap)
                            let presenceMap = findSharedPresenceFlags fields

                            // Field write expressions — bundle-aware
                            let fieldExprs = [
                                for fb in bundled do
                                    match fb with
                                    | Single(f, _) ->
                                        if not (isFlagInt f ffs) then
                                            let resolved = resolve f
                                            let pf = Map.tryFind f.Name presenceMap
                                            yield! buildFieldWriteExprs f resolved fieldAccess needsLayerSet pf
                                    | Bundle(bName, items, _, _) ->
                                        // Destructure tuple option and write each component
                                        let destructNames = items |> List.mapi (fun i _ -> $"_b%d{i}")
                                        let writes = [
                                            for (_, rt), dName in List.zip items destructNames do
                                                let innerType = stripOption rt
                                                yield innerWriteAstExpr innerType (mkIdent dName) needsLayerSet
                                        ]
                                        let tuplePat =
                                            SynPat.CreateParen(SynPat.CreateTuple(destructNames |> List.map mkPatNamed))
                                        yield mkMatch (fieldAccess bName) [
                                            mkMatchClause
                                                (mkPatLongIdent [ "Some" ] [ tuplePat ])
                                                (mkSeq writes)
                                            mkMatchClause (mkPatLongIdentSimple [ "None" ]) mkUnit
                                        ]
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

                    // Map camelCase f.Name (what callers pass) to the
                    // PascalCase record-field label (f.RecordName).
                    // Positional mode ignores the map — local bindings
                    // keep camelCase.
                    let fieldRecordMap =
                        dfs
                        |> List.map (fun f -> f.Name, f.RecordName)
                        |> Map.ofList
                    let fieldAccess name =
                        if dfs.IsEmpty then mkIdent name
                        else
                            let recordName = Map.tryFind name fieldRecordMap |> Option.defaultValue name
                            mkDotGet pExpr recordName

                    let resolve f = resolveFieldType f.FSharpType unionResultTypes singleResultTypes
                    let bundled = bundleFields dfs resolve
                    let nameRemap = buildNameRemap bundled
                    let presenceMap = findSharedPresenceFlags fields

                    let flagGroups =
                        ffs |> List.map (fun ff -> ff, buildFlagConditions ff fields fieldAccess nameRemap)

                    let fieldExprs = [
                        for fb in bundled do
                            match fb with
                            | Single(f, _) ->
                                if not (isFlagInt f ffs) then
                                    let pf = Map.tryFind f.Name presenceMap
                                    yield! buildFieldWriteExprs f (resolve f) fieldAccess needsLayerSet pf
                            | Bundle(bName, items, _, _) ->
                                let destructNames = items |> List.mapi (fun i _ -> $"_b%d{i}")
                                let writes = [
                                    for (_, rt), dName in List.zip items destructNames do
                                        let innerType = stripOption rt
                                        if innerType.EndsWith(" array") then
                                            let arrayInner = innerType[.. innerType.Length - 7]
                                            let itemExpr = innerWriteAstExpr arrayInner (mkIdent "item") needsLayerSet
                                            yield mkApp (mkDotGet wExpr "WriteConstructorId") (mkParen (mkHexUInt32 0x1CB5C415u))
                                            yield mkApp (mkDotGet wExpr "WriteInt32") (mkParen (mkDotGet (mkIdent dName) "Length"))
                                            yield mkForEach "item" (mkIdent dName) itemExpr
                                        else
                                            yield innerWriteAstExpr innerType (mkIdent dName) needsLayerSet
                                ]
                                let tuplePat =
                                    SynPat.CreateParen(SynPat.CreateTuple(destructNames |> List.map mkPatNamed))
                                yield mkMatch (fieldAccess bName) [
                                    mkMatchClause
                                        (mkPatLongIdent [ "Some" ] [ tuplePat ])
                                        (mkSeq writes)
                                    mkMatchClause (mkPatLongIdentSimple [ "None" ]) mkUnit
                                ]
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
