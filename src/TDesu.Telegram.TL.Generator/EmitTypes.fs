namespace TDesu.Telegram.TL.Generator

open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Text
open TDesu.Telegram.TL.Generator.Fantomas.Extensions
open TDesu.Telegram.TL.Generator.Fantomas.ExprBuilder
open TDesu.Telegram.TL.Generator.Fantomas.TypeBuilder
/// Emits F# serialization types (records/unions with Serialize/Deserialize) via Fantomas AST.
module EmitTypes =

    let private r = Range.Zero
    let private writer = mkIdent "writer"
    let private reader = mkIdent "reader"
    let private value = mkIdent "value"

    // --- Serialize/Deserialize expression builders ---

    let rec private serializeExprFor (fsharpType: string) (valueExpr: SynExpr) : SynExpr =
        match fsharpType with
        | "int32" -> mkApp (mkDotGet writer "WriteInt32") (mkParen valueExpr)
        | "int64" -> mkApp (mkDotGet writer "WriteInt64") (mkParen valueExpr)
        | "double" -> mkApp (mkDotGet writer "WriteDouble") (mkParen valueExpr)
        | "bool" -> mkApp (mkDotGet writer "WriteBool") (mkParen valueExpr)
        | "string" -> mkApp (mkDotGet writer "WriteString") (mkParen valueExpr)
        | "byte[]" -> mkApp (mkDotGet writer "WriteBytes") (mkParen valueExpr)
        // Opaque type ref — caller provides a complete pre-serialized TL
        // value; write it raw without the bytes-primitive length prefix.
        | "rawBytes" -> mkApp (mkDotGet writer "WriteRawBytes") (mkParen valueExpr)
        | t when t.EndsWith(" option") ->
            let inner = t[.. t.Length - 8]
            let innerCall = serializeExprFor inner (mkIdent "v")

            mkMatch
                valueExpr
                [ mkMatchClause (mkPatLongIdent [ "Some" ] [ mkPatNamed "v" ]) innerCall
                  mkMatchClause (mkPatLongIdentSimple [ "None" ]) mkUnit ]
        | t when t.EndsWith(" array") ->
            let inner = t[.. t.Length - 7]
            let innerCall = serializeExprFor inner (mkIdent "item")
            let lambdaBody = mkLet "writer" (mkIdent "w") innerCall
            let lambda = mkLambda [ "w"; "item" ] lambdaBody

            // WriteVector takes a tuple `(items, writeItem)` — emit a single
            // tupled call, not a curried one.
            mkApp
                (mkDotGet writer "WriteVector")
                (mkParen (mkTuple [ valueExpr; lambda ]))
        | typeName ->
            mkApp
                (mkLongIdent [ typeName; "Serialize" ])
                (mkParen (mkTuple [ writer; valueExpr ]))

    let rec private deserializeExprFor (fsharpType: string) : SynExpr =
        match fsharpType with
        | "int32" -> mkApp (mkDotGet reader "ReadInt32") mkUnit
        | "int64" -> mkApp (mkDotGet reader "ReadInt64") mkUnit
        | "double" -> mkApp (mkDotGet reader "ReadDouble") mkUnit
        | "bool" -> mkApp (mkDotGet reader "ReadBool") mkUnit
        | "string" -> mkApp (mkDotGet reader "ReadString") mkUnit
        | "byte[]" -> mkApp (mkDotGet reader "ReadBytes") mkUnit
        // Opaque type ref — read side can't structurally parse it without
        // the schema; ReadBytes preserves prior behavior. Callers that need
        // structured access should whitelist the opaque type's constructors.
        | "rawBytes" -> mkApp (mkDotGet reader "ReadBytes") mkUnit
        | t when t.EndsWith(" array") ->
            let inner = t[.. t.Length - 7]
            let innerCall = deserializeExprFor inner
            let lambdaBody = mkLet "reader" (mkIdent "r") innerCall
            let lambda = mkLambda [ "r" ] lambdaBody

            mkApp (mkDotGet reader "ReadVector") (mkParen lambda)
        | typeName ->
            mkApp (mkLongIdent [ typeName; "Deserialize" ]) (mkParen reader)

    // --- Flag helpers ---

    let private flagFieldNames = FieldHelpers.flagFieldNames
    let private isRawFlagField = FieldHelpers.isRawFlagField

    /// Build flags computation: mutable var, set bits, write
    let private flagsComputationExprs (flagField: string) (fields: GeneratedField list) : SynExpr list =
        let setBitExprs =
            fields
            |> List.choose (fun f ->
                match f.FlagField, f.FlagBit with
                | Some ff, Some bit when ff = flagField ->
                    let condition =
                        if f.IsOptional then
                            mkDotGet (mkDotGet value f.RecordName) "IsSome"
                        else
                            mkDotGet value f.RecordName

                    let setBit =
                        mkSet
                            flagField
                            (mkInfixApp
                                "|||"
                                (mkIdent flagField)
                                (mkParen (mkInfixApp "<<<" (mkInt32 1) (mkInt32 bit))))

                    Some(mkIf condition setBit None)
                | _ -> None)

        let writeFlags = mkApp (mkDotGet writer "WriteInt32") (mkParen (mkIdent flagField))
        setBitExprs @ [ writeFlags ]

    /// Build a single field's serialize expression
    let private serializeFieldExpr (f: GeneratedField) : SynExpr option =
        match f.FlagField, f.FlagBit with
        | Some _, Some _ when f.IsOptional ->
            let innerType = f.FSharpType[.. f.FSharpType.Length - 8]
            let innerCall = serializeExprFor innerType (mkIdent "v")

            Some(
                mkMatch
                    (mkDotGet value f.RecordName)
                    [ mkMatchClause (mkPatLongIdent [ "Some" ] [ mkPatNamed "v" ]) innerCall
                      mkMatchClause (mkPatLongIdentSimple [ "None" ]) mkUnit ]
            )
        | Some _, Some _ ->
            // Presence flag (bool), no serialization needed
            None
        | _ ->
            let valueExpr = mkDotGet value f.RecordName
            Some(serializeExprFor f.FSharpType valueExpr)

    // --- Member builders ---

    let private mkSerializeMember (name: string) (fields: GeneratedField list) (ctorId: uint32) =
        let flagFields = flagFieldNames fields
        let writeCtorId = mkApp (mkDotGet writer "WriteConstructorId") (mkParen (mkUInt32 ctorId))

        let flagExprs =
            flagFields |> List.collect (fun ff -> flagsComputationExprs ff fields)

        let fieldExprs =
            fields
            |> List.filter (fun f -> not (isRawFlagField flagFields f))
            |> List.choose serializeFieldExpr

        let bodyParts = [ writeCtorId ] @ flagExprs @ fieldExprs

        // Wrap in mutable let for each flag field
        let body =
            let inner = mkSeq bodyParts

            (inner, flagFields |> List.rev)
            ||> List.fold (fun acc ff -> mkLetMutable ff (mkInt32 0) acc)

        let writerPat =
            mkPatTyped (mkPatNamed "writer") (mkSynType "TlWriteBuffer")

        let valuePat = mkPatTyped (mkPatNamed "value") (mkSynType name)

        mkStaticMemberWithReturnType
            "Serialize"
            [ SynPat.CreateTuple([ writerPat; valuePat ]) ]
            (Some(mkSynType "unit"))
            body

    let private mkDeserializeRecordBody
        (recordFields: GeneratedField list)
        (flagFields: string list)
        (readCid: bool)
        =
        // LHS is record-field label (PascalCase); RHS is the local binding
        // introduced by mkLet below (camelCase, matches f.Name).
        let buildRecordExpr =
            mkRecordExpr (
                recordFields
                |> List.map (fun f -> [ f.RecordName ], mkIdent f.Name)
            )

        // Build nested let chain from bottom up
        let withFieldReads =
            (buildRecordExpr, recordFields |> List.rev)
            ||> List.fold (fun acc f ->
                match f.FlagField, f.FlagBit with
                | Some ff, Some bit when f.IsOptional ->
                    let innerType = f.FSharpType[.. f.FSharpType.Length - 8]
                    let desCall = deserializeExprFor innerType

                    let condExpr =
                        mkInfixApp
                            "<>"
                            (mkParen (
                                mkInfixApp
                                    "&&&"
                                    (mkIdent ff)
                                    (mkParen (mkInfixApp "<<<" (mkInt32 1) (mkInt32 bit)))
                            ))
                            (mkInt32 0)

                    let rhs =
                        mkIf condExpr (mkApp (mkIdent "Some") (mkParen desCall)) (Some(mkIdent "None"))

                    mkLet f.Name rhs acc
                | Some ff, Some bit ->
                    let condExpr =
                        mkInfixApp
                            "<>"
                            (mkParen (
                                mkInfixApp
                                    "&&&"
                                    (mkIdent ff)
                                    (mkParen (mkInfixApp "<<<" (mkInt32 1) (mkInt32 bit)))
                            ))
                            (mkInt32 0)

                    mkLet f.Name condExpr acc
                | _ ->
                    let desCall = deserializeExprFor f.FSharpType
                    mkLet f.Name desCall acc)

        let withFlagReads =
            (withFieldReads, flagFields |> List.rev)
            ||> List.fold (fun acc ff ->
                mkLet ff (mkApp (mkDotGet reader "ReadInt32") mkUnit) acc)

        if readCid then
            mkLet "_cid" (mkApp (mkDotGet reader "ReadConstructorId") mkUnit) withFlagReads
        else
            withFlagReads

    let private mkDeserializeRecordMember (name: string) (fields: GeneratedField list) =
        let flagFields = flagFieldNames fields

        let recordFields =
            fields |> List.filter (fun f -> not (isRawFlagField flagFields f))

        let body = mkDeserializeRecordBody recordFields flagFields true

        let readerPat =
            mkPatTyped (mkPatNamed "reader") (mkSynType "TlReadBuffer")

        mkStaticMemberWithReturnType "Deserialize" [ readerPat ] (Some(mkSynType name)) body

    let private mkConstructorIdMember (ctorId: uint32) =
        mkStaticProperty "ConstructorId" (Some(mkSynType "uint32")) (mkUInt32 ctorId)

    /// Emit `static member AliasCids: uint32[] = [| ... |]` listing any
    /// additional wire CIDs that should dispatch to the same handler.
    /// Populated from `[[aliases]]` TOML entries. For types with no aliases
    /// the array is empty (callers can still read the member unconditionally).
    let private mkAliasCidsMember (aliasCids: uint32 list) =
        let arr = aliasCids |> List.map mkUInt32 |> mkArrayExpr
        mkStaticProperty "AliasCids" (Some(mkSynTypeArray (mkSynType "uint32"))) arr

    // --- Record type ---

    let buildRecordDecl (name: string) (fields: GeneratedField list) (constructorId: uint32) : SynTypeDefn =
        let flagFields = flagFieldNames fields

        let recordFields =
            fields |> List.filter (fun f -> not (isRawFlagField flagFields f))

        let synFields =
            recordFields
            |> List.map (fun f -> mkRecordField f.RecordName (mkSynType f.FSharpType))

        let members =
            [ mkConstructorIdMember constructorId
              mkSerializeMember name fields constructorId
              mkDeserializeRecordMember name fields ]

        mkRecordType
            name
            synFields
            members
            (SynTypeDefnLeadingKeyword.Type r)
            []

    // --- Union type ---

    let private mkUnionSerializeMember (name: string) (cases: UnionCase list) =
        let clauses =
            cases
            |> List.map (fun c ->
                let caseFields =
                    c.Fields
                    |> List.filter (fun f ->
                        not (isRawFlagField (flagFieldNames c.Fields) f))

                let pat =
                    if caseFields.IsEmpty then
                        mkPatLongIdentSimple [ c.Name ]
                    else
                        mkPatLongIdent [ c.Name ] (caseFields |> List.map (fun f -> mkPatNamed f.Name))

                let writeCtorId =
                    mkApp (mkDotGet writer "WriteConstructorId") (mkParen (mkUInt32 c.ConstructorId))

                let fieldWrites =
                    caseFields
                    |> List.map (fun f -> serializeExprFor f.FSharpType (mkIdent f.Name))

                let body = mkSeq (writeCtorId :: fieldWrites)
                mkMatchClause pat body)

        let body = mkMatch value clauses

        let writerPat =
            mkPatTyped (mkPatNamed "writer") (mkSynType "TlWriteBuffer")

        let valuePat = mkPatTyped (mkPatNamed "value") (mkSynType name)

        mkStaticMemberWithReturnType
            "Serialize"
            [ SynPat.CreateTuple([ writerPat; valuePat ]) ]
            (Some(mkSynType "unit"))
            body

    let private mkUnionDeserializeMember (name: string) (cases: UnionCase list) =
        let clauses =
            cases
            |> List.map (fun c ->
                let caseFields =
                    c.Fields
                    |> List.filter (fun f ->
                        not (isRawFlagField (flagFieldNames c.Fields) f))

                let allCids = c.ConstructorId :: c.AliasCids

                let cidPat =
                    mkPatOr (allCids |> List.map (fun cid -> mkPatConst (SynConst.UInt32 cid)))

                let caseFlags = flagFieldNames c.Fields

                let buildCaseExpr =
                    if caseFields.IsEmpty then
                        mkIdent c.Name
                    else
                        let args =
                            caseFields |> List.map (fun f -> mkIdent f.Name)

                        mkApp (mkIdent c.Name) (mkParen (mkTuple args))

                // Build nested let chain for field reads
                let withFieldReads =
                    (buildCaseExpr, caseFields |> List.rev)
                    ||> List.fold (fun acc f ->
                        match f.FlagField, f.FlagBit with
                        | Some ff, Some bit when f.IsOptional ->
                            let innerType = f.FSharpType[.. f.FSharpType.Length - 8]
                            let desCall = deserializeExprFor innerType

                            let condExpr =
                                mkInfixApp
                                    "<>"
                                    (mkParen (
                                        mkInfixApp
                                            "&&&"
                                            (mkIdent ff)
                                            (mkParen (mkInfixApp "<<<" (mkInt32 1) (mkInt32 bit)))
                                    ))
                                    (mkInt32 0)

                            let rhs =
                                mkIf
                                    condExpr
                                    (mkApp (mkIdent "Some") (mkParen desCall))
                                    (Some(mkIdent "None"))

                            mkLet f.Name rhs acc
                        | Some ff, Some bit ->
                            let condExpr =
                                mkInfixApp
                                    "<>"
                                    (mkParen (
                                        mkInfixApp
                                            "&&&"
                                            (mkIdent ff)
                                            (mkParen (mkInfixApp "<<<" (mkInt32 1) (mkInt32 bit)))
                                    ))
                                    (mkInt32 0)

                            mkLet f.Name condExpr acc
                        | _ ->
                            let desCall = deserializeExprFor f.FSharpType
                            mkLet f.Name desCall acc)

                let withFlagReads =
                    (withFieldReads, caseFlags |> List.rev)
                    ||> List.fold (fun acc ff ->
                        mkLet ff (mkApp (mkDotGet reader "ReadInt32") mkUnit) acc)

                mkMatchClause cidPat withFlagReads)

        // Add wildcard clause with failwith
        let failClause =
            let failExpr =
                mkApp
                    (mkIdent "failwith")
                    (mkParen
                        (mkInfixApp
                            "+"
                            (mkString $"Unknown constructor id for %s{name}: 0x")
                            (mkApp (mkDotGet (mkIdent "ctorId") "ToString") (mkParen (mkString "X08")))))

            mkMatchClause mkPatWild failExpr

        let body =
            mkLet
                "ctorId"
                (mkApp (mkDotGet reader "ReadConstructorId") mkUnit)
                (mkMatch (mkIdent "ctorId") (clauses @ [ failClause ]))

        let readerPat =
            mkPatTyped (mkPatNamed "reader") (mkSynType "TlReadBuffer")

        mkStaticMemberWithReturnType "Deserialize" [ readerPat ] (Some(mkSynType name)) body

    /// Generate inline field accessor members for a union type
    let private mkFieldAccessors (name: string) (cases: UnionCase list) : SynMemberDefn list =
        let caseRecordFields =
            cases
            |> List.map (fun c ->
                c.Name,
                c.Fields
                |> List.filter (fun f ->
                    not (isRawFlagField (flagFieldNames c.Fields) f)))

        // Collect all field names with their types, ordered by case size (biggest first)
        let allFieldNames =
            let casesBySize =
                caseRecordFields |> List.sortByDescending (fun (_, fields) -> fields.Length)

            casesBySize
            |> List.collect (fun (_, fields) -> fields |> List.map (fun f -> f.Name, f.FSharpType))
            |> List.distinctBy fst

        [ for fieldName, fieldType in allFieldNames do
              // Strip backticks if the F# field name is keyword-escaped (e.g. ``type``)
              // before pascal-casing — otherwise the accessor name comes out as
              // `get``type``` which is a syntax error.
              let unescapedName =
                  if fieldName.StartsWith("``") && fieldName.EndsWith("``") && fieldName.Length > 4 then
                      fieldName.Substring(2, fieldName.Length - 4)
                  else
                      fieldName

              let pascalName =
                  if unescapedName.Length > 0 then
                      string (System.Char.ToUpperInvariant(unescapedName[0])) + unescapedName[1..]
                  else
                      unescapedName

              let inAllCases =
                  caseRecordFields
                  |> List.forall (fun (_, fields) ->
                      fields
                      |> List.exists (fun f -> f.Name = fieldName && f.FSharpType = fieldType))

              let clauses =
                  caseRecordFields
                  |> List.map (fun (caseName, fields) ->
                      match
                          fields
                          |> List.tryFindIndex (fun f -> f.Name = fieldName && f.FSharpType = fieldType)
                      with
                      | Some idx ->
                          let pats =
                              fields
                              |> List.mapi (fun i f ->
                                  if i = idx then mkPatNamed f.Name else mkPatWild)

                          let pat = mkPatLongIdent [ caseName ] pats
                          let body =
                              if inAllCases then
                                  mkIdent fieldName
                              else
                                  mkApp (mkIdent "ValueSome") (mkIdent fieldName)

                          mkMatchClause pat body
                      | None ->
                          let pat =
                              if fields.IsEmpty then
                                  mkPatLongIdentSimple [ caseName ]
                              else
                                  mkPatLongIdent
                                      [ caseName ]
                                      (fields |> List.map (fun _ -> mkPatWild))

                          let body =
                              if inAllCases then
                                  mkIdent fieldName // should not happen
                              else
                                  mkIdent "ValueNone"

                          mkMatchClause pat body)

              let body = mkMatch (mkIdent "x") clauses

              let xPat = mkPatTyped (mkPatNamed "x") (mkSynType name)
              yield mkInlineStaticMember $"get%s{pascalName}" [ xPat ] body ]

    let buildUnionDecl (name: string) (cases: UnionCase list) : SynTypeDefn =
        let synCases =
            cases
            |> List.map (fun c ->
                let caseFields =
                    c.Fields
                    |> List.filter (fun f ->
                        not (isRawFlagField (flagFieldNames c.Fields) f))

                let fields =
                    caseFields
                    |> List.map (fun f ->
                        SynField.Create(
                            fieldType = mkSynType f.FSharpType,
                            idOpt = Ident.Create f.Name
                        ))

                mkUnionCase c.Name fields)

        let members =
            [ mkUnionSerializeMember name cases
              mkUnionDeserializeMember name cases ]
            @ mkFieldAccessors name cases

        mkUnionType name synCases members (SynTypeDefnLeadingKeyword.Type r) []

    // --- Function type ---

    let buildFunctionDecl (func: GeneratedFunction) : SynTypeDefn =
        let flagFields = flagFieldNames func.Params

        let recordFields =
            func.Params |> List.filter (fun f -> not (isRawFlagField flagFields f))

        let synFields =
            if recordFields.IsEmpty then
                [ mkRecordField "_placeholder" (mkSynType "unit") ]
            else
                recordFields
                |> List.map (fun f -> mkRecordField f.RecordName (mkSynType f.FSharpType))

        // DeserializeFields member (reads params without CID)
        let deserializeFieldsBody =
            if recordFields.IsEmpty && flagFields.IsEmpty then
                mkRecordExpr [ [ "_placeholder" ], mkUnit ]
            else
                mkDeserializeRecordBody recordFields flagFields false

        let deserializeFieldsMember =
            let readerPat =
                mkPatTyped (mkPatNamed "reader") (mkSynType "TlReadBuffer")

            mkStaticMemberWithReturnType
                "DeserializeFields"
                [ readerPat ]
                (Some(mkSynType func.Name))
                deserializeFieldsBody

        // Deserialize(body: byte[]) member
        let deserializeBody =
            mkUse
                "reader"
                (mkNew (mkSynType "TlReadBuffer") (mkParen (mkIdent "body")))
                (mkLet
                    "_cid"
                    (mkApp (mkDotGet reader "ReadConstructorId") mkUnit)
                    (mkApp (mkLongIdent [ func.Name; "DeserializeFields" ]) (mkParen reader)))

        let deserializeMember =
            let bodyPat =
                mkPatTyped (mkPatNamed "body") (mkSynTypeArray (mkSynType "byte"))

            mkStaticMemberWithReturnType
                "Deserialize"
                [ bodyPat ]
                (Some(mkSynType func.Name))
                deserializeBody

        let members =
            [ mkConstructorIdMember func.ConstructorId
              mkAliasCidsMember func.AliasCids
              mkSerializeMember func.Name func.Params func.ConstructorId
              deserializeFieldsMember
              deserializeMember ]

        mkRecordType func.Name synFields members (SynTypeDefnLeadingKeyword.Type r) []

    // --- Topo sort ---

    let private topoSortTypes (types: GeneratedType list) : GeneratedType list =
        let nameOf =
            function
            | Record(n, _, _) -> n
            | Union(n, _) -> n

        let depsOf t =
            let extractTypeRefs (fsharpType: string) =
                let mutable s = fsharpType

                if s.EndsWith(" option") then
                    s <- s[.. s.Length - 8]

                if s.EndsWith(" array") then
                    s <- s[.. s.Length - 7]

                if
                    s = "int32"
                    || s = "int64"
                    || s = "double"
                    || s = "bool"
                    || s = "string"
                    || s = "byte[]"
                    || s = "obj"
                    || s = "unit"
                then
                    []
                else
                    [ s ]

            match t with
            | Record(_, fields, _) -> fields |> List.collect (fun f -> extractTypeRefs f.FSharpType)
            | Union(_, cases) ->
                cases
                |> List.collect (fun c -> c.Fields |> List.collect (fun f -> extractTypeRefs f.FSharpType))

        let typeNames = types |> List.map nameOf |> Set.ofList

        let graph =
            types
            |> List.map (fun t -> nameOf t, depsOf t |> List.filter (fun d -> typeNames.Contains d))
            |> Map.ofList

        let mutable visited = Set.empty
        let mutable result = []

        let rec visit name =
            if not (visited.Contains name) then
                visited <- visited.Add name

                match graph.TryFind name with
                | Some deps ->
                    for d in deps do
                        visit d
                | None -> ()

                match types |> List.tryFind (fun t -> nameOf t = name) with
                | Some t -> result <- t :: result
                | None -> ()

        for t in types do
            visit (nameOf t)

        result |> List.rev

    // --- Format helpers ---

    let private runFormat (parsed: ParsedInput) : string =
        formatAst parsed |> Async.RunSynchronously

    let private formatSingleDecl (typeDefn: SynTypeDefn) : string =
        let decl = SynModuleDecl.Types([ typeDefn ], r)
        let ns = mkNamespace "Temp" [ decl ]
        let parsed = mkParsedInput [ ns ]
        let formatted = runFormat parsed
        // Strip "namespace Temp\n" or "namespace Temp\r\n" prefix + blank line
        let normalized = formatted.Replace("\r\n", "\n")
        let idx = normalized.IndexOf("\n\n")

        if idx >= 0 then
            normalized.Substring(idx + 2)
        else
            normalized

    // --- Public API ---

    let buildRecordCode (name: string) (fields: GeneratedField list) (constructorId: uint32) : string =
        buildRecordDecl name fields constructorId |> formatSingleDecl

    let buildUnionCode (name: string) (cases: UnionCase list) : string =
        buildUnionDecl name cases |> formatSingleDecl

    let buildFunctionCode (func: GeneratedFunction) : string =
        buildFunctionDecl func |> formatSingleDecl

    let buildModule
        (ns: string)
        (_moduleName: string)
        (types: GeneratedType list)
        (functions: GeneratedFunction list)
        : string =
        let typeDecls =
            topoSortTypes types
            |> List.map (fun t ->
                match t with
                | Record(name, fields, ctorId) -> buildRecordDecl name fields ctorId
                | Union(name, cases) -> buildUnionDecl name cases)

        let funcDecls = functions |> List.map buildFunctionDecl

        let allDecls = typeDecls @ funcDecls

        let moduleDecls =
            [ mkOpenDecl "TDesu.Serialization" ]
            @ (allDecls |> List.map (fun td -> SynModuleDecl.Types([ td ], r)))

        let nsNode = mkNamespace ns moduleDecls
        let parsed = mkParsedInput [ nsNode ]
        let formatted = runFormat parsed

        let header =
            $"// Auto-generated at %A{System.DateTimeOffset.Now}\n// Do not edit manually.\n\n"

        header + formatted
