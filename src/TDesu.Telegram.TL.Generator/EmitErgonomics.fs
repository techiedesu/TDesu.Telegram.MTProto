namespace TDesu.Telegram.TL.Generator

open System.Text
open Microsoft.Extensions.Logging
open TDesu.FSharp.Utilities
open TDesu.Telegram.TL.AST

/// Ergonomic surface over the generated types. Three emit patterns:
///
///  1. `static member Create` on records with `?arg` for every `option`
///     field and every presence-bool flag. Required = wire-mandatory
///     non-flag data fields. Consumer never types the None-boilerplate.
///
///  2. `static member Create<CaseSuffix>` on DUs, one per case. Same
///     required/optional split as records. Consumer types
///     `InputReplyTo.CreateMessage(replyToMsgId = 42)` instead of
///     `InputReplyTo.InputReplyToMessage(42, None, None, None, ...)`.
///
///  3. Struct active patterns for field extraction across DU cases.
///     For every (DU, fieldName, fsharpType) triple where the same
///     named field appears in ≥3 cases with a matching type, emit
///     `[<return: Struct>] let (|FieldName|_|) (u: DU) : Type voption`
///     that covers every case. Consumer writes
///     `match u with | Pts n -> ...`, no per-match allocation.
module EmitErgonomics =

    let private log = Logger.get "EmitErgonomics"

    // ── Naming helpers ──────────────────────────────────────────

    /// Strip the DU-name prefix from a case name to get a factory-suffix.
    /// `InputReplyToMessage` under `InputReplyTo` → `Message`.
    /// Falls back to the full case name if the case doesn't start with
    /// the DU prefix followed by an uppercase letter (rare — the tool
    /// usually preserves the prefix per TL convention).
    let private caseSuffix (duName: string) (caseName: string) : string =
        if caseName.StartsWith(duName, System.StringComparison.Ordinal)
           && caseName.Length > duName.Length
           && System.Char.IsUpper(caseName[duName.Length]) then
            caseName.Substring(duName.Length)
        else
            caseName

    // ── Field partition ─────────────────────────────────────────

    /// (required, optional). Required = non-optional, non-presence-flag,
    /// non-raw-flag data fields — mandatory on the wire.
    /// Optional = every `option`-typed field OR presence-bool flag
    /// (which defaults to `false` and is safe to omit).
    let private partitionFields (fields: GeneratedField list) =
        let data = FieldHelpers.dataFields fields
        let isOptCtor (f: GeneratedField) =
            f.IsOptional || FieldHelpers.isPresenceFlag f
        let required = data |> List.filter (fun f -> not (isOptCtor f))
        let optional = data |> List.filter isOptCtor
        required, optional

    // ── Emit helpers ────────────────────────────────────────────

    let private emitLine (sb: StringBuilder) (indent: int) (line: string) =
        for _ in 1 .. indent do
            sb.Append("    ") |> ignore
        sb.Append(line).Append('\n') |> ignore

    let private emitBlank (sb: StringBuilder) =
        sb.Append('\n') |> ignore

    /// Format an F# parameter binding for `Create` — always camelCase Name,
    /// keeps `?` prefix for optional. Type comes from `FSharpType` spelling.
    /// Presence-bool flags are exposed as `bool` (the record stores them
    /// as `bool`), not `bool option` — `defaultArg` unwraps at call.
    let private paramSig (isOptional: bool) (f: GeneratedField) : string =
        let ty =
            if isOptional && f.IsOptional then
                // Field type is already `T option`; strip for `?arg: T`.
                IrType.unoption (IrType.spelling f.FSharpType)
            else
                IrType.spelling f.FSharpType
        let prefix = if isOptional then "?" else ""
        $"{prefix}{f.Name}: {ty}"

    let private fieldInitLine
        (indent: int)
        (isOptional: bool)
        (f: GeneratedField)
        : string =
        let value =
            if not isOptional then
                f.Name
            elif f.IsOptional then
                // Field type is `T option`; caller passed `?f: T`, wrap back.
                f.Name
            else
                // Presence bool; caller passed `?f: bool`, unwrap with default.
                $"defaultArg {f.Name} false"
        String.replicate indent "    " + $"{f.RecordName} = {value}"

    // ── Record Create emit ──────────────────────────────────────

    /// Skip records that would produce a Create with no interesting
    /// ergonomic gain: zero fields (nothing to construct), or every
    /// field required (F# record construction already fine).
    let private worthCreateForRecord (fields: GeneratedField list) =
        let _, optional = partitionFields fields
        // Any option field or presence-bool → Create earns its keep.
        not optional.IsEmpty

    let private emitRecordCreate (sb: StringBuilder) (name: string) (fields: GeneratedField list) =
        let required, optional = partitionFields fields

        emitLine sb 1 $"type {name} with"
        emitLine sb 0 ""
        emitLine sb 2 $"/// Ergonomic constructor. Optional = TL `flags.N?T` fields + presence-bool flags."
        emitLine sb 2 "static member Create"
        emitLine sb 3 "("

        let allParams =
            (required |> List.map (fun f -> "    " + paramSig false f))
            @ (optional |> List.map (fun f -> "    " + paramSig true f))

        let paramLines =
            allParams
            |> List.mapi (fun i s ->
                if i < allParams.Length - 1 then s + "," else s)

        for line in paramLines do
            emitLine sb 3 line

        emitLine sb 3 $") : {name} ="
        emitLine sb 3 "{"

        // Data fields, IN ORDER (record init order matches record decl order).
        let dataOrdered = FieldHelpers.dataFields fields
        for f in dataOrdered do
            let isOpt = f.IsOptional || FieldHelpers.isPresenceFlag f
            emitLine sb 0 (fieldInitLine 4 isOpt f)

        emitLine sb 3 "}"
        emitLine sb 0 ""

    // ── DU per-case Create emit ─────────────────────────────────

    let private worthCreateForCase (fields: GeneratedField list) =
        // Emit even for cases with only required fields — consumer still
        // benefits from named-arg construction (no manual DU case name).
        // Skip only truly empty (nullary) cases; they're one-token literals.
        not (List.isEmpty fields)

    let private emitCaseCreate
        (sb: StringBuilder)
        (duName: string)
        (case: UnionCase)
        =
        let required, optional = partitionFields case.Fields
        let suffix = caseSuffix duName case.Name
        let memberName = "Create" + suffix

        emitLine sb 2 $"/// Construct `{duName}.{case.Name}`. Optional = TL `flags.N?T` + presence-bool flags."
        emitLine sb 2 $"static member {memberName}"
        emitLine sb 3 "("

        let allParams =
            (required |> List.map (fun f -> "    " + paramSig false f))
            @ (optional |> List.map (fun f -> "    " + paramSig true f))

        let paramLines =
            allParams
            |> List.mapi (fun i s ->
                if i < allParams.Length - 1 then s + "," else s)

        for line in paramLines do
            emitLine sb 3 line

        emitLine sb 3 $") : {duName} ="
        emitLine sb 3 $"{duName}.{case.Name}("

        // Named args in DECLARATION ORDER — matches the tool's DU case emit
        // (named-field union case).
        let dataOrdered = FieldHelpers.dataFields case.Fields

        let argLines =
            dataOrdered
            |> List.mapi (fun i f ->
                let isOpt = f.IsOptional || FieldHelpers.isPresenceFlag f
                let rhs =
                    if not isOpt then f.Name
                    elif f.IsOptional then f.Name
                    else $"defaultArg {f.Name} false"
                let comma = if i < dataOrdered.Length - 1 then "," else ""
                $"{f.Name} = {rhs}{comma}")

        for line in argLines do
            emitLine sb 5 line

        emitLine sb 3 ")"
        emitLine sb 0 ""

    let private emitDuCreates (sb: StringBuilder) (name: string) (cases: UnionCase list) =
        // Emit at least ONE Create<Suffix> for interesting cases.
        let interesting =
            cases |> List.filter (fun c -> worthCreateForCase c.Fields)

        if not interesting.IsEmpty then
            emitLine sb 1 $"type {name} with"
            emitLine sb 0 ""

            for case in interesting do
                emitCaseCreate sb name case

    // ── Field-extractor active patterns ─────────────────────────

    /// Emit `[<return: Struct>] let (|FieldName|_|) (u: DU) : Type voption`
    /// for every (DU, fieldName, fsharpType) where the same named field
    /// appears in ≥ MinCases cases with a matching type.
    [<Literal>]
    let private MinCasesForPattern = 3

    /// Fields whose auto-emit would shadow a common F# identifier or a
    /// well-known active-pattern name from FSharp.Core. Keeping the list
    /// tight — the alternative (`emit + collision at consumer's build`)
    /// only surfaces the problem after regen.
    let private reservedPatternNames =
        Set.ofList [
            "None"; "Some"; "ValueNone"; "ValueSome"; "Empty"
            "Error"; "Ok"; "Choice1Of2"; "Choice2Of2"
        ]

    let private patternName (fieldName: string) : string =
        Naming.pascalCase fieldName

    let private emitDuActivePatterns (sb: StringBuilder) (name: string) (cases: UnionCase list) =
        // Group case-field occurrences by (fieldName, fsharpType).
        let occurrences =
            [
                for case in cases do
                    for f in FieldHelpers.dataFields case.Fields do
                        yield f.Name, f.FSharpType, case
            ]
            |> List.groupBy (fun (n, t, _) -> n, t)
            |> List.map (fun ((n, t), grp) -> n, t, grp |> List.map (fun (_, _, c) -> c))

        let candidates =
            occurrences
            |> List.filter (fun (_, _, cs) -> List.length cs >= MinCasesForPattern)
            |> List.filter (fun (n, _, _) ->
                let pn = patternName n
                not (reservedPatternNames.Contains pn))
            // Sort by field name for stable emit order.
            |> List.sortBy (fun (n, _, _) -> n)

        for (fieldName, fsharpType, casesWithField) in candidates do
            let pn = patternName fieldName
            let tySpelling = IrType.spelling fsharpType

            emitLine sb 1 $"/// Extract `{fieldName}` from any `{name}` case that carries it."
            emitLine sb 1 $"/// Covers {List.length casesWithField} case(s)."
            emitLine sb 1 "[<return: Struct>]"
            emitLine sb 1 $"let (|{pn}|_|) (u: {name}) : {tySpelling} voption ="
            emitLine sb 2 "match u with"

            let orderedCases =
                casesWithField |> List.sortBy (fun c -> c.Name)

            for i, case in List.indexed orderedCases do
                let sep = if i < orderedCases.Length - 1 then "" else " -> ValueSome v"
                emitLine sb 2 $"| {name}.{case.Name}({fieldName} = v){sep}"

            emitLine sb 2 "| _ -> ValueNone"
            emitLine sb 0 ""

    // ── Entry point ─────────────────────────────────────────────

    /// Generate the ergonomics file. `ns` is the TARGET namespace where
    /// the types live (usually `<runtimeNs>.Requests`); ergonomics goes
    /// into a sibling module in the same namespace.
    let generate
        (ns: string)
        (types: GeneratedType list)
        (functions: GeneratedFunction list)
        : string =

        let sb = StringBuilder()

        sb.Append(Managed.banner "dotnet fsi tools/regen-tl.fsx (or td-tl-gen --target ergonomics)")
        |> ignore

        sb.Append($"namespace {ns}\n\n") |> ignore
        sb.Append($"open TDesu.Serialization\n\n") |> ignore

        // ---- Ergonomics module: Create factories ----
        sb.Append("/// Create factories for records and DU cases. Optional args (`?arg`)\n") |> ignore
        sb.Append("/// mirror TL `flags.N?T` fields and presence-bool flags.\n") |> ignore
        sb.Append("module Ergonomics =\n\n") |> ignore

        let mutable createdCount = 0

        // Records: types emitted with a curly-brace shape.
        let records =
            types
            |> List.choose (fun t ->
                match t with
                | Record(name, fields, _) -> Some(name, fields)
                | Union _ -> None)
            |> List.sortBy fst

        for (name, fields) in records do
            if worthCreateForRecord fields then
                emitRecordCreate sb name fields
                createdCount <- createdCount + 1

        // Function-request types are ALSO emitted as records (record shape
        // `type <FuncName> = { <Params...> }`). They live in `functions`,
        // not `types` — emit Create for them too so every request has one.
        let functionRecords =
            functions
            |> List.map (fun fn -> fn.Name, fn.Params)
            |> List.sortBy fst

        for (name, fields) in functionRecords do
            if worthCreateForRecord fields then
                emitRecordCreate sb name fields
                createdCount <- createdCount + 1

        // Unions get per-case Create<Suffix> factories.
        let unions =
            types
            |> List.choose (fun t ->
                match t with
                | Union(name, cases) -> Some(name, cases)
                | Record _ -> None)
            |> List.sortBy fst
        let mutable duCreatedCount = 0

        for (name, cases) in unions do
            let interesting =
                cases |> List.filter (fun c -> worthCreateForCase c.Fields)
            if not interesting.IsEmpty then
                emitDuCreates sb name cases
                duCreatedCount <- duCreatedCount + List.length interesting

        // ---- Auto-open Patterns module: field extractors ----
        emitBlank sb
        sb.Append("/// Struct field-extractor active patterns across DU cases.\n") |> ignore
        sb.Append("/// `[<return: Struct>]` → `voption`, zero heap allocation per match.\n") |> ignore
        sb.Append("[<AutoOpen>]\n") |> ignore
        sb.Append("module Patterns =\n\n") |> ignore

        let mutable patternCount = 0
        let sbBefore = sb.Length

        for (name, cases) in unions do
            let sbAt = sb.Length
            emitDuActivePatterns sb name cases
            if sb.Length > sbAt then
                patternCount <- patternCount + 1

        if sb.Length = sbBefore then
            // No union produced any pattern (thresholds not met). Emit a
            // benign no-op so the module compiles.
            emitLine sb 1 "// No auto-emit patterns produced under the current thresholds."
            emitLine sb 1 "let private _keepModuleNonEmpty = ()"

        log.LogInformation(
            "Ergonomics: {Records} record Create members, {DuCases} DU case factories, {Patterns} field-extractor patterns",
            createdCount,
            duCreatedCount,
            patternCount)

        sb.ToString()
