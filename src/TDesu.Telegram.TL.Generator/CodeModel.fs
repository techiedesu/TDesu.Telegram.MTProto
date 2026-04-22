namespace TDesu.Telegram.TL.Generator

open TDesu.Telegram.TL
open TDesu.Telegram.TL.AST

// ----------------------------------------------------------------
// Intermediate representation: TL schema mapped to F# type model
// ----------------------------------------------------------------

type GeneratedType =
    | Record of name: string * fields: GeneratedField list * constructorId: uint32
    | Union of name: string * cases: UnionCase list

and GeneratedField = {
    /// F#-camelCase name, used for DU positional labels, pattern bindings,
    /// local let bindings, and flag-field reference comparisons.
    /// Backtick-escaped if the original TL name collides with an F# keyword.
    Name: string
    /// F#-PascalCase record-field name. Used by the emitter at record
    /// declaration sites and for `value.Field` dot-access in generated
    /// code. Bindings + DU labels stay camelCase per F# convention.
    RecordName: string
    FSharpType: string
    IsOptional: bool
    FlagField: string option
    FlagBit: int option
    /// If Some(n), this field exists only at layer > n. The writer
    /// emits its write expression inside `if layer > n then ...`, and
    /// the struct exposes the field unconditionally (callers pass a
    /// value; it's dropped on the wire for layers ≤ n). Populated from
    /// `[[structural_overlays]]` in the overrides file; None for fields
    /// that come straight from the primary schema.
    LayerGate: int option
}

and UnionCase = {
    Name: string
    ConstructorId: uint32
    AliasCids: uint32 list
    Fields: GeneratedField list
}

type GeneratedFunction = {
    Name: string
    ConstructorId: uint32
    AliasCids: uint32 list
    Params: GeneratedField list
    ReturnType: string
}

// ----------------------------------------------------------------
// TL combinator accessors
// ----------------------------------------------------------------

[<RequireQualifiedAccess>]
module Combinator =

    let id (c: TlCombinator) =
        match c.ConstructorId with
        | Some(TlConstructorId id) -> id
        | None -> Crc32.computeForCombinator c

    let tlName (c: TlCombinator) =
        match c.Id.Namespace with
        | Some ns -> $"%s{ns}.%s{c.Id.Name}"
        | None -> c.Id.Name

    let pascalName (c: TlCombinator) =
        Naming.pascalCase (tlName c)

    let resultTypePascalName (expr: TlTypeExpr) =
        match expr with
        | TlTypeExpr.Bare id | TlTypeExpr.Boxed id ->
            match id.Namespace with
            | Some ns -> Naming.pascalCase $"%s{ns}.%s{id.Name}"
            | None -> Naming.pascalCase id.Name
        | _ -> "Unknown"

// ----------------------------------------------------------------
// Field helpers (shared by EmitTypes and EmitWriters)
// ----------------------------------------------------------------

module FieldHelpers =

    /// Raw flag-field names as they appear in the TL schema ("flags", "flags2").
    /// These survive verbatim through `f.FlagField` on dependent fields; do NOT
    /// case-convert here — callers use this string both to name local mutable
    /// bindings in generated code (`let mutable flags = 0`) and to compare
    /// against another field's FlagField reference.
    let flagFieldNames (fields: GeneratedField list) : string list =
        fields |> List.choose (fun f -> f.FlagField) |> List.distinct

    let isRawFlagField (flagFields: string list) (f: GeneratedField) =
        f.FSharpType = "int32" && flagFields |> List.contains f.Name

    let isPresenceFlag (f: GeneratedField) =
        f.FlagField.IsSome && f.FlagBit.IsSome && not f.IsOptional

    let dataFields (fields: GeneratedField list) =
        let ffs = flagFieldNames fields
        fields |> List.filter (fun f -> not (isRawFlagField ffs f))

    /// Map a TL type name ("int", "long", "string", "bytes") to its F# equivalent.
    /// Used by StructuralOverlay extras. Limited to wire-primitives; record/DU
    /// fields in overlays would require richer type resolution (punted).
    let private tlScalarToFSharp (t: string) =
        match t.Trim().ToLowerInvariant() with
        | "int" | "int32" | "nat" -> Some "int32"
        | "long" | "int64" -> Some "int64"
        | "double" -> Some "double"
        | "string" -> Some "string"
        | "bytes" -> Some "byte[]"
        | "bool" -> Some "bool"
        | _ -> None

    /// Splice extras from a [[structural_overlays]] entry into a field list.
    /// For each extra, find the field whose TL name matches `After` and insert
    /// the extra immediately after it. Extras carry `LayerGate = Some maxOld`
    /// so the writer wraps their writes in `if layer > maxOld then ...`.
    /// Unknown `After` anchors are logged and skipped (schema drift signal).
    ///
    /// TOML `after` values are snake_case (matching the TL wire name); GeneratedField.Name
    /// is camelCase (matching F# convention). Convert the lookup keys so the
    /// two meet.
    let applyStructuralExtras
        (maxOldLayer: int)
        (extras: (string * string * string) list) // (After, Name, TlType)
        (fields: GeneratedField list)
        : GeneratedField list =
        let byAfter =
            extras
            |> List.groupBy (fun (a, _, _) -> Naming.camelCase a)
            |> Map.ofList

        let buildExtra (_, name, tlType) : GeneratedField option =
            tlScalarToFSharp tlType
            |> Option.map (fun fsharp ->
                { Name = Naming.camelCase name
                  RecordName = Naming.pascalCase name
                  FSharpType = fsharp
                  IsOptional = false
                  FlagField = None
                  FlagBit = None
                  LayerGate = Some maxOldLayer })

        [
            for f in fields do
                yield f
                match Map.tryFind f.Name byAfter with
                | Some es ->
                    for e in es do
                        match buildExtra e with
                        | Some g -> yield g
                        | None -> () // unsupported type — caller must hotfix
                | None -> ()
        ]

// ----------------------------------------------------------------
// Generic topological sort
// ----------------------------------------------------------------

module TopSort =

    /// Sort items topologically. Breaks cycles by emitting remaining items.
    let sort (nameOf: 'a -> string) (depsOf: 'a -> string list) (items: 'a list) : 'a list =
        let allNames = items |> List.map nameOf |> Set.ofList

        let mutable remaining = items |> List.map (fun x -> nameOf x, x) |> Map.ofList
        let mutable emitted = Set.empty
        let mutable result = []

        while not remaining.IsEmpty do
            let next =
                remaining
                |> Map.toSeq
                |> Seq.tryFind (fun (_, x) ->
                    depsOf x
                    |> List.filter (fun d -> allNames.Contains d && d <> nameOf x)
                    |> List.forall emitted.Contains)
            match next with
            | Some(name, x) ->
                result <- x :: result
                emitted <- emitted.Add name
                remaining <- remaining.Remove name
            | None ->
                for KeyValue(_, x) in remaining do
                    result <- x :: result
                remaining <- Map.empty

        List.rev result

// ----------------------------------------------------------------
// TL schema → CodeModel mapping
// ----------------------------------------------------------------

module CodeModelMapping =

    let private pascalCase = Naming.pascalCase
    let private camelCase name = Naming.camelCase name |> Naming.escapeKeyword

    /// Types mapped to opaque raw bytes to avoid recursion/deep nesting in
    /// codegen.
    ///
    /// At the F# emit layer these become `byte[]` (same as the TL `bytes`
    /// primitive), but the generator marks them with the internal `rawBytes`
    /// sentinel so that **writers emit `WriteRawBytes`** (raw blob — the
    /// caller is expected to provide a complete pre-serialized TL value)
    /// instead of `WriteBytes` (length-prefixed TL `bytes` primitive). Read
    /// side can't structurally parse opaque refs, so `ReadBytes` stays —
    /// callers that need to deserialize one of these types must whitelist
    /// its constructors.
    let private opaqueTypes = set [
        "PageBlock"; "RichText"; "Page"
        "StoryItem"; "StoryViews"; "StoryFwdHeader"
        "MediaArea"
        "MessageExtendedMedia"
        "BotApp"; "BotInlineResult"; "BotInlineMessage"
        "SecureValueError"; "SecureValue"
    ]

    /// Internal sentinel for opaque-type-ref `byte[]` fields; the F# type
    /// emitted is still `byte[]` but the writer chooses `WriteRawBytes`
    /// instead of `WriteBytes`. Recognized in `mkSynType`, the writer/reader
    /// emit functions, and the writer-target's `isPrimitive` set.
    [<Literal>]
    let RawBytesSentinel = "rawBytes"

    let rec mapPrimitiveType (name: string) : string =
        match name.ToLowerInvariant() with
        | "int" -> "int32"
        | "long" -> "int64"
        | "double" -> "double"
        | "int128" -> "byte[]"
        | "int256" -> "byte[]"
        | "string" -> "string"
        | "bytes" -> "byte[]"
        | "bool" -> "bool"
        | "true" -> "bool"
        | "#" -> "int32"
        | "object" | "!x" -> "obj"
        | _ ->
            let pc = pascalCase name
            if opaqueTypes.Contains pc then RawBytesSentinel else pc

    let rec private mapTypeExpr (expr: TlTypeExpr) : string =
        match expr with
        | TlTypeExpr.Bare id -> mapIdentType id
        | TlTypeExpr.Boxed id -> mapIdentType id
        | TlTypeExpr.TypeVar _ -> "obj"
        | TlTypeExpr.Vector inner -> $"%s{mapTypeExpr inner} array"
        | TlTypeExpr.Nat -> "int32"
        | TlTypeExpr.Conditional(_, _, inner) -> mapTypeExpr inner

    and private mapIdentType (id: TlIdentifier) : string =
        let fullName =
            match id.Namespace with
            | Some ns -> $"%s{ns}.%s{id.Name}"
            | None -> id.Name
        mapPrimitiveType fullName

    let private recordName (name: string) = Naming.pascalCase name

    let mapParam (p: TlParam) : GeneratedField =
        let pascalName = recordName p.Name
        let name = camelCase p.Name
        match p.Type with
        | TlTypeExpr.Conditional(fieldRef, bitIndex, innerType) ->
            let innerTypeStr = mapTypeExpr innerType
            let isPresenceFlag =
                match innerType with
                | TlTypeExpr.Bare id when id.Name.ToLowerInvariant() = "true" -> true
                | _ -> false
            {
                Name = name
                RecordName = pascalName
                FSharpType = if isPresenceFlag then "bool" else $"%s{innerTypeStr} option"
                IsOptional = not isPresenceFlag
                FlagField = Some fieldRef
                FlagBit = Some bitIndex
                LayerGate = None
            }
        | TlTypeExpr.Nat ->
            { Name = name; RecordName = pascalName; FSharpType = "int32"; IsOptional = false; FlagField = None; FlagBit = None; LayerGate = None }
        | _ ->
            { Name = name; RecordName = pascalName; FSharpType = mapTypeExpr p.Type; IsOptional = false; FlagField = None; FlagBit = None; LayerGate = None }

    let internal getResultTypeName (expr: TlTypeExpr) : string =
        match expr with
        | TlTypeExpr.Bare id | TlTypeExpr.Boxed id ->
            match id.Namespace with
            | Some ns -> pascalCase $"%s{ns}.%s{id.Name}"
            | None -> pascalCase id.Name
        | TlTypeExpr.Vector inner -> $"%s{mapTypeExpr inner} array"
        | _ -> mapTypeExpr expr

    let internal mapTypeExprPublic = mapTypeExpr
