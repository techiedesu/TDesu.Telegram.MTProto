namespace TDesu.Telegram.TL.Generator

open TDesu.Telegram.TL
open TDesu.Telegram.TL.AST

// ----------------------------------------------------------------
// Intermediate representation: TL schema mapped to F# type model
// ----------------------------------------------------------------

/// The IR's field-type vocabulary.
///
/// A field type is a **string**: one base token, optionally wrapped by a
/// vector suffix (` array` / ` barevector`) and then ` option`. That one
/// string does two jobs — it is the F#/C# type spelling *and* the wire
/// encoding — and both shipped encoding defects are the second job having no
/// channel of its own:
///
///   * **#116** — `int128` and `int256` collapsed to `byte[]`, which already
///     means the length-prefixed `bytes` primitive. A 16-byte nonce went out
///     with a TL length envelope in front of it and padding behind it.
///   * **#117** — `vector<T>` and `Vector<T>` collapsed to `T array`, and a
///     bare element to the same spelling as a boxed one, so `future_salts`
///     shipped a `0x1CB5C415` header and a per-element constructor id that a
///     real client reads as an element count of 482,092,053.
///
/// The width channel the audit asked for is `FixedBytes`; here it is a base
/// token (`Int128` / `Int256`) with `fixedWidth` as its accessor, because in
/// a string-typed IR a token *is* the node. A discriminated union would have
/// to replace `GeneratedField.FSharpType` outright, and that string is also
/// where the overrides file injects arbitrary consumer-supplied F# type
/// spellings (`stub_types`, structural-overlay extras) — a DU would need a
/// `Raw of string` escape hatch for them and would buy nothing over this.
///
/// The three wire-only markers below are **not** part of the type spelling:
/// `spelling` erases them and every site that only wants a type name calls
/// it; every site that emits a read or a write interprets them instead.
/// They are deliberately not valid C#/F# identifiers, so a marker that leaks
/// into emitted source is a hard error at the Roslyn parse check or at the
/// consumer's compile, never a silent wire change.
module IrType =

    /// Opaque type ref: a complete pre-serialized TL value, written raw.
    [<Literal>]
    let RawBytes = "rawBytes"

    /// `int128 4*[ int ]` — 16 raw bytes, no length prefix, no padding.
    [<Literal>]
    let Int128 = "int128"

    /// `int256 8*[ int ]` — 32 raw bytes.
    [<Literal>]
    let Int256 = "int256"

    /// Marks a reference to a generated type that is written WITHOUT its
    /// constructor id (TL's lowercase type reference).
    [<Literal>]
    let BarePrefix = "bare:"

    /// `Vector<T>`: `0x1CB5C415`, count, elements.
    [<Literal>]
    let ArraySuffix = " array"

    /// `vector<T>`: count, elements. No constructor id.
    [<Literal>]
    let BareVectorSuffix = " barevector"

    [<Literal>]
    let OptionSuffix = " option"

    let private endsWith (suffix: string) (t: string) =
        t.EndsWith(suffix, System.StringComparison.Ordinal)

    /// Byte width of a fixed-width TL scalar, or None.
    ///
    /// The family is closed: `grep -nE '\*\[ ' schema/*.tl` yields only
    /// `int128 4*[ int ]` and `int256 8*[ int ]`, so there is no third case
    /// to generalise for. A new one would be a new token here.
    let fixedWidth (t: string) : int option =
        match t with
        | Int128 -> Some 16
        | Int256 -> Some 32
        | _ -> None

    let isFixedBytes (t: string) = (fixedWidth t).IsSome

    let bare (name: string) = BarePrefix + name
    let isBare (t: string) = t.StartsWith(BarePrefix, System.StringComparison.Ordinal)
    let unbare (t: string) = if isBare t then t.Substring BarePrefix.Length else t

    let isBoxedVector (t: string) = endsWith ArraySuffix t
    let isBareVector (t: string) = endsWith BareVectorSuffix t
    let isVector (t: string) = isBoxedVector t || isBareVector t

    let vectorOf (isBareVec: bool) (element: string) =
        element + (if isBareVec then BareVectorSuffix else ArraySuffix)

    /// The element type of a vector; the type itself if it is not one.
    let element (t: string) =
        if isBareVector t then t.Substring(0, t.Length - BareVectorSuffix.Length)
        elif isBoxedVector t then t.Substring(0, t.Length - ArraySuffix.Length)
        else t

    let isOption (t: string) = endsWith OptionSuffix t
    let unoption (t: string) = if isOption t then t.Substring(0, t.Length - OptionSuffix.Length) else t

    /// Erase every wire-only marker. What is left is exactly the vocabulary
    /// the emitters spoke before #116/#117: `byte[]`, `X array`, `X option`,
    /// `rawBytes` and generated type names. Call this wherever a type NAME is
    /// wanted; never where a read or a write is being emitted.
    let rec spelling (t: string) : string =
        if isOption t then spelling (unoption t) + OptionSuffix
        elif isVector t then spelling (element t) + ArraySuffix
        elif isFixedBytes t then "byte[]"
        else unbare t

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
        | "int128" -> Some IrType.Int128
        | "int256" -> Some IrType.Int256
        | "bool" -> Some "bool"
        | _ -> None

    /// Splice extras from a [[structural_overlays]] entry into a field list.
    /// For each extra, find the field whose TL name matches `After` and insert
    /// the extra immediately after it. Extras carry `LayerGate = Some maxOld`
    /// so the writer wraps their writes in `if layer > maxOld then ...`.
    /// Unknown `After` anchors are logged and skipped (schema drift signal).
    ///
    /// **Idempotent**: if a field with the extra's name already exists on the
    /// type (because the upstream schema bump pulled the field into the base
    /// definition), the overlay is skipped — the existing field stays. This
    /// lets old `[[structural_overlays]]` entries survive a schema upgrade
    /// without producing duplicate-field compile errors. Operators can then
    /// retire the now-redundant overlays at their leisure.
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

        let existingFieldNames =
            fields |> List.map (fun f -> f.Name) |> Set.ofList

        let buildExtra (_, name, tlType) : GeneratedField option =
            // Idempotency guard — see docstring.
            if existingFieldNames.Contains(Naming.camelCase name) then
                None
            else
                tlScalarToFSharp tlType
                |> Option.map (fun fsharp ->
                    { Name = Naming.camelCase name
                      RecordName = Naming.pascalCase name
                      FSharpType = fsharp
                      IsOptional = false
                      FlagField = None
                      FlagBit = None
                      LayerGate = Some maxOldLayer })

        // Fields the overlay declares that ALREADY exist in the base schema
        // (the primary schema advanced past `max_old_layer` and pulled the
        // field into the base definition). The field stays at its base
        // position, but it must still be gated: a layer ≤ max_old_layer caller
        // uses the OLD constructor CID whose wire shape lacks this field, so
        // writing it unconditionally desyncs that reader. We therefore stamp
        // `LayerGate = Some max_old_layer` onto the existing field so the
        // writer wraps it in `if layer > max_old_layer`.
        let gateExistingNames =
            extras
            |> List.choose (fun (_, name, _) ->
                let camel = Naming.camelCase name
                if existingFieldNames.Contains(camel) then Some camel else None)
            |> Set.ofList

        [
            for f in fields do
                if gateExistingNames.Contains(f.Name) && f.LayerGate.IsNone then
                    yield { f with LayerGate = Some maxOldLayer }
                else
                    yield f

                match Map.tryFind f.Name byAfter with
                | Some es ->
                    for e in es do
                        match buildExtra e with
                        | Some g -> yield g
                        | None -> () // unsupported type OR field already present (gated above)
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
    // Previously a set of "opaque" types mapped to raw byte[] to dodge recursion/depth.
    // That corrupts DESERIALIZATION (an embedded boxed object has no length prefix, so the
    // `ReadBytes` stub reads the wrong number of bytes and derails the rest of the message —
    // e.g. webPage.cached_page:Page). We now generate real parsers for everything; the
    // generator handles recursive unions (RichText, PageBlock) via SCC ordering.
    let private opaqueTypes : Set<string> = Set.empty

    /// Internal sentinel for opaque-type-ref `byte[]` fields; the F# type
    /// emitted is still `byte[]` but the writer chooses `WriteRawBytes`
    /// instead of `WriteBytes`. Recognized in `mkSynType`, the writer/reader
    /// emit functions, and the writer-target's `isPrimitive` set.
    [<Literal>]
    let RawBytesSentinel = IrType.RawBytes

    let rec mapPrimitiveType (name: string) : string =
        match name.ToLowerInvariant() with
        | "int" -> "int32"
        | "long" -> "int64"
        | "double" -> "double"
        // Fixed-width raw scalars, NOT the `bytes` primitive: 16 and 32 bytes
        // with no length prefix and no padding. They kept their TL spelling as
        // the IR token so the width survives to the emitters (#116).
        | "int128" -> IrType.Int128
        | "int256" -> IrType.Int256
        | "string" -> "string"
        | "bytes" -> "byte[]"
        | "bool" -> "bool"
        | "true" -> "bool"
        | "#" -> "int32"
        | "object" | "!x" -> "obj"
        | _ ->
            let pc = pascalCase name
            if opaqueTypes.Contains pc then RawBytesSentinel else pc

    /// Scalar tokens: everything `mapPrimitiveType` can produce that is not a
    /// reference to a generated type. Only a generated-type reference can be
    /// bare — a primitive has no constructor id to omit.
    let private scalarTokens =
        Set.ofList [ "int32"; "int64"; "double"; "string"; "byte[]"; "bool"; "obj"
                     IrType.Int128; IrType.Int256; IrType.RawBytes ]

    let rec private mapTypeExpr (expr: TlTypeExpr) : string =
        match expr with
        | TlTypeExpr.Bare id -> mapIdentType true id
        | TlTypeExpr.Boxed id -> mapIdentType false id
        | TlTypeExpr.TypeVar _ -> "obj"
        | TlTypeExpr.Vector(isBare, inner) -> IrType.vectorOf isBare (mapTypeExpr inner)
        | TlTypeExpr.Nat -> "int32"
        | TlTypeExpr.Conditional(_, _, inner) -> mapTypeExpr inner

    /// `isBareRef` is TL's lowercase type reference: the constructor's fields
    /// with no constructor id in front. It names a *constructor*, which for
    /// every bare reference in api.tl/mtproto.tl pascal-cases to the same
    /// identifier the emitter gives that constructor's class.
    and private mapIdentType (isBareRef: bool) (id: TlIdentifier) : string =
        let fullName =
            match id.Namespace with
            | Some ns -> $"%s{ns}.%s{id.Name}"
            | None -> id.Name
        let mapped = mapPrimitiveType fullName
        if isBareRef && not (scalarTokens.Contains mapped) then IrType.bare mapped else mapped

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
        | TlTypeExpr.Vector(isBare, inner) -> IrType.vectorOf isBare (mapTypeExpr inner)
        | _ -> mapTypeExpr expr

    let internal mapTypeExprPublic = mapTypeExpr
