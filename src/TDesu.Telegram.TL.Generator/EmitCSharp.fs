namespace TDesu.Telegram.TL.Generator

open System.Text

/// C# backend: emits a single-layer C# TL schema (types + serialize/deserialize
/// + constructor ids) over the same CodeModel IR the F# emitters use. Unlike the
/// F# writers this is SINGLE-LAYER: no `layer` parameter, no LayerGate, no
/// layer-variant CIDs — constructor ids come straight from the IR (current
/// schema layer). Unions become an abstract base + sealed nested case classes.
///
/// Runtime contract (Altergram.Schema namespace):
///   interface ITlObject { uint ConstructorId { get; }; void Serialize(TlWriteBuffer w); }
///   TlWriteBuffer: WriteInt32/WriteInt64/WriteDouble/WriteBool/WriteString/
///                  WriteBytes/WriteRawBytes/WriteConstructorId/WriteVector<T>
///   TlReadBuffer:  ReadInt32/ReadInt64/ReadDouble/ReadBool/ReadString/
///                  ReadBytes/ReadRawBytes/ReadConstructorId/ReadVector<T>/Remaining
///
/// Contract: every type's instance `Serialize` writes its own constructor id;
/// every static `Deserialize(TlReadBuffer)` reads its own constructor id first.
/// Unions read the cid then dispatch to a per-case `ReadBody` (no re-read).
module EmitCSharp =

    let private valueTypes = set [ "int"; "long"; "double"; "bool" ]

    /// Namespace prefix for generated types, set at buildModule time. Generated
    /// type references are qualified with it so they never resolve to an
    /// in-scope property of the same name (CS0120).
    let mutable private nsRef = ""

    /// Unions that have a case whose name equals the union name (the common TL
    /// "primary constructor" pattern, e.g. user#... in type User). Their abstract
    /// boxed type is named `<Name>Base` so the case can keep the clean `<Name>`
    /// as a top-level class — no nesting, no '_' suffix.
    let mutable private collidingUnions : Set<string> = Set.empty

    /// The C# name of a union's abstract boxed type (what fields are typed as).
    let private baseNameOf (unionName: string) =
        if Set.contains unionName collidingUnions then unionName + "Base" else unionName

    /// C# reserved words that need `@` escaping when used as identifiers.
    let private csKeywords =
        set [ "abstract";"as";"base";"bool";"break";"byte";"case";"catch";"char";"checked";
              "class";"const";"continue";"decimal";"default";"delegate";"do";"double";"else";
              "enum";"event";"explicit";"extern";"false";"finally";"fixed";"float";"for";
              "foreach";"goto";"if";"implicit";"in";"int";"interface";"internal";"is";"lock";
              "long";"namespace";"new";"null";"object";"operator";"out";"override";"params";
              "private";"protected";"public";"readonly";"ref";"return";"sbyte";"sealed";"short";
              "sizeof";"stackalloc";"static";"string";"struct";"switch";"this";"throw";"true";
              "try";"typeof";"uint";"ulong";"unchecked";"unsafe";"ushort";"using";"virtual";
              "void";"volatile";"while" ]

    let private unbacktick (s: string) = s.Replace("`", "")
    let private escId (s: string) =
        let s = unbacktick s
        if csKeywords.Contains s then "@" + s else s

    /// Map an IR FSharpType string to a C# type spelling.
    let rec csType (t: string) : string =
        if t.EndsWith(" array") then csType (t.Substring(0, t.Length - 6)) + "[]"
        elif t.EndsWith(" option") then csType (t.Substring(0, t.Length - 7)) + "?"
        else
            match t with
            | "int32" -> "int"
            | "int64" -> "long"
            | "double" -> "double"
            | "string" -> "string"
            | "byte[]" -> "byte[]"
            | "bool" -> "bool"
            | "obj" -> "byte[]"
            | CodeModelMapping.RawBytesSentinel -> "byte[]"
            | other ->
                let nm = baseNameOf other
                if nsRef = "" then nm else nsRef + "." + nm

    /// Strip a single suffix layer to get the base IR type.
    let private baseIr (t: string) : string =
        if t.EndsWith(" array") then t.Substring(0, t.Length - 6)
        elif t.EndsWith(" option") then t.Substring(0, t.Length - 7)
        else t

    let private isArrayIr (t: string) = t.EndsWith(" array")

    let private isValueCs (cs: string) =
        let b = if cs.EndsWith("?") then cs.Substring(0, cs.Length - 1) else cs
        valueTypes.Contains b

    // ---- scalar read/write --------------------------------------------------

    /// Statement writing scalar `expr` of IR base type `t` (no suffix).
    let private writeScalarStmt (w: string) (t: string) (expr: string) : string =
        match t with
        | "int32" -> $"{w}.WriteInt32({expr});"
        | "int64" -> $"{w}.WriteInt64({expr});"
        | "double" -> $"{w}.WriteDouble({expr});"
        | "bool" -> $"{w}.WriteBool({expr});"
        | "string" -> $"{w}.WriteString({expr});"
        | "byte[]" -> $"{w}.WriteBytes({expr});"
        | "obj" -> $"{w}.WriteRawBytes({expr});"
        | CodeModelMapping.RawBytesSentinel -> $"{w}.WriteRawBytes({expr});"
        | _ -> $"{expr}.Serialize({w});"

    /// Expression reading a scalar of IR base type `t`.
    let private readScalarExpr (r: string) (t: string) : string =
        match t with
        | "int32" -> $"{r}.ReadInt32()"
        | "int64" -> $"{r}.ReadInt64()"
        | "double" -> $"{r}.ReadDouble()"
        | "bool" -> $"{r}.ReadBool()"
        | "string" -> $"{r}.ReadString()"
        | "byte[]" -> $"{r}.ReadBytes()"
        | "obj" -> $"{r}.ReadRawBytes({r}.Remaining)"
        | CodeModelMapping.RawBytesSentinel -> $"{r}.ReadRawBytes({r}.Remaining)"
        | csName -> $"{csType csName}.Deserialize({r})"

    // ---- recursive value read/write (handles nested vectors) ----------------

    /// Statement writing value `expr` of full IR type `t` (may be "X array...").
    let rec private writeValueStmt (w: string) (t: string) (expr: string) : string =
        if isArrayIr t then
            let elem = baseIr t
            let elemCs = csType elem
            let inner = writeValueStmt "w_" elem "it"
            $"{w}.WriteVector<{elemCs}>({expr}, (w_, it) => {{ {inner} }});"
        else
            writeScalarStmt w t expr

    /// Expression reading a value of full IR type `t`.
    let rec private readValueExpr (r: string) (t: string) : string =
        if isArrayIr t then
            let elem = baseIr t
            let elemCs = csType elem
            let inner = readValueExpr "r_" elem
            $"{r}.ReadVector<{elemCs}>(r_ => {inner})"
        else
            readScalarExpr r t

    // ---- flag helpers -------------------------------------------------------

    let private flagWordNames (fields: GeneratedField list) =
        fields |> List.choose (fun f -> f.FlagField) |> List.distinct

    let private isRawFlag (flagWords: string list) (f: GeneratedField) =
        f.FSharpType = "int32" && List.contains f.Name flagWords

    let private isPresenceFlag (f: GeneratedField) =
        f.FlagField.IsSome && f.FlagBit.IsSome && not f.IsOptional

    let private isOptional (f: GeneratedField) =
        f.FlagField.IsSome && f.FlagBit.IsSome && f.IsOptional

    // ---- C# property + field emission ---------------------------------------

    /// C# property name for a field within an enclosing type. If the PascalCase
    /// name would collide with the enclosing type (CS0542), fall back to the
    /// camelCase form (e.g. `Message.message`, `Username.username`) — clean and
    /// distinct since C# is case-sensitive, rather than an ugly '_' suffix.
    let private propName (enclosing: string) (f: GeneratedField) : string =
        let n = unbacktick f.RecordName
        if n = enclosing && n.Length > 0 then
            escId (string (System.Char.ToLowerInvariant n[0]) + n.Substring(1))
        else
            escId n

    /// Property declaration for a non-raw-flag field.
    let private propDecl (enclosing: string) (f: GeneratedField) : string =
        let cs = csType f.FSharpType
        $"    public {cs} {propName enclosing f};"

    /// Local name for a flag word (its raw TL name: "flags"/"flags2").
    let private flagLocal (name: string) = escId name

    /// Emit the field-write statements (excluding cid) for a constructor's
    /// fields. `access` maps a field's RecordName to its C# accessor expression.
    let private emitWrites (w: string) (fields: GeneratedField list) (access: GeneratedField -> string) : string list =
        let flagWords = flagWordNames fields
        // Precompute flag-word assignments.
        let flagSetup =
            [ for fw in flagWords ->
                let sets =
                    fields
                    |> List.filter (fun f -> f.FlagField = Some fw && f.FlagBit.IsSome)
                    |> List.map (fun f ->
                        let bit = f.FlagBit.Value
                        let acc = access f
                        if isPresenceFlag f then $"if ({acc}) {flagLocal fw} |= (1 << {bit});"
                        else
                            // optional: value-type uses HasValue, ref-type != null
                            let cs = csType f.FSharpType
                            if isValueCs cs then $"if ({acc}.HasValue) {flagLocal fw} |= (1 << {bit});"
                            else $"if ({acc} != null) {flagLocal fw} |= (1 << {bit});")
                fw, sets ]
        let flagDecls = [ for fw, _ in flagSetup -> $"int {flagLocal fw} = 0;" ]
        let flagSets = [ for _, sets in flagSetup do yield! sets ]
        let bodyLines =
            [ for f in fields do
                if isRawFlag flagWords f then
                    yield $"{w}.WriteInt32({flagLocal f.Name});"
                elif isPresenceFlag f then
                    () // presence flag writes nothing
                elif isOptional f then
                    let acc = access f
                    let cs = csType f.FSharpType
                    let inner = baseIr f.FSharpType
                    if isValueCs cs then
                        let stmt = writeValueStmt w inner (acc + ".Value")
                        yield $"if ({acc}.HasValue) {stmt}"
                    else
                        let stmt = writeValueStmt w inner acc
                        yield $"if ({acc} != null) {stmt}"
                else
                    yield writeValueStmt w f.FSharpType (access f) ]
        flagDecls @ flagSets @ bodyLines

    /// Emit field-read statements + the constructed object's field initializers.
    /// Returns (statements, initializerPairs) where init pairs are (PropName, localExpr).
    let private emitReads (enclosing: string) (r: string) (fields: GeneratedField list) : string list * (string * string) list =
        let flagWords = flagWordNames fields
        let stmts = System.Collections.Generic.List<string>()
        let inits = System.Collections.Generic.List<string * string>()
        for f in fields do
            if isRawFlag flagWords f then
                stmts.Add($"int {flagLocal f.Name} = {r}.ReadInt32();")
            elif isPresenceFlag f then
                let fw = flagLocal f.FlagField.Value
                let bit = f.FlagBit.Value
                let local = "p_" + f.RecordName
                stmts.Add($"bool {local} = ({fw} & (1 << {bit})) != 0;")
                inits.Add(propName enclosing f, local)
            elif isOptional f then
                let fw = flagLocal f.FlagField.Value
                let bit = f.FlagBit.Value
                let cs = csType f.FSharpType
                let inner = baseIr f.FSharpType
                let local = "v_" + f.RecordName
                let readExpr = readValueExpr r inner
                let readExpr = if isValueCs cs then $"({cs})({readExpr})" else readExpr
                stmts.Add($"{cs} {local} = ({fw} & (1 << {bit})) != 0 ? {readExpr} : null;")
                inits.Add(propName enclosing f, local)
            else
                let cs = csType f.FSharpType
                let local = "v_" + f.RecordName
                stmts.Add($"{cs} {local} = {readValueExpr r f.FSharpType};")
                inits.Add(propName enclosing f, local)
        List.ofSeq stmts, List.ofSeq inits

    // ---- record + union + function emission ---------------------------------

    let private dataFields (fields: GeneratedField list) =
        let fw = flagWordNames fields
        fields |> List.filter (fun f -> not (isRawFlag fw f))

    let private emitRecordClass (sb: StringBuilder) (name: string) (fields: GeneratedField list) (cid: uint32) =
        let cidHex = sprintf "0x%08Xu" cid
        sb.AppendLine($"public sealed class {name} : ITlObject") |> ignore
        sb.AppendLine("{") |> ignore
        sb.AppendLine($"    public const uint Cid = {cidHex};") |> ignore
        sb.AppendLine($"    public uint ConstructorId => Cid;") |> ignore
        for f in dataFields fields do sb.AppendLine(propDecl name f) |> ignore
        // Serialize
        sb.AppendLine("    public void Serialize(TlWriteBuffer w)") |> ignore
        sb.AppendLine("    {") |> ignore
        sb.AppendLine("        w.WriteConstructorId(Cid);") |> ignore
        for line in emitWrites "w" fields (propName name) do
            sb.AppendLine($"        {line}") |> ignore
        sb.AppendLine("    }") |> ignore
        // Deserialize
        sb.AppendLine($"    public static {name} Deserialize(TlReadBuffer r)") |> ignore
        sb.AppendLine("    {") |> ignore
        sb.AppendLine("        r.ReadConstructorId();") |> ignore
        let stmts, inits = emitReads name "r" fields
        for s in stmts do sb.AppendLine($"        {s}") |> ignore
        sb.AppendLine($"        return new {name}") |> ignore
        sb.AppendLine("        {") |> ignore
        for (p, v) in inits do sb.AppendLine($"            {p} = {v},") |> ignore
        sb.AppendLine("        };") |> ignore
        sb.AppendLine("    }") |> ignore
        sb.AppendLine("}") |> ignore
        sb.AppendLine() |> ignore

    /// Abstract boxed type for a union: dispatch-only Deserialize over cid+aliases.
    let private emitUnionBase (sb: StringBuilder) (name: string) (cases: UnionCase list) =
        let baseName = baseNameOf name
        sb.AppendLine($"public abstract class {baseName} : ITlObject") |> ignore
        sb.AppendLine("{") |> ignore
        sb.AppendLine("    public abstract uint ConstructorId { get; }") |> ignore
        sb.AppendLine("    public abstract void Serialize(TlWriteBuffer w);") |> ignore
        sb.AppendLine($"    public static {baseName} Deserialize(TlReadBuffer r)") |> ignore
        sb.AppendLine("    {") |> ignore
        sb.AppendLine("        uint cid = r.ReadConstructorId();") |> ignore
        sb.AppendLine("        return cid switch") |> ignore
        sb.AppendLine("        {") |> ignore
        for c in cases do
            let allCids = (c.ConstructorId :: c.AliasCids) |> List.map (sprintf "0x%08Xu") |> String.concat " or "
            sb.AppendLine($"            {allCids} => {c.Name}.ReadBody(r),") |> ignore
        sb.AppendLine("            _ => throw new System.IO.InvalidDataException($\"Unknown constructor 0x{cid:x8} for " + baseName + "\"),") |> ignore
        sb.AppendLine("        };") |> ignore
        sb.AppendLine("    }") |> ignore
        sb.AppendLine("}") |> ignore
        sb.AppendLine() |> ignore

    /// One union case as a clean top-level sealed class deriving from the base.
    let private emitCaseClass (sb: StringBuilder) (unionName: string) (c: UnionCase) =
        let baseName = baseNameOf unionName
        let cidHex = sprintf "0x%08Xu" c.ConstructorId
        sb.AppendLine($"public sealed class {c.Name} : {baseName}") |> ignore
        sb.AppendLine("{") |> ignore
        sb.AppendLine($"    public const uint Cid = {cidHex};") |> ignore
        sb.AppendLine($"    public override uint ConstructorId => Cid;") |> ignore
        for f in dataFields c.Fields do sb.AppendLine(propDecl c.Name f) |> ignore
        sb.AppendLine("    public override void Serialize(TlWriteBuffer w)") |> ignore
        sb.AppendLine("    {") |> ignore
        sb.AppendLine("        w.WriteConstructorId(Cid);") |> ignore
        for line in emitWrites "w" c.Fields (propName c.Name) do
            sb.AppendLine($"        {line}") |> ignore
        sb.AppendLine("    }") |> ignore
        sb.AppendLine($"    internal static {c.Name} ReadBody(TlReadBuffer r)") |> ignore
        sb.AppendLine("    {") |> ignore
        let stmts, inits = emitReads c.Name "r" c.Fields
        for s in stmts do sb.AppendLine($"        {s}") |> ignore
        sb.AppendLine($"        return new {c.Name}") |> ignore
        sb.AppendLine("        {") |> ignore
        for (p, v) in inits do sb.AppendLine($"            {p} = {v},") |> ignore
        sb.AppendLine("        };") |> ignore
        sb.AppendLine("    }") |> ignore
        sb.AppendLine("}") |> ignore
        sb.AppendLine() |> ignore

    let private emitFunctionClass (sb: StringBuilder) (fn: GeneratedFunction) =
        let cidHex = sprintf "0x%08Xu" fn.ConstructorId
        sb.AppendLine($"public sealed class {fn.Name} : ITlObject") |> ignore
        sb.AppendLine("{") |> ignore
        sb.AppendLine($"    public const uint Cid = {cidHex};") |> ignore
        sb.AppendLine($"    public uint ConstructorId => Cid;") |> ignore
        for f in dataFields fn.Params do sb.AppendLine(propDecl fn.Name f) |> ignore
        sb.AppendLine("    public void Serialize(TlWriteBuffer w)") |> ignore
        sb.AppendLine("    {") |> ignore
        sb.AppendLine("        w.WriteConstructorId(Cid);") |> ignore
        for line in emitWrites "w" fn.Params (propName fn.Name) do
            sb.AppendLine($"        {line}") |> ignore
        sb.AppendLine("    }") |> ignore
        sb.AppendLine($"    public static {fn.Name} DeserializeFields(TlReadBuffer r)") |> ignore
        sb.AppendLine("    {") |> ignore
        let stmts, inits = emitReads fn.Name "r" fn.Params
        for s in stmts do sb.AppendLine($"        {s}") |> ignore
        sb.AppendLine($"        return new {fn.Name}") |> ignore
        sb.AppendLine("        {") |> ignore
        for (p, v) in inits do sb.AppendLine($"            {p} = {v},") |> ignore
        sb.AppendLine("        };") |> ignore
        sb.AppendLine("    }") |> ignore
        sb.AppendLine($"    public static {fn.Name} Deserialize(TlReadBuffer r)") |> ignore
        sb.AppendLine("    {") |> ignore
        sb.AppendLine("        r.ReadConstructorId();") |> ignore
        sb.AppendLine("        return DeserializeFields(r);") |> ignore
        sb.AppendLine("    }") |> ignore
        sb.AppendLine("}") |> ignore
        sb.AppendLine() |> ignore

    /// Build the whole C# module string.
    let buildModule (namespaceName: string) (types: GeneratedType list) (functions: GeneratedFunction list) : string =
        nsRef <- namespaceName
        collidingUnions <-
            types
            |> List.choose (function
                | Union(name, cases) when cases |> List.exists (fun c -> c.Name = name) -> Some name
                | _ -> None)
            |> Set.ofList

        // Guard against top-level name clashes (records, union cases, functions
        // all share the namespace now that cases aren't nested).
        let seen = System.Collections.Generic.HashSet<string>()
        let claim (n: string) =
            if not (seen.Add n) then
                failwithf "EmitCSharp: duplicate top-level type name '%s' — needs disambiguation" n
        for t in types do
            match t with
            | Record(name, _, _) -> claim name
            | Union(name, cases) ->
                claim (baseNameOf name)
                for c in cases do claim c.Name
        for fn in functions do claim fn.Name

        let sb = StringBuilder()
        sb.AppendLine("// <auto-generated> td-tl-gen C# backend. Do not edit. </auto-generated>") |> ignore
        sb.AppendLine("#nullable enable") |> ignore
        sb.AppendLine($"namespace {namespaceName};") |> ignore
        sb.AppendLine() |> ignore
        for t in types do
            match t with
            | Record(name, fields, cid) -> emitRecordClass sb name fields cid
            | Union(name, cases) ->
                emitUnionBase sb name cases
                for c in cases do emitCaseClass sb name c
        for fn in functions do
            emitFunctionClass sb fn
        sb.ToString()
