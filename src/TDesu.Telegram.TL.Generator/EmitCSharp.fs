namespace TDesu.Telegram.TL.Generator

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax

/// C# backend: emits a single-layer C# TL schema (types + serialize/deserialize
/// + constructor ids) over the same CodeModel IR the F# emitters use. Unlike the
/// F# writers this is SINGLE-LAYER: no `layer` parameter, no LayerGate, no
/// layer-variant CIDs — constructor ids come straight from the IR (current
/// schema layer). Unions become an abstract base + sealed nested case classes.
///
/// Emission is Roslyn-syntax-tree based, mirroring what the F# side does with
/// Fantomas: declarations are built with SyntaxFactory, statement bodies are
/// parsed from generated fragments, and the finished compilation unit is
/// rejected if it carries a single parse diagnostic. A malformed emitter change
/// therefore fails HERE instead of shipping broken C# to the consumer, and
/// indentation comes from Roslyn rather than hand-counted spaces.
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

    /// Terse alias — SyntaxFactory is referenced on nearly every line, and
    /// `open type` would drag `List`/`Argument`/`Identifier` over F# core names.
    type private F = SyntaxFactory

    let private valueTypes = set [ "int"; "long"; "double"; "bool" ]

    /// Bare `Vector<T>` responses put the vector constructor on the wire, not a
    /// type of their own.
    [<Literal>]
    let private VectorCid = 0x1CB5C415u

    /// Namespace prefix for generated types, set at buildModule time. Generated
    /// type references are qualified with it so they never resolve to an
    /// in-scope property of the same name (CS0120).
    let mutable private nsRef = ""

    /// Unions that have a case whose name equals the union name (the common TL
    /// "primary constructor" pattern, e.g. user#... in type User). Their abstract
    /// boxed type is named `<Name>Base` so the case can keep the clean `<Name>`
    /// as a top-level class — no nesting, no '_' suffix.
    let mutable private collidingUnions: Set<string> = Set.empty

    /// The C# name of a union's abstract boxed type (what fields are typed as).
    let private baseNameOf (unionName: string) =
        if Set.contains unionName collidingUnions then unionName + "Base" else unionName

    /// Types some declaration in this surface references *barely*. Only these
    /// get the constructor-id-free `SerializeBody` / `ReadBody` pair, so a
    /// schema with no bare reference emits exactly what it emitted before.
    let mutable private bareBodied: Set<string> = Set.empty

    /// Whether any declaration carries a bare `vector<T>`, and therefore
    /// whether the `TlBare` helper class is emitted at all.
    let mutable private needsBareVector = false

    /// The generated helper holding the bare-vector codec. Generated rather
    /// than required of the runtime: the C# backend's runtime contract (see
    /// the module docstring) is a consumer-owned file, and a codegen fix that
    /// forces a hand-edit there is a fix consumers cannot take by regenerating.
    [<Literal>]
    let BareHelperName = "TlBare"

    /// Names referenced barely anywhere inside a field type, walking through
    /// the ` option` / vector suffixes.
    let rec private bareNamesIn (t: string) : string list =
        if IrType.isOption t then bareNamesIn (IrType.unoption t)
        elif IrType.isVector t then bareNamesIn (IrType.element t)
        elif IrType.isBare t then [ IrType.unbare t ]
        else []

    let rec private hasBareVector (t: string) : bool =
        if IrType.isOption t then hasBareVector (IrType.unoption t)
        elif IrType.isBareVector t then true
        elif IrType.isVector t then hasBareVector (IrType.element t)
        else false

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

    let private hex (cid: uint32) = sprintf "0x%08Xu" cid

    /// Map an IR FSharpType string to a C# type spelling. The wire-only
    /// markers have no spelling of their own: a bare `future_salt` is still a
    /// `FutureSalt`, an `int128` is still a `byte[]`.
    let rec csType (t: string) : string =
        if IrType.isOption t then csType (IrType.unoption t) + "?"
        elif IrType.isVector t then csType (IrType.element t) + "[]"
        elif IrType.isBare t then
            // A bare reference names a CONSTRUCTOR, so it resolves to the case
            // class — never to the `…Base` abstract type `baseNameOf` would
            // hand a boxed reference to the same union.
            let nm = IrType.unbare t
            if nsRef = "" then nm else nsRef + "." + nm
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
            | fixed' when IrType.isFixedBytes fixed' -> "byte[]"
            | other ->
                let nm = baseNameOf other
                if nsRef = "" then nm else nsRef + "." + nm

    /// Strip a single suffix layer to get the base IR type.
    let private baseIr (t: string) : string =
        if IrType.isVector t then IrType.element t
        elif IrType.isOption t then IrType.unoption t
        else t

    let private isArrayIr (t: string) = IrType.isVector t

    let private isValueCs (cs: string) =
        let b = if cs.EndsWith("?") then cs.Substring(0, cs.Length - 1) else cs
        valueTypes.Contains b

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
        | _ when IrType.isFixedBytes t ->
            // `int128`/`int256`: exactly N raw bytes — no length prefix, no
            // padding. The width is a wire invariant `byte[]` cannot state, so
            // it is checked instead of silently truncated: every byte after a
            // short nonce is off by the difference (#116).
            let n = (IrType.fixedWidth t).Value
            $"{{ if ({expr}.Length != {n}) throw new System.InvalidOperationException(\"{t} must be exactly {n} bytes, got \" + {expr}.Length); {w}.WriteRawBytes({expr}); }}"
        | _ when IrType.isBare t ->
            // TL's lowercase reference: the fields, with no constructor id.
            $"{expr}.SerializeBody({w});"
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
        | _ when IrType.isFixedBytes t -> $"{r}.ReadRawBytes({(IrType.fixedWidth t).Value})"
        | _ when IrType.isBare t -> $"{csType t}.ReadBody({r})"
        | csName -> $"{csType csName}.Deserialize({r})"

    /// Statement writing value `expr` of full IR type `t` (may be "X array...").
    let rec private writeValueStmt (w: string) (t: string) (expr: string) : string =
        if isArrayIr t then
            let elem = baseIr t
            let elemCs = csType elem
            let inner = writeValueStmt "w_" elem "it"
            // `Vector<T>` prefixes `0x1CB5C415`; `vector<T>` writes the count
            // and the elements and nothing else. Same emitted shape either
            // way, so the two cannot drift apart (#117).
            if IrType.isBareVector t then
                $"TlBare.WriteVector<{elemCs}>({w}, {expr}, (w_, it) => {{ {inner} }});"
            else
                $"{w}.WriteVector<{elemCs}>({expr}, (w_, it) => {{ {inner} }});"
        else
            writeScalarStmt w t expr

    /// Expression reading a value of full IR type `t`.
    let rec private readValueExpr (r: string) (t: string) : string =
        if isArrayIr t then
            let elem = baseIr t
            let elemCs = csType elem
            let inner = readValueExpr "r_" elem
            if IrType.isBareVector t then
                $"TlBare.ReadVector<{elemCs}>({r}, r_ => {inner})"
            else
                $"{r}.ReadVector<{elemCs}>(r_ => {inner})"
        else
            readScalarExpr r t

    let private flagWordNames (fields: GeneratedField list) =
        fields |> List.choose (fun f -> f.FlagField) |> List.distinct

    let private isRawFlag (flagWords: string list) (f: GeneratedField) =
        f.FSharpType = "int32" && List.contains f.Name flagWords

    let private isPresenceFlag (f: GeneratedField) =
        f.FlagField.IsSome && f.FlagBit.IsSome && not f.IsOptional

    let private isOptional (f: GeneratedField) =
        f.FlagField.IsSome && f.FlagBit.IsSome && f.IsOptional

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

    /// Initializer expression silencing CS8618 on non-nullable reference fields
    /// (deserialize always overwrites; manual construction sets what it needs).
    /// We avoid `required` so partial object construction still compiles.
    ///
    /// `irType` carries the wire markers `cs` has erased: a fixed-width scalar
    /// defaults to an array of the DECLARED width, not to an empty one. The
    /// zero value of an `int128` is sixteen zero bytes — an empty array is not
    /// a smaller nonce, it is a malformed message — and a default-constructed
    /// instance has to satisfy `Serialize`'s width check or every fixture that
    /// builds one (Altergram's `tl-fuzz` truncation sweep, for instance) stops
    /// reaching the type at all.
    let private defaultInit (irType: string) (cs: string) : string option =
        if cs.EndsWith("?") then None
        elif cs = "int" || cs = "long" || cs = "double" || cs = "bool" then None
        elif cs = "string" then Some "\"\""
        else
            match IrType.fixedWidth irType with
            | Some n -> Some $"new byte[{n}]"
            | None -> if cs.EndsWith("[]") then Some "[]" else Some "null!"

    /// Local name for a flag word (its raw TL name: "flags"/"flags2").
    let private flagLocal (name: string) = escId name

    let private dataFields (fields: GeneratedField list) =
        let fw = flagWordNames fields
        fields |> List.filter (fun f -> not (isRawFlag fw f))

    // ── Roslyn helpers ─────────────────────────────────────────────────────

    let private stmt (text: string) : StatementSyntax = F.ParseStatement text
    let private expr (text: string) : ExpressionSyntax = F.ParseExpression text

    /// `void` is only legal as a return type, and ParseTypeName rejects it
    /// (CS1547) — build that one predefined type directly.
    let private ty (text: string) : TypeSyntax =
        if text = "void" then
            F.PredefinedType(F.Token SyntaxKind.VoidKeyword)
        else
            F.ParseTypeName text

    let private modifier (k: SyntaxKind) = F.Token k
    let private pub = modifier SyntaxKind.PublicKeyword
    let private semi = F.Token SyntaxKind.SemicolonToken

    let private block (stmts: StatementSyntax list) = F.Block(Array.ofList stmts)

    /// Comma-separated list that KEEPS a trailing comma: adding a member then
    /// touches one line instead of two, and it matches what the previous
    /// string emitter produced (so the changeover is a no-op for consumers).
    let private commaListTrailing (nodes: 'T list when 'T :> SyntaxNode) =
        F.SeparatedList<'T>(Seq.ofList nodes, List.replicate nodes.Length (F.Token SyntaxKind.CommaToken))

    /// `public const uint Cid = 0x...u;`
    let private cidField (cid: uint32) : MemberDeclarationSyntax =
        F
            .FieldDeclaration(
                F
                    .VariableDeclaration(ty "uint")
                    .AddVariables(F.VariableDeclarator(F.Identifier "Cid").WithInitializer(F.EqualsValueClause(expr (hex cid))))
            )
            .AddModifiers(pub, modifier SyntaxKind.ConstKeyword)

    /// `public [override] uint ConstructorId => Cid;`
    let private constructorIdProp (isOverride: bool) : MemberDeclarationSyntax =
        let mods =
            if isOverride then [| pub; modifier SyntaxKind.OverrideKeyword |] else [| pub |]

        F
            .PropertyDeclaration(ty "uint", F.Identifier "ConstructorId")
            .AddModifiers(mods)
            .WithExpressionBody(F.ArrowExpressionClause(F.IdentifierName "Cid"))
            .WithSemicolonToken(semi)

    let private fieldDecl (enclosing: string) (f: GeneratedField) : MemberDeclarationSyntax =
        let cs = csType f.FSharpType
        let declarator = F.VariableDeclarator(F.Identifier(propName enclosing f))

        let declarator =
            match defaultInit f.FSharpType cs with
            | Some init -> declarator.WithInitializer(F.EqualsValueClause(expr init))
            | None -> declarator

        F.FieldDeclaration(F.VariableDeclaration(ty cs).AddVariables declarator).AddModifiers(pub)

    /// `return new Name { Prop = local, ... };`
    let private returnNew (typeName: string) (inits: (string * string) list) : StatementSyntax =
        let assignments =
            inits
            |> List.map (fun (p, v) ->
                F.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression, F.IdentifierName p, expr v)
                :> ExpressionSyntax)

        let initializer =
            F.InitializerExpression(SyntaxKind.ObjectInitializerExpression, commaListTrailing assignments)

        F.ReturnStatement(F.ObjectCreationExpression(ty typeName).WithInitializer initializer)

    // ── Serialize / Deserialize bodies ─────────────────────────────────────

    /// Field-write statements (excluding the cid). `access` maps a field to its
    /// C# accessor expression; `enclosing` only names the type in guard messages.
    let private emitWrites
        (enclosing: string)
        (w: string)
        (fields: GeneratedField list)
        (access: GeneratedField -> string)
        : StatementSyntax list =
        let flagWords = flagWordNames fields

        /// The boolean "this field is present" expression a flag bit is derived from.
        let presence (f: GeneratedField) =
            let acc = access f

            if isPresenceFlag f then
                acc
            else
                let cs = csType f.FSharpType
                if isValueCs cs then $"{acc}.HasValue" else $"{acc} != null"

        // TL routinely puts several fields behind ONE bit (my_boost + my_boost_slots,
        // setup_password_required + otherwise_relogin_days, ...). Deriving the bit
        // from each field independently sets it for a half-filled object and then
        // writes no payload, which desyncs the reader from that offset on — so the
        // bit is derived once per (word, bit) and disagreement throws.
        let groups =
            fields
            |> List.filter (fun f -> f.FlagField.IsSome && f.FlagBit.IsSome)
            |> List.groupBy (fun f -> f.FlagField.Value, f.FlagBit.Value)

        let flagDecls = [ for fw in flagWords -> stmt $"int {flagLocal fw} = 0;" ]

        let flagSets =
            [ for (fw, bit), groupFields in groups do
                  let exprs = groupFields |> List.map presence
                  let head = List.head exprs

                  match exprs with
                  | [ _ ] -> ()
                  | _ ->
                      let disagreement =
                          exprs
                          |> List.tail
                          |> List.map (fun e -> $"({head}) != ({e})")
                          |> String.concat " || "

                      let names = groupFields |> List.map (fun f -> propName enclosing f) |> String.concat ", "

                      yield
                          stmt
                              $"if ({disagreement}) throw new System.InvalidOperationException(\"{enclosing}: {names} share flags bit {bit} and must be set together.\");"

                  yield stmt $"if ({head}) {flagLocal fw} |= (1 << {bit});" ]

        let body =
            [ for f in fields do
                  if isRawFlag flagWords f then
                      yield stmt $"{w}.WriteInt32({flagLocal f.Name});"
                  elif isPresenceFlag f then
                      () // presence flag writes nothing
                  elif isOptional f then
                      let acc = access f
                      let cs = csType f.FSharpType
                      let inner = baseIr f.FSharpType

                      if isValueCs cs then
                          let write = writeValueStmt w inner (acc + ".Value")
                          yield stmt $"if ({acc}.HasValue) {write}"
                      else
                          yield stmt $"if ({acc} != null) {writeValueStmt w inner acc}"
                  else
                      yield stmt (writeValueStmt w f.FSharpType (access f)) ]

        flagDecls @ flagSets @ body

    /// Field-read statements plus the (PropName, localExpr) pairs the object
    /// initializer is built from.
    let private emitReads
        (enclosing: string)
        (r: string)
        (fields: GeneratedField list)
        : StatementSyntax list * (string * string) list =
        let flagWords = flagWordNames fields
        let stmts = ResizeArray<StatementSyntax>()
        let inits = ResizeArray<string * string>()

        for f in fields do
            if isRawFlag flagWords f then
                stmts.Add(stmt $"int {flagLocal f.Name} = {r}.ReadInt32();")
            elif isPresenceFlag f then
                let fw = flagLocal f.FlagField.Value
                let bit = f.FlagBit.Value
                let local = "p_" + f.RecordName
                stmts.Add(stmt $"bool {local} = ({fw} & (1 << {bit})) != 0;")
                inits.Add(propName enclosing f, local)
            elif isOptional f then
                let fw = flagLocal f.FlagField.Value
                let bit = f.FlagBit.Value
                let cs = csType f.FSharpType
                let inner = baseIr f.FSharpType
                let local = "v_" + f.RecordName
                let readExpr = readValueExpr r inner
                let readExpr = if isValueCs cs then $"({cs})({readExpr})" else readExpr
                stmts.Add(stmt $"{cs} {local} = ({fw} & (1 << {bit})) != 0 ? {readExpr} : null;")
                inits.Add(propName enclosing f, local)
            else
                let cs = csType f.FSharpType
                let local = "v_" + f.RecordName
                stmts.Add(stmt $"{cs} {local} = {readValueExpr r f.FSharpType};")
                inits.Add(propName enclosing f, local)

        List.ofSeq stmts, List.ofSeq inits

    /// `SerializeBody` — the fields with no constructor id, which is what a
    /// bare TL reference puts on the wire. Emitted only for the types some
    /// declaration references barely.
    let private serializeBodyMethod
        (enclosing: string)
        (fields: GeneratedField list)
        (access: GeneratedField -> string)
        : MemberDeclarationSyntax =
        F
            .MethodDeclaration(ty "void", F.Identifier "SerializeBody")
            .AddModifiers(modifier SyntaxKind.InternalKeyword)
            .AddParameterListParameters(F.Parameter(F.Identifier "w").WithType(ty "TlWriteBuffer"))
            .WithBody(block (emitWrites enclosing "w" fields access))

    /// `hasBody`: this type also emits `SerializeBody`, so `Serialize` is the
    /// cid plus a call to it rather than a second copy of the field writes.
    let private serializeMethod
        (isOverride: bool)
        (hasBody: bool)
        (enclosing: string)
        (fields: GeneratedField list)
        (access: GeneratedField -> string)
        : MemberDeclarationSyntax =
        let mods =
            if isOverride then [| pub; modifier SyntaxKind.OverrideKeyword |] else [| pub |]

        let body =
            if hasBody then
                [ stmt "w.WriteConstructorId(Cid);"; stmt "SerializeBody(w);" ]
            else
                stmt "w.WriteConstructorId(Cid);" :: emitWrites enclosing "w" fields access

        F
            .MethodDeclaration(ty "void", F.Identifier "Serialize")
            .AddModifiers(mods)
            .AddParameterListParameters(F.Parameter(F.Identifier "w").WithType(ty "TlWriteBuffer"))
            .WithBody(block body)

    /// A reader method over the type's fields. `readCid` prepends the cid read
    /// (the union case path has already consumed it in the base dispatcher).
    let private readerMethod
        (name: string)
        (methodName: string)
        (modifiers: SyntaxToken[])
        (readCid: bool)
        (fields: GeneratedField list)
        : MemberDeclarationSyntax =
        let stmts, inits = emitReads name "r" fields

        let body =
            [ if readCid then
                  yield stmt "r.ReadConstructorId();"
              yield! stmts
              yield returnNew name inits ]

        F
            .MethodDeclaration(ty name, F.Identifier methodName)
            .AddModifiers(modifiers)
            .AddParameterListParameters(F.Parameter(F.Identifier "r").WithType(ty "TlReadBuffer"))
            .WithBody(block body)

    // ── Top-level declarations ─────────────────────────────────────────────

    let private recordClass (name: string) (fields: GeneratedField list) (cid: uint32) : MemberDeclarationSyntax =
        // Referenced barely somewhere: split the cid off the field codec so a
        // bare reference can reach the fields on their own.
        let bare = bareBodied.Contains name

        let deserialize =
            if bare then
                F
                    .MethodDeclaration(ty name, F.Identifier "Deserialize")
                    .AddModifiers(pub, modifier SyntaxKind.StaticKeyword)
                    .AddParameterListParameters(F.Parameter(F.Identifier "r").WithType(ty "TlReadBuffer"))
                    .WithBody(block [ stmt "r.ReadConstructorId();"; stmt "return ReadBody(r);" ])
                :> MemberDeclarationSyntax
            else
                readerMethod name "Deserialize" [| pub; modifier SyntaxKind.StaticKeyword |] true fields

        let members =
            [ yield cidField cid
              yield constructorIdProp false
              for f in dataFields fields -> fieldDecl name f
              yield serializeMethod false bare name fields (propName name)
              if bare then
                  yield serializeBodyMethod name fields (propName name)
              yield deserialize
              if bare then
                  yield
                      readerMethod
                          name
                          "ReadBody"
                          [| modifier SyntaxKind.InternalKeyword; modifier SyntaxKind.StaticKeyword |]
                          false
                          fields ]

        F
            .ClassDeclaration(name)
            .AddModifiers(pub, modifier SyntaxKind.SealedKeyword)
            .AddBaseListTypes(F.SimpleBaseType(ty "ITlObject"))
            .AddMembers(Array.ofList members)

    /// Abstract boxed type for a union: dispatch-only Deserialize over cid+aliases.
    let private unionBaseClass (name: string) (cases: UnionCase list) : MemberDeclarationSyntax =
        let baseName = baseNameOf name

        let arms =
            [ for c in cases do
                  let pattern =
                      (c.ConstructorId :: c.AliasCids)
                      |> List.map (fun cid -> F.ConstantPattern(expr (hex cid)) :> PatternSyntax)
                      |> List.reduce (fun a b -> F.BinaryPattern(SyntaxKind.OrPattern, a, b) :> PatternSyntax)

                  yield F.SwitchExpressionArm(pattern, expr $"{c.Name}.ReadBody(r)")

              yield
                  F.SwitchExpressionArm(
                      F.DiscardPattern(),
                      F.ThrowExpression(
                          F
                              .ObjectCreationExpression(ty "System.IO.InvalidDataException")
                              .AddArgumentListArguments(
                                  F.Argument(expr ("$\"Unknown constructor 0x{cid:x8} for " + baseName + "\""))
                              )
                      )
                  ) ]

        let deserialize =
            F
                .MethodDeclaration(ty baseName, F.Identifier "Deserialize")
                .AddModifiers(pub, modifier SyntaxKind.StaticKeyword)
                .AddParameterListParameters(F.Parameter(F.Identifier "r").WithType(ty "TlReadBuffer"))
                .WithBody(
                    block
                        [ stmt "uint cid = r.ReadConstructorId();"
                          F.ReturnStatement(F.SwitchExpression(F.IdentifierName "cid", commaListTrailing arms)) ]
                )

        let constructorId =
            F
                .PropertyDeclaration(ty "uint", F.Identifier "ConstructorId")
                .AddModifiers(pub, modifier SyntaxKind.AbstractKeyword)
                .AddAccessorListAccessors(
                    F.AccessorDeclaration(SyntaxKind.GetAccessorDeclaration).WithSemicolonToken(semi)
                )

        let serialize =
            F
                .MethodDeclaration(ty "void", F.Identifier "Serialize")
                .AddModifiers(pub, modifier SyntaxKind.AbstractKeyword)
                .AddParameterListParameters(F.Parameter(F.Identifier "w").WithType(ty "TlWriteBuffer"))
                .WithSemicolonToken(semi)

        F
            .ClassDeclaration(baseName)
            .AddModifiers(pub, modifier SyntaxKind.AbstractKeyword)
            .AddBaseListTypes(F.SimpleBaseType(ty "ITlObject"))
            .AddMembers(constructorId, serialize, deserialize)

    /// One union case as a clean top-level sealed class deriving from the base.
    let private caseClass (unionName: string) (c: UnionCase) : MemberDeclarationSyntax =
        // `ReadBody` already exists on every case — the union's dispatcher has
        // consumed the cid before it calls one. Only the write half is
        // conditional.
        let bare = bareBodied.Contains c.Name

        let members =
            [ yield cidField c.ConstructorId
              yield constructorIdProp true
              for f in dataFields c.Fields -> fieldDecl c.Name f
              yield serializeMethod true bare c.Name c.Fields (propName c.Name)
              if bare then
                  yield serializeBodyMethod c.Name c.Fields (propName c.Name)
              yield
                  readerMethod
                      c.Name
                      "ReadBody"
                      [| modifier SyntaxKind.InternalKeyword; modifier SyntaxKind.StaticKeyword |]
                      false
                      c.Fields ]

        F
            .ClassDeclaration(c.Name)
            .AddModifiers(pub, modifier SyntaxKind.SealedKeyword)
            .AddBaseListTypes(F.SimpleBaseType(ty (baseNameOf unionName)))
            .AddMembers(Array.ofList members)

    let private functionClass (fn: GeneratedFunction) : MemberDeclarationSyntax =
        let deserialize =
            F
                .MethodDeclaration(ty fn.Name, F.Identifier "Deserialize")
                .AddModifiers(pub, modifier SyntaxKind.StaticKeyword)
                .AddParameterListParameters(F.Parameter(F.Identifier "r").WithType(ty "TlReadBuffer"))
                .WithBody(block [ stmt "r.ReadConstructorId();"; stmt "return DeserializeFields(r);" ])

        let members =
            [ yield cidField fn.ConstructorId
              yield constructorIdProp false
              for f in dataFields fn.Params -> fieldDecl fn.Name f
              yield serializeMethod false false fn.Name fn.Params (propName fn.Name)
              yield
                  readerMethod
                      fn.Name
                      "DeserializeFields"
                      [| pub; modifier SyntaxKind.StaticKeyword |]
                      false
                      fn.Params
              yield deserialize ]

        F
            .ClassDeclaration(fn.Name)
            .AddModifiers(pub, modifier SyntaxKind.SealedKeyword)
            .AddBaseListTypes(F.SimpleBaseType(ty "ITlObject"))
            .AddMembers(Array.ofList members)

    // ── Request → response constructor ids ─────────────────────────────────

    [<Literal>]
    let ReturnTypeMapName = "GeneratedReturnTypes"

    /// Constructor ids a response of IR type `t` may legally carry. Bare
    /// primitives have no cid on the wire and map to an empty set.
    let private responseCidsOf (types: GeneratedType list) =
        let byName = System.Collections.Generic.Dictionary<string, uint32 list>()

        for t in types do
            match t with
            | Record(name, _, cid) -> byName[name] <- [ cid ]
            | Union(name, cases) ->
                byName[name] <-
                    [ for c in cases do
                          yield c.ConstructorId
                          yield! c.AliasCids ]

        fun (irType: string) ->
            // A bare vector puts no constructor id on the wire at all, and
            // neither does a bare reference — there is nothing for a caller to
            // match against, so the entry is omitted rather than asserted
            // wrongly (#117).
            if IrType.isBareVector irType || IrType.isBare irType then
                []
            elif isArrayIr irType then
                [ VectorCid ]
            else
                // `bool` collapses to the C# primitive in field position, but on the
                // wire a Bool response is still boolTrue/boolFalse.
                let key = if baseIr irType = "bool" then "Bool" else baseIr irType

                match byName.TryGetValue key with
                | true, cids -> cids
                | _ -> []

    /// `public static class GeneratedReturnTypes` — request cid → legal response
    /// cids. Nothing between a handler and the wire otherwise checks that a
    /// response belongs to the method's declared TL return type.
    let private returnTypeMapClass (types: GeneratedType list) (functions: GeneratedFunction list) : MemberDeclarationSyntax =
        let cidsOf = responseCidsOf types

        let entries =
            [ for fn in functions do
                  match cidsOf fn.ReturnType with
                  | [] -> ()
                  | cids ->
                      let values = cids |> List.map hex |> String.concat ", "

                      yield
                          F.AssignmentExpression(
                              SyntaxKind.SimpleAssignmentExpression,
                              F.ImplicitElementAccess().AddArgumentListArguments(F.Argument(expr (hex fn.ConstructorId))),
                              expr $"[{values}]"
                          )
                          :> ExpressionSyntax ]

        let dictionary =
            F
                .ObjectCreationExpression(ty "System.Collections.Generic.Dictionary<uint, uint[]>")
                .WithInitializer(F.InitializerExpression(SyntaxKind.ObjectInitializerExpression, commaListTrailing entries))

        let field =
            F
                .FieldDeclaration(
                    F
                        .VariableDeclaration(ty "System.Collections.Generic.IReadOnlyDictionary<uint, uint[]>")
                        .AddVariables(
                            F.VariableDeclarator(F.Identifier "ByRequest").WithInitializer(F.EqualsValueClause dictionary)
                        )
                )
                .AddModifiers(pub, modifier SyntaxKind.StaticKeyword, modifier SyntaxKind.ReadOnlyKeyword)

        F
            .ClassDeclaration(ReturnTypeMapName)
            .AddModifiers(pub, modifier SyntaxKind.StaticKeyword)
            .AddMembers(field)

    // ── File assembly ──────────────────────────────────────────────────────

    /// Auto-generated banner + `#nullable enable`, attached once per file.
    /// Parsed rather than hand-assembled: a directive built from bare tokens
    /// carries no whitespace of its own and renders as `#nullableenable`.
    let private headerTrivia =
        F.ParseLeadingTrivia(
            "// <auto-generated> td-tl-gen C# backend. Do not edit. </auto-generated>\n#nullable enable\n"
        )

    /// Render top-level declarations into one file-scoped-namespace source file.
    /// Any parse diagnostic here means the emitter produced invalid C#; failing
    /// now beats shipping it to the consumer's build.
    let private render (namespaceName: string) (members: MemberDeclarationSyntax list) : string =
        let ns =
            F.FileScopedNamespaceDeclaration(F.ParseName namespaceName).AddMembers(Array.ofList members)

        // eol is pinned: NormalizeWhitespace defaults to Environment.NewLine, which
        // makes the emitted bytes depend on the machine that ran the generator. The
        // output is committed by consumers, so it has to be the same on every OS.
        let unit = F.CompilationUnit().AddMembers(ns).NormalizeWhitespace("    ", "\n")

        if unit.ContainsDiagnostics then
            let d =
                unit.GetDiagnostics() |> Seq.map (fun x -> x.ToString()) |> String.concat "; "

            failwithf "EmitCSharp: emitted invalid C# — %s" d

        unit.WithLeadingTrivia(headerTrivia).ToFullString().TrimEnd() + "\n"

    /// `static class TlBare` — the bare-vector codec, mirroring the runtime's
    /// `WriteVector`/`ReadVector` minus the `0x1CB5C415` header. Emitted only
    /// when some declaration carries a `vector<T>`.
    ///
    /// It is generated rather than demanded of the runtime because the runtime
    /// is a consumer-owned file: a codegen fix that also needs a hand-edit
    /// there is a fix consumers cannot take by regenerating. The element-count
    /// bound is the runtime's own rule — every TL element costs at least four
    /// bytes, so a count that cannot fit in `Remaining` is a lie and is
    /// rejected before anything is allocated.
    let private bareHelperClass () : MemberDeclarationSyntax =
        let writeVector =
            F
                .MethodDeclaration(ty "void", F.Identifier "WriteVector")
                .AddModifiers(pub, modifier SyntaxKind.StaticKeyword)
                .AddTypeParameterListParameters(F.TypeParameter "T")
                .AddParameterListParameters(
                    F.Parameter(F.Identifier "w").WithType(ty "TlWriteBuffer"),
                    F.Parameter(F.Identifier "items").WithType(ty "System.Collections.Generic.IReadOnlyList<T>"),
                    F.Parameter(F.Identifier "writeItem").WithType(ty "System.Action<TlWriteBuffer, T>")
                )
                .WithBody(
                    block
                        [ stmt "w.WriteInt32(items.Count);"
                          stmt "foreach (var it in items) { writeItem(w, it); }" ]
                )

        let readVector =
            F
                .MethodDeclaration(ty "T[]", F.Identifier "ReadVector")
                .AddModifiers(pub, modifier SyntaxKind.StaticKeyword)
                .AddTypeParameterListParameters(F.TypeParameter "T")
                .AddParameterListParameters(
                    F.Parameter(F.Identifier "r").WithType(ty "TlReadBuffer"),
                    F.Parameter(F.Identifier "readItem").WithType(ty "System.Func<TlReadBuffer, T>")
                )
                .WithBody(
                    block
                        [ stmt "var count = r.ReadInt32();"
                          stmt
                              "if (count < 0 || (long)count * 4 > r.Remaining) throw new System.IO.InvalidDataException($\"TL bare vector: implausible element count {count} with {r.Remaining} bytes remaining\");"
                          // `return [];` is what the runtime writes, but
                          // NormalizeWhitespace renders it `return[];` — legal,
                          // and needlessly startling in a committed file.
                          stmt "if (count == 0) { return System.Array.Empty<T>(); }"
                          // Grow with the data actually decoded — never trust
                          // the count enough to pre-size, even after the bound.
                          stmt "var items = new System.Collections.Generic.List<T>(System.Math.Min(count, 64));"
                          stmt "for (var i = 0; i < count; i++) { items.Add(readItem(r)); }"
                          stmt "return items.ToArray();" ]
                )

        F
            .ClassDeclaration(BareHelperName)
            .AddModifiers(modifier SyntaxKind.InternalKeyword, modifier SyntaxKind.StaticKeyword)
            .AddMembers(writeVector, readVector)

    /// Shared initialisation: set global refs and check for duplicate top-level
    /// names. Called by both buildModule and buildFiles.
    let private setup (namespaceName: string) (types: GeneratedType list) (functions: GeneratedFunction list) =
        nsRef <- namespaceName

        collidingUnions <-
            types
            |> List.choose (function
                | Union(name, cases) when cases |> List.exists (fun c -> c.Name = name) -> Some name
                | _ -> None)
            |> Set.ofList

        // Every field type in the surface, plus every declared return type:
        // the wire markers #116/#117 introduced decide which extra members get
        // emitted, so they have to be collected before anything is built.
        let allFieldTypes =
            seq {
                for t in types do
                    match t with
                    | Record(_, fields, _) ->
                        for f in fields do
                            yield f.FSharpType
                    | Union(_, cases) ->
                        for c in cases do
                            for f in c.Fields do
                                yield f.FSharpType
                for fn in functions do
                    for f in fn.Params do
                        yield f.FSharpType
                    yield fn.ReturnType
            }
            |> Seq.toList

        bareBodied <- allFieldTypes |> List.collect bareNamesIn |> Set.ofList
        needsBareVector <- allFieldTypes |> List.exists hasBareVector

        // Guard against top-level name clashes (records, union cases, functions
        // all share the namespace now that cases aren't nested).
        let seen = System.Collections.Generic.HashSet<string>()

        let claim (n: string) =
            if not (seen.Add n) then
                failwithf "EmitCSharp: duplicate top-level type name '%s' — needs disambiguation" n

        claim ReturnTypeMapName

        if needsBareVector then
            claim BareHelperName

        for t in types do
            match t with
            | Record(name, _, _) -> claim name
            | Union(name, cases) ->
                claim (baseNameOf name)
                for c in cases do
                    claim c.Name

        for fn in functions do
            claim fn.Name

        // A bare reference has to name a CONSTRUCTOR: only a constructor has a
        // body to write without an id. Catch a reference that resolved to an
        // abstract union base (or to nothing) here, where the message can name
        // the type, rather than as a missing-method error in the consumer.
        let concrete =
            seq {
                for t in types do
                    match t with
                    | Record(name, _, _) -> yield name
                    | Union(_, cases) ->
                        for c in cases do
                            yield c.Name
            }
            |> Set.ofSeq

        match bareBodied - concrete |> Set.toList with
        | [] -> ()
        | missing ->
            failwithf
                "EmitCSharp: bare reference(s) to %s — a bare TL type reference names a constructor, not a boxed type"
                (String.concat ", " missing)

    let private declarationsOf (t: GeneratedType) : MemberDeclarationSyntax list =
        match t with
        | Record(name, fields, cid) -> [ recordClass name fields cid ]
        | Union(name, cases) -> unionBaseClass name cases :: [ for c in cases -> caseClass name c ]

    /// Build the whole C# module as a single string (original behaviour).
    let buildModule (namespaceName: string) (types: GeneratedType list) (functions: GeneratedFunction list) : string =
        setup namespaceName types functions

        let members =
            [ for t in types do
                  yield! declarationsOf t
              for fn in functions -> functionClass fn
              yield returnTypeMapClass types functions
              if needsBareVector then
                  yield bareHelperClass () ]

        render namespaceName members

    /// Build one .g.cs file per top-level declaration and return (filename, content) pairs.
    /// Unions emit their abstract base and all case classes into a single file named
    /// after the base type (e.g. UserBase.g.cs), so cross-case references stay local.
    /// Call before writing to allow the caller to --clean the output directory first.
    let buildFiles
        (namespaceName: string)
        (types: GeneratedType list)
        (functions: GeneratedFunction list)
        : (string * string) list =
        setup namespaceName types functions

        [ for t in types do
              let fileName =
                  match t with
                  | Record(name, _, _) -> name
                  | Union(name, _) -> baseNameOf name

              yield (fileName + ".g.cs", render namespaceName (declarationsOf t))
          for fn in functions do
              yield (fn.Name + ".g.cs", render namespaceName [ functionClass fn ])
          yield (ReturnTypeMapName + ".g.cs", render namespaceName [ returnTypeMapClass types functions ])
          if needsBareVector then
              yield (BareHelperName + ".g.cs", render namespaceName [ bareHelperClass () ]) ]
