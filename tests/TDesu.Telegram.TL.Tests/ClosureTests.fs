namespace TDesu.Telegram.TL.Tests

open NUnit.Framework
open TDesu.Telegram.TL
open TDesu.Telegram.TL.AST
open TDesu.Telegram.TL.Generator

/// §1 of docs/design/td-tl-gen-improvements.md (SedBot repo): transitive
/// whitelist closure. A whitelist-scoped `types` target must emit not just
/// the whitelisted names but every type transitively reachable from them,
/// so `writers` (whitelisted independently, on raw TL constructor names)
/// never references an undefined name and `--no-whitelist` is no longer
/// required for a self-consistent build.
///
/// Fixture: `A` → `B` → `C` (transitively reachable), `D` unrelated (never
/// pulled in). Mirrors the design doc's suggested fixture shape.
[<TestFixture>]
module ClosureTests =

    let private parse (tl: string) : TlSchema =
        match AstFactory.parse tl with
        | Ok s -> s
        | Error e -> failwith e

    /// `A` (field ref) → `B` (field ref) → `C` (leaf); `D` is disjoint.
    let private baseSchema =
        "a#00000001 b:B = A;\n\
         b#00000002 c:C = B;\n\
         c#00000003 x:int = C;\n\
         d#00000004 y:int = D;\n"

    let private typeNames (types: GeneratedType list) =
        types
        |> List.map (function Record(n, _, _) -> n | Union(n, _) -> n)
        |> Set.ofList

    [<Test>]
    let ``types whitelist closure reaches transitive field references, not unrelated types`` () =
        let schema = parse baseSchema
        let types, _ =
            SchemaMapper.mapSchemaWhitelisted schema (set [ "A" ]) Set.empty Map.empty
        let names = typeNames types
        Assert.That(names, Is.EquivalentTo(set [ "A"; "B"; "C" ]), NUnitString.op_Implicit $"got: %A{names}")

    [<Test>]
    let ``vector-wrapped field reference closes over the element type, not Vector itself`` () =
        let schema =
            parse (baseSchema + "aVec#00000005 items:Vector<B> = AVec;\n")
        let types, _ =
            SchemaMapper.mapSchemaWhitelisted schema (set [ "AVec" ]) Set.empty Map.empty
        let names = typeNames types
        Assert.That(names, Is.EquivalentTo(set [ "AVec"; "B"; "C" ]), NUnitString.op_Implicit $"got: %A{names}")

    [<Test>]
    let ``flags-conditional field reference is a hard dependency even though it's optional on the wire`` () =
        let schema =
            parse (baseSchema + "aFlag#00000006 flags:# b:flags.0?B = AFlag;\n")
        let types, _ =
            SchemaMapper.mapSchemaWhitelisted schema (set [ "AFlag" ]) Set.empty Map.empty
        let names = typeNames types
        Assert.That(names, Is.EquivalentTo(set [ "AFlag"; "B"; "C" ]), NUnitString.op_Implicit $"got: %A{names}")

    /// A function seed's RETURN type must join the closure too — a
    /// zero-param request's only link to its response type is `ReturnType`,
    /// which `Whitelist.resolve` did not walk before this fix (it only
    /// walked `f.Params`, the request side).
    [<Test>]
    let ``function seed closes over its return type, not just its params`` () =
        let schema =
            parse (
                baseSchema
                + "aRet#00000007 = ARet;\n---functions---\ngetARet#00000008 = ARet;\n"
            )
        let types, functions =
            SchemaMapper.mapSchemaWhitelisted schema (set [ "GetARet" ]) Set.empty Map.empty
        Assert.That(functions |> List.map (fun f -> f.Name), Does.Contain "GetARet")
        Assert.That(typeNames types, Does.Contain "ARet", NUnitString.op_Implicit $"got: %A{typeNames types}")

    /// `SchemaMapper.deriveTypeSeeds` is what makes `types` see what
    /// `writers` needs: `[whitelists].writers` names raw TL constructors
    /// (snake_case, e.g. "a"), not the boxed PascalCase result-type name
    /// `types`/`mapSchemaWhitelisted` expect. `deriveTypeSeeds` must
    /// translate before seeding, and the closure that follows must still
    /// reach `B`/`C` and still exclude `D`.
    [<Test>]
    let ``deriveTypeSeeds translates raw writer-whitelist constructor names into closure seeds`` () =
        let schema = parse baseSchema
        let seeds = SchemaMapper.deriveTypeSeeds schema Set.empty (set [ "a" ]) Set.empty
        Assert.That(seeds, Is.EquivalentTo(set [ "A" ]), NUnitString.op_Implicit $"seeds: %A{seeds}")

        let types, _ = SchemaMapper.mapSchemaWhitelisted schema seeds Set.empty Map.empty
        Assert.That(typeNames types, Is.EquivalentTo(set [ "A"; "B"; "C" ]), NUnitString.op_Implicit $"got: %A{typeNames types}")

    /// Same translation for `writer_layer_types` (also raw TL constructor
    /// names), independent of `writers` — a config could reasonably combine
    /// the two, but each must work as its own seed source.
    [<Test>]
    let ``deriveTypeSeeds translates writer_layer_types names too`` () =
        let schema = parse baseSchema
        let seeds = SchemaMapper.deriveTypeSeeds schema Set.empty Set.empty (set [ "a" ])
        Assert.That(seeds, Is.EquivalentTo(set [ "A" ]), NUnitString.op_Implicit $"seeds: %A{seeds}")

    [<Test>]
    let ``deriveTypeSeeds is a strict superset of the plain type whitelist`` () =
        let schema = parse baseSchema
        let seeds = SchemaMapper.deriveTypeSeeds schema (set [ "D" ]) (set [ "a" ]) Set.empty
        Assert.That(seeds, Is.EquivalentTo(set [ "A"; "D" ]), NUnitString.op_Implicit $"seeds: %A{seeds}")

    /// Regression for the bug the closure fix exposed: `EmitWriters`
    /// previously decided whether `Requests.{X}` is a union (so its
    /// `toWrite{X}` converter must pattern-match) purely from the
    /// writer-whitelisted constructor count. When only SOME of a boxed
    /// type's constructors are writer-whitelisted, the REQUEST side is
    /// still the full union (`types` groups by the whole schema), so the
    /// converter must still pattern-match — with a runtime `failwith` for
    /// the constructor(s) that have no writer support — instead of doing
    /// `x.Field` on what is actually a DU case (a compile error once the
    /// type is genuinely emitted, which whitelist-scoped `--no-whitelist`
    /// removal now makes routine).
    [<Test>]
    let ``converter pattern-matches every request-side case even when only one constructor is writer-whitelisted`` () =
        let schema =
            parse
                "xNotModified#00000010 = X;\n\
                 x#00000011 n:int = X;\n"
        let actual =
            EmitWriters.generateWriterModule "TDesu.Serialization" schema (set [ "x" ]) Set.empty [] [] Set.empty

        // Writer side stays a record (only `x` is writer-whitelisted).
        if not (actual.Contains "WriteXParams") then
            Assert.Fail($"expected a WriteXParams record (only 1 ctor writer-whitelisted):\n{actual}")

        // Converter must pattern-match the request-side union, not read a
        // field directly off `x` (which is not a record on that side).
        if not (actual.Contains "toWriteX (x: X)") then
            Assert.Fail($"expected a toWriteX converter over the full request-side union:\n{actual}")
        if actual.Contains "x.N" then
            Assert.Fail($"converter must not do direct field access on a union-typed request value:\n{actual}")

        // The unsupported case falls back to a runtime failure, not a
        // silently wrong conversion.
        if not (actual.Contains "X.X(n) -> { N = n }") then
            Assert.Fail($"expected the writer-whitelisted case to build WriteXParams from bound fields:\n{actual}")
        if not (actual.Contains "| _ -> failwith \"toWriteX: constructor not covered by [whitelists].writers\"") then
            Assert.Fail($"expected the non-writer-whitelisted case to fail loudly, not silently:\n{actual}")

    /// Regression: a field referencing a boxed type outside the writer
    /// whitelist resolves to `rawBytes` (`resolveFieldType`'s existing,
    /// correct fallback) — the `toWrite{X}` converter must reproduce those
    /// exact bytes by re-serializing through the request-side type's own
    /// `Serialize`, not pass the request value through unchanged (a type
    /// mismatch: `byte[]` expected, boxed record/union given).
    [<Test>]
    let ``converter re-serializes a field type outside the writer whitelist instead of passing it through`` () =
        let schema =
            parse
                "notWhitelisted#00000020 n:int = NotWhitelisted;\n\
                 holder#00000021 inner:NotWhitelisted = Holder;\n"
        let actual =
            EmitWriters.generateWriterModule "TDesu.Serialization" schema (set [ "holder" ]) Set.empty [] [] Set.empty

        if not (actual.Contains "NotWhitelisted.Serialize") then
            Assert.Fail($"expected the converter to re-serialize the non-whitelisted field type:\n{actual}")
        if not (actual.Contains "use __buf = new TlWriteBuffer()") then
            Assert.Fail($"expected a disposable scratch buffer (TlWriteBuffer implements IDisposable):\n{actual}")
