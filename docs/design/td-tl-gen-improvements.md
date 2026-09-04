# td-tl-gen: improvements

Design notes for the upstream tool, organised by feature. Everything
here needs the tool's own parsed-schema access — the wrapper
(`tools/regen-tl.fsx`) can only work around it. Ship into `td-tl-gen`
and drop the wrapper flags per the follow-up checklists.

---

## 1. Transitive whitelist closure

## Problem

`td-tl-gen 0.9.0` (published as `TDesu.Telegram.TL.Generator`) has a
whitelist for the `types` target that decides which schema constructors
land in `components.schemas` / F# records. The `writers` target has its
own whitelist and emits `WriteFoo` DUs whose case fields reference the
request-side types (`Foo`).

If the writer whitelist covers a type whose fields include another type
that is NOT in the `types` whitelist, the writers file references an
undefined name at compile time. In 0.8 and earlier the tool implicitly
pulled every writer-referenced type into `types`; 0.9.0 dropped that,
leaving ~100 undefined names in `GeneratedTlWriters.g.fs` for a
whitelist-scoped downstream regen.

Current workaround: `tools/regen-tl.fsx` passes `--no-whitelist`, which
tells 0.9.0 to emit the FULL schema (every constructor). Wire compat is
preserved (CIDs derive from `.tl`), but the emitted files balloon:
`Base.g.fs` ≈ 4 MB, `GeneratedTlWriters.g.fs` ≈ 1.3 MB. Compilation is
still fast enough today, but the diff blast radius on every regen is
unpleasant to review and every downstream user of `td-tl-gen` inherits
the same tension.

## Fix

Restore implicit transitive closure in the generator. The `types` target
must emit not just what the whitelist names, but also every type
transitively required to keep the `writers` (and `client-parsers`) target
self-consistent.

### Algorithm

Inputs: the `.tl` schema, the merged overrides file. Outputs: an emit
set for the `types` target that is a strict superset of the whitelist.

```text
1. Parse the schema into a graph:
     TypeGraph : Map<BoxedType, Set<Constructor>>            // boxed → its ctors
     CtorFields : Map<Constructor, Set<BoxedType>>           // ctor → boxed types it references
   Both maps skip TL primitives (`int`, `long`, `bytes`, `true`, `#`, `!`,
   `Vector<t>` wrappers, parametric `{t:Type}` declarations).

2. Build the initial seed set from the whitelist. Seeds MUST include:
     * every boxed type in `[whitelists].types`
     * every boxed type of a constructor in `[whitelists].writers`
     * every boxed type of a client-parsed function's RETURN type in
       `[whitelists].client_parsers`
     * every writer_layer_type's boxed type

3. BFS from the seed set:
     visited ← seed
     queue   ← seed
     while queue non-empty:
       t ← dequeue
       for ctor ∈ TypeGraph[t]:
         for dep ∈ CtorFields[ctor]:
           if dep ∉ visited:
             visited ← visited ∪ {dep}
             enqueue dep

4. Emit `types` target for exactly `visited`. Emit `writers` unchanged
   (it stays whitelist-scoped and now references only defined types).
```

Complexity: O(constructors + fields) — one linear parse plus a
worklist. For api.tl (~1600 ctors, ~5000 fields) this is milliseconds.

### Edge cases

* **Vector unwrap.** `Vector<Foo>` in a field position → dependency on
  `Foo`, NOT on `Vector`.
* **`flags.N?T`.** Conditional fields still contribute `T` as a hard
  dependency (Deserialize must know how to read `T` even if the flag is
  clear on this particular wire).
* **Dot-namespaced boxed types.** `messages.Dialogs`, `photos.Photos`,
  etc. — treat the whole dotted name as one identifier.
* **`InputPeer` and other unions.** Boxed types with many ctors: pulling
  the boxed type into the closure pulls every ctor that implements it,
  which pulls every field type of every such ctor. That is the intended
  transitive behaviour.
* **`stub_types` / `layer_variants`.** Already handled by the existing
  overrides machinery; the closure runs BEFORE those transforms so stub
  substitutions still land last.

### CLI surface

Preferred: no new flag. Closure is always on when the whitelist is
active; `--no-whitelist` remains as the "emit literally everything"
escape hatch.

Alternative: `--whitelist-closure=strict|off` if we want to keep the
current 0.9.0 behaviour available (default strict = closure on).

## Testing

Add a fixture `MTProto.TL.Generator.Tests/ClosureTests.fs`:

* Small hand-written `.tl` fragment declaring `A`, `B`, `C`, `D` where
  `A` fields reference `B`, `B` references `C`, `D` is unrelated.
* Whitelist `[A]` → closure `[A, B, C]`, not `D`.
* Whitelist a writer for a ctor of `A` → same closure.
* Vector wrap: field is `Vector<B>` → same closure.
* flags.N?B → same closure.

Fixture keeps the closure logic honest under refactors and is cheap to
run in the generator's own test suite.

## §1 Consumer-side follow-up (once the closure ships)

1. `dotnet tool update tdesu.telegram.tl.generator` in
   `.config/dotnet-tools.json` — pin the closure-supporting version.
2. Drop `--no-whitelist` from `regen-tl.fsx`
   (`regenSchemaTargets`) + the sibling doc comment.
3. Run `dotnet fsi tools/regen-tl.fsx`; confirm the expected shrink
   (`Base.g.fs` from ~4 MB to under 1 MB is realistic).
4. Rebuild + full test suite.
5. Refresh `AGENTS.md` § Generated code to drop the `--no-whitelist`
   note and add a pointer to the closure feature.
6. Commit the tool bump + regen output together so wire compat is
   review-friendly.

## Why not do the closure in `tools/regen-tl.fsx`

Considered. The wrapper would need its own TL parser (~300 lines with
all the edge cases above), then rewrite the merged overrides file to
add the closed set to `[whitelists].types` before every invocation.

Two reasons not to:

1. Every downstream user of `td-tl-gen` re-implements the same wrapper
   and drifts. The right home for schema-aware behaviour is the tool
   that already parses the schema.
2. `td-tl-gen` already has the parsed schema in memory when the
   `types` target runs. Doing the closure there is one BFS on data it
   already owns; in the wrapper we parse the schema twice.

If upstream stalls and this becomes urgent, the wrapper fallback is
still a well-scoped 300-line F# script — but it is a hack.

---

## 2. Managed-file hygiene

A consumer's regen wrapper currently does four things post-emit that the tool
itself should own once it grows a `--hygiene` (or equivalent) flag.
Every downstream user of `td-tl-gen` re-implements them otherwise.

### 2.1 Consistent banner + machine-detectable marker

The wrapper prepends this to every emitted `.fs` file:

```text
// <auto-generated>
//   This file is generated by td-tl-gen from cached/api.tl + the merged overrides.
//   DO NOT EDIT — the next `dotnet fsi tools/regen-tl.fsx` overwrites your changes.
//   To change output: edit the schema, the overrides under src/tl-overrides/,
//   or the generator itself. Never this file.
// </auto-generated>
//# td-tl-gen-managed
```

Two loads on that block:

1. `<auto-generated>` is the canonical XML comment that Fantomas,
   Roslyn analyzers, IDE lint rules, and StyleCop skip warnings on.
   Emitting it consistently means IDE noise disappears without a
   per-file suppression list.
2. `//# td-tl-gen-managed` is a machine-detectable marker on line 6.
   Grep-friendly (`grep -l '^//# td-tl-gen-managed'`), stable across
   edits, unlikely to appear by accident in hand-written code.

Targets manifests get the XML variant of the same banner (with
`<!--` … `-->`), also carrying the marker so a marker sweep can hit
them too.

### 2.2 Safe wipe before regen

Every regen currently overwrites `<output>/**/*.g.fs` regardless of
what else is in the directory. If a downstream user hand-adds
`Requests/Custom.fs` next to the tool's output, they lose it.

With the marker in place, the pre-regen sweep MUST:

```text
for every .fs / .targets file under <output>:
    if file's first 10 lines contain "//# td-tl-gen-managed":
        delete it
    else:
        leave it alone
```

Result: hand-added files survive a `--clean` regen. Only files WE
wrote get wiped. This is the reason the marker exists as a distinct
line rather than a comment inside the `<auto-generated>` block —
one-shot text search, no XML parse.

### 2.3 `.Generated.fs` extension instead of `.g.fs`

`.Generated.fs` is the convention used by Myriad, `System.Text.Json`
source generators, F# source generators, and the wider .NET ecosystem.
`.g.fs` is a td-tl-gen-ism inherited from Roslyn's `.g.cs`. Consistency
with the rest of the repo (Myriad emits `<Foo>LogDef.Generated.fs`)
beats consistency with the C# `.g.cs` internals.

Breaking change:

* `.g.fs` files rename to `.Generated.fs`.
* Every `Compile Include="…\Foo.g.fs"` in the manifest rewrites to
  `.Generated.fs`.
* Downstream `.fsproj` refs update in the same regen.
* Doc comments / `.gitattributes` / `.fantomasignore` / linter ignores
  update once at bump time.

### 2.4 File split for the monoliths

Currently `Base.g.fs` is a single, multi-megabyte file holding every
type declared under the base TL namespace, chained together as one
giant `type X = ... and Y = ... and Z = ...` block. IDEs (Rider, VS,
VS Code + Ionide) tokenise it whole and stall on incremental type
checks. Splitting is not a wrapper job — it needs the schema graph.

**Status: implemented in v0.12.0-alpha.2, opt-in via `--split-by-scc`.**
Requires `--split-by-domain` (it further splits that mode's `Base`
bucket specifically — SCC-sharding is unconditional (given the flag)
for whichever domain the config names, always `Base` today; the other
domains stay small enough on their own). Default off — a
`--split-by-domain` run without `--split-by-scc` is byte-for-byte
unchanged from before this section shipped.

#### Correction: `namespace rec` does not span files

The original draft of this section claimed `namespace rec Foo` lets
every type under `Foo` reference every other "regardless of file
order," and proposed relying on that to split `Base.g.fs` into
independently-ordered shard files. That is wrong. Per the F# 4.1
design note that introduced the feature (FS-1009) and confirmed by a
direct test (two files, `namespace rec Foo` in both, `A.fs`'s `type X`
referencing `B.fs`'s `type Y` compiled *after* it):

```text
A.fs(3,15): error FS0039: The type 'Y' is not defined.
```

`namespace rec` / `module rec` only relax ordering *within a single
file's* declarations (equivalent to an `and`-chain, syntactic sugar
over it). File compile order remains a hard requirement across files —
FS-1009 says so explicitly ("it doesn't change the requirement to
have a file order in F# compilation"). A type in an earlier file can
never forward-reference a type first declared in a later file, `rec`
or not.

Consequence: the SCC computation in step 2 below is not optional
scaffolding — it is the *only* thing that makes a multi-file split
possible at all. Every strongly-connected component (mutually
recursive cluster, e.g. `Message` ↔ `MessageMedia` ↔ `Peer` ↔ ...) MUST
land in one file, and shards must be emitted in the SCC DAG's
topological order so a later shard may reference an earlier one but
never the reverse. The "type aliases" alternative this section also
proposed does not help either — an alias still needs its target
already defined, same file-order constraint.

#### Algorithm (as shipped)

```text
1. Build a directed graph over the whitelist-closed type set (post §1):
   node = type name, edge = type A's field references type B.
2. Run Tarjan's SCC algorithm over that graph — EmitTypes.topoSortSCCs.
   This ALREADY EXISTED (since 0.2.7, for single-file and-chain
   grouping) and needed no reimplementation: just exposing (was
   `private`) and reusing for the multi-file case. It already returns
   SCCs in the order multi-file declaration needs (a dependency's SCC
   before its dependent's).
3. Bin-pack SCCs into shards in topological order — EmitTypes.binPackSccs
   — target 400 types or 1 MB per shard (EmitTypes.defaultMaxTypesPerShard
   / defaultMaxBytesPerShard), NEVER splitting an SCC across two
   shards. A single SCC bigger than the target size still gets its
   own, oversized shard — correctness over shard-size uniformity.
   Sequential greedy chunking, not a reordering bin-packer —
   reordering would break the ordering guarantee step 2 exists for.
4. Emit one file per shard — EmitTypes.buildSccShardedModule:
     namespace {ns}.Base
   with `type X = ... and Y = ... and Z = ...` for every type in every
   SCC in that shard. Functions never participate in SCC/shard
   ordering (nothing ever references a function's own name as a field
   type), so every function in the domain is appended to the LAST
   shard, where every type it could reference is already in scope.
5. Emit Base.targets, <Compile Include> entries in the SAME
   topological shard order computed in step 3. Requests.targets
   imports it (<Import Project="Base.targets" />) at Base's position
   instead of a direct <Compile Include> for a single Base.g.fs.
```

Files: `Base.00.g.fs`, `Base.01.g.fs`, … (zero-padded to at least 2
digits, wider past 100 shards) + `Base.targets`.

#### MSBuild path gotcha (found by the compile-check integration test)

`Requests.targets` and `Base.targets` need a MIXED relative-path
convention, not one uniform rule — surprising, and worth recording so
the next `.targets`-emitting feature doesn't rediscover it by trial
and error:

* `<Compile Include="Generated\Requests\<file>">` — the F# SDK's
  compile task resolves a relative `%(Identity)` against the ENTRY
  project's directory, regardless of which imported `.targets` file
  declared the item. This was already `Requests.targets`'s convention;
  `Base.targets`'s own shard entries follow the same rule.
* `<Import Project="<file>">` — pure MSBuild evaluation, resolves
  against the file that WRITES it. `Requests.targets`'s
  `<Import Project="Base.targets" />` is therefore a BARE filename — a
  `Generated\Requests\`-prefixed one double-nests (MSBuild looks for
  `Generated\Requests\Generated\Requests\Base.targets` and fails to
  find it).

Getting this wrong doesn't fail at `td-tl-gen` generation time — only
at the CONSUMER's `dotnet build`. That gap is exactly why an
integration test that actually compiles the output (not just asserts
on emitted text) was a hard requirement for this section, not a nice
to have — see `SccSplitTests.fs` in td-tl-gen.

#### Byte-size estimator: calibrated, not guessed

TL's real schema is dominated by a handful of huge unions
(`MessageMedia`, `InputMedia`, `Update`, …) — a handful of ~20-case
unions outweigh hundreds of small records, so a plain type-COUNT
estimate never trips the 1 MB budget on real data: a real consumer's whole
241-type, 3.3 MB `Base` bucket has only 231 SCCs (biggest: 9 types /
259 KB) — few types, huge bytes, exactly the shape a count-only
budget misses. `EmitTypes.estimateTypeBytes` weights per field and
per union case instead; its constants are calibrated against real
Fantomas output (`buildRecordCode` / `buildUnionCode`), not guessed —
within 0.6% of the real total for that consumer's `Base` bucket (3,327,170
estimated vs. 3,306,380 actual).

#### What's blocking

Nothing — fully implemented and verified:

* Tarjan SCC: reused `EmitTypes.topoSortSCCs` (pre-existing since
  0.2.7, now exposed as public), not reimplemented. Unit-tested on a
  hand-built cyclic fixture (A→B→C→A, D independent, plus a 5th node
  E→A to test topological order of the SCC DAG itself) in
  `SccSplitTests.fs`.
* Bin-packing: `EmitTypes.binPackSccs`, unit-tested for the
  never-split-an-SCC invariant (both the type-count and byte-count
  budget dimensions) and for order preservation.
* Shard emission + functions: `EmitTypes.buildSccShardedModule`. The
  first implementation pass silently dropped a sharded domain's
  functions entirely (no test forced Base to contain a function) —
  caught by deliberately writing that regression test, not by
  inspection; functions now append to the last shard.
* CLI: `--split-by-scc`, opt-in, requires `--split-by-domain` (hard
  error otherwise), default off — `--split-by-domain` alone is
  unchanged.
* Integration test: `SccSplitTests.fs`'s "SCC-split Base output for a
  real cyclic api.tl closure actually compiles" parses the real tdlib
  api.tl fixture, whitelists `Message` (pulling in the real `Message`
  ↔ `MessageMedia` ↔ `Peer` ↔ `WebPage` mutually-recursive cluster
  this section's own example cites), forces a small shard budget to
  get many shards deterministically, and `dotnet build`s a throwaway
  project against the real output. This is what caught the MSBuild
  path gotcha above. Additionally hand-verified (not in the automated
  suite, to keep it fast) against a real consumer's whitelist at the
  DEFAULT 400-type/1 MB budget: 4 Base shards (381 KB / 469 KB /
  1.06 MB / 1.39 MB) + the other 12 domains, `dotnet build` clean.

Whoever touches this next: the algorithm above (not the
pre-2026-08-08 `namespace rec` version in git history) is what
shipped: start there.

### CLI surface (all of §2)

Shipped as independent, individually-documented flags — not the
unified `--hygiene=strict/legacy` bundle this section originally
proposed:

* §2.1 (banner + marker) and §2.2 (safe wipe) run unconditionally on
  every emit — no flag, nothing to opt into or out of.
* §2.3 (`.Generated.fs` extension): tried in 0.10.0-alpha.1, reverted
  in 0.10.0-alpha.2 — the churn on every downstream `.fsproj` wasn't
  worth the cosmetic gain. Back to `.g.fs` unconditionally, no flag.
* §2.4 (file split): `--split-by-domain` (per-domain files) and, on
  top of it, `--split-by-scc` (SCC-shard the oversized `Base` bucket;
  requires `--split-by-domain`, hard error otherwise).

In practice, incremental per-behavior flags — each landing and
shipping independently — turned out simpler to reason about and
review than a single bundle flag would have been; the single-flag
proposal below is kept only as the pre-implementation plan, not
current guidance.

```text
--hygiene=strict   (proposed, not built)   banner + marker + safe wipe + .Generated.fs + split
--hygiene=legacy   (proposed, not built)   emit .g.fs, no banner, no marker — pre-0.9 behaviour
```

## §2 Consumer-side follow-up (once the tool ships)

Once §2 is upstream:

1. Bump `td-tl-gen` in `.config/dotnet-tools.json`.
2. Delete `tools/apply-managed-hygiene.fsx` (one-shot migration script).
3. Simplify `tools/regen-tl.fsx`: drop `sweepManaged`,
   `injectBanner`, `renameGDotFs`, `rewriteTargets`, `finaliseDir`,
   `managedBanner`, `ManagedMarker`. The tool handles it now.
4. Run `dotnet fsi tools/regen-tl.fsx`; the file split should chop
   `Base.Generated.fs` from ~4 MB into `Base.NN.Generated.fs` shards
   under 1 MB each. IDEs stop stalling on incremental checks.
5. Rebuild + full test suite.
6. Refresh `AGENTS.md` § Generated code to drop the wrapper
   post-processing note.
7. Commit the tool bump + regenerated file layout together so wire
   compat is review-friendly.

If §1 (closure) ships in the same version as §2, do both follow-ups
in a single commit and drop `--no-whitelist` at the same time.
