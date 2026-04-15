# Release notes

## 0.1.10

Fix for `rawBytes` sentinel inside composite type spellings. `mkSynType` in
`Fantomas/TypeBuilder.fs` only collapsed bare `"rawBytes"` → `"byte[]"`; for
DU case fields whose F# type was a composite string like `"rawBytes option"`
or `"rawBytes array"`, the sentinel survived emission and produced
`type X = { f: rawBytes option }` which doesn't compile.

`mkSynType` is now recursive and parses ` option`/` array` suffixes the same
way `EmitWriters.toSynType` already did, so the sentinel collapses correctly
inside composites.

Consumers can drop the `: rawBytes option` → `: byte[] option` /
`: rawBytes array` → `: byte[] array` post-processing they may have added
as a workaround (SedBot had this in `tools/regen-tl.fsx#patchRawBytes`).

Snapshot tests updated accordingly: arrays in record fields now render as
`T[]` (the SynType.Array AST node) instead of being passed through as raw
text — both spellings are equivalent F#, but the previous output was a
side effect of the buggy passthrough.

## 0.1.9

Fix for `writer_record_per_case_unions` when a union has cases with no
data fields. Previously the generator emitted an empty record
(`type WriteFooBarParams = { }`) which F# rejects ("expecting record
field"). Empty cases now skip the per-case record entirely and stay as
bare `| CaseName` in the union.

Also flags a known generator-emit gotcha (not fixed here): for unions
NOT in `writer_record_per_case_unions`, fields are destructured
positionally — if any case has a field named `w` (the write-buffer
parameter name) or `h`, the destructuring shadows the outer scope. The
existing workaround is to add the union to `writer_record_per_case_unions`,
which switches dispatch to record access (`p_.w`) and avoids shadowing.

## 0.1.8

Wire-format fix: opaque-type-ref `byte[]` fields now serialize with
`WriteRawBytes` (raw blob — caller provides a complete pre-serialized TL
value) instead of `WriteBytes` (TL `bytes` primitive — length-prefixed).

The bug was: the generator's "fallback to byte[]" for unwhitelisted /
hardcoded-opaque types (`Page`, `RichText`, `MediaArea`, …) emitted a
field type of `byte[]` and a write call of `WriteBytes`. For a TL `bytes`
primitive that's correct, but for a boxed type ref it produces wrong wire
output (extra `bytes`-ctor + length + padding wrapped around the value).

Implementation: `CodeModel.mapPrimitiveType` now returns the internal
sentinel `"rawBytes"` for opaque types instead of `"byte[]"`. The
sentinel collapses back to `byte[]` at F# emit time (`mkSynType`,
`toSynType`) but the writer-call path checks for it and emits
`WriteRawBytes` instead of `WriteBytes`.

Read side stays on `ReadBytes` for opaque refs — without schema knowledge
the reader can't structurally parse a boxed type ref. Callers that need
to deserialize must whitelist the opaque type's constructors.

In practice: SedBot always passes `None` / `[||]` for opaque-typed
fields, so this bug never fired at runtime. Future consumers that
actually populate these fields now get correct wire output.

## 0.1.7

Refines the per-case record naming added in 0.1.6: when a case name starts
with the type name, the duplicated prefix is dropped. So instead of
`WriteMessageMessageParams` and `WriteMessageMessageServiceParams`, the
generator now emits `WriteMessageParams` and `WriteMessageServiceParams`.

This keeps the single-case record name `Write{TypeName}Params` stable
across a single→multi-case migration: existing record-with construction
still type-checks, only the union-case wrap becomes necessary at the
point of consumption.

## 0.1.6

New writer-target feature: **per-case records for multi-case unions**, opt-in
via `[whitelists].writer_record_per_case_unions = ["Message", ...]` in the
override TOML.

By default, a multi-case union `WriteX` is emitted with positional case
constructors:

```fsharp
type WriteX =
    | Foo of a: int * b: string * ... 40 fields
    | Bar of x: int * y: int
```

For unions whose F# result-type name (PascalCase) appears in the
`writer_record_per_case_unions` whitelist, the generator instead emits a
record per case and references it from the union:

```fsharp
type WriteXFooParams = { a: int; b: string; ...; (40 fields) }
type WriteXBarParams = { x: int; y: int }
type WriteX =
    | Foo of value: WriteXFooParams
    | Bar of value: WriteXBarParams
```

This makes record-with construction syntax usable at callsites
(`WriteX.Foo({ defaultFoo with a = ... })`) instead of forcing positional
spelling of every field. Useful for unions like the schema's `Message`
that have many fields per case.

Backwards-compatible: unions not listed keep their existing shape.

## 0.1.5

Fix `WriteVector` emission in `EmitTypes.serializeExprFor`: the previous
form `writer.WriteVector (v) (lambda)` is parsed by F# as a curried call,
but `TlWriteBuffer.WriteVector` is a tuple-taking member
(`WriteVector(items: 'a[], writeItem: ...)`) — so the generated code didn't
type-check at the consumer. Now emits `writer.WriteVector(v, lambda)`.

This bug only surfaces when the generated `*.Serialize` member contains a
`Vector<T>` field, so it didn't trip the generator's snapshot tests.
Caught downstream when the SedBot regen produced ~100 build errors.

## 0.1.4

Two emit fixes that previously required hand-restoring `.g.fs` files after
running the generator:

- **Empty `aliases` array offside.** `EmitTemplates.generateLayerAliases`
  emitted `[|\n    |]` when the alias list was empty, which F# parses as
  offside and breaks the build. Now collapses to a single-line `[||]`.
- **`get` accessor for keyword-named fields.** `EmitTypes.mkFieldAccessors`
  pascal-cased keyword-escaped field names like `` ``type`` `` directly,
  producing invalid `` get``type`` `` member names. Now strips the
  surrounding backticks before pascal-casing, yielding `getType`.

## 0.1.3

Allow `GeoPoint` to resolve structurally when its constructors are in the
writers/types whitelist.

Until this release `GeoPoint` was hardcoded in the generator's `opaqueTypes`
list, forcing every field referencing it to emit as `byte[]` regardless of
whitelist — i.e. there was no way to get `WriteGeoPoint.GeoPoint(lon, lat, …)`
out of `td-tl-gen`. `GeoPoint` is a small, non-recursive type with only two
constructors, so the recursion-avoidance rationale that applies to `Page` /
`PageBlock` / `RichText` doesn't hold. `GeoPoint` is now structural by default.

Callers that referenced `geo: byte[]` via the old opaque path do not break on
the package upgrade alone, but the next time they regen writers (whitelist
includes `geoPoint` / `geoPointEmpty`) the field type will change; update
callsites accordingly.

Known issue (pre-existing, not fixed here): `byte[]` fields from remaining
opaque refs (`Page`, `RichText`, `MediaArea`, `MessageExtendedMedia`, …) are
serialized with `WriteBytes` (TL `bytes` primitive, length-prefixed) rather
than `WriteRawBytes`. This is a wire-format bug that only bites if you pass
non-empty data; `None` / empty vectors are safe. Planned fix in 0.2.0.

## 0.1.2

Release plumbing fix — no API changes. Adds `MinVerTagPrefix=v` to
`Directory.Build.props` so the release workflow actually computes the version
from `v*` tags. Prior `v0.1.0` and `v0.1.1` tags were packed as
`0.0.0-alpha.0.2` because MinVer ignored the `v` prefix with no configuration.
Consumers on 0.1.x should install 0.1.2 (first correctly-versioned 0.1
release on NuGet).

## 0.1.0

First proper release. The 0.0.0-alpha.0 generator was a private SedBot helper that
happened to be packaged as a NuGet tool — its hardcoded paths, namespaces and
embedded "default" overrides made it unusable for anyone outside the original
project. 0.1.0 reshapes it into a generic dotnet tool.

This release covers both packages in this repository:

- **TDesu.Telegram.TL 0.1.0** — TL parser and AST. No public API changes from
  0.0.0-alpha.0; just out of alpha.
- **TDesu.Telegram.TL.Generator 0.1.0** (`td-tl-gen`) — major CLI rework, see below.

### Breaking — `td-tl-gen` CLI rework

The CLI is rewritten end-to-end. The old free-floating flags
(`--cid-only`, `--serialization-types`, `--writers`, ...) wrote to hardcoded paths
and used hardcoded namespaces; they're replaced by an explicit
`--target` selector that writes to a directory you choose, with a namespace you
choose.

```sh
td-tl-gen \
  --schema cached/api.tl \
  --output ./Generated \
  --namespace MyApp.Serialization \
  --overrides path/to/overrides.toml \
  --target cid,types,writers,coverage,return-types
```

Required flags:
- `--schema <path>` — TL schema file (e.g. `cached/api.tl`)
- `--output <dir>` — directory where generated `.g.fs` files are written
- `--namespace <ns>` — F# namespace for emitted code
- `--overrides <toml>` — TOML override config (no embedded default in 0.1.0+)
- `--target <names>` — comma-separated targets

Available targets:

| Target | Output file | Notes |
|---|---|---|
| `cid` | `GeneratedCid.g.fs` | Requires `--mtproto-schema <path>` |
| `types` | `GeneratedTlRequests.g.fs` | Whitelist-filtered request types |
| `writers` | `GeneratedTlWriters.g.fs` | Standalone `write{X}` functions and `Write*` DUs |
| `coverage` | `GeneratedCoverageValidator.g.fs` | Handler coverage validator |
| `return-types` | `GeneratedReturnTypes.g.fs` | CID → return type lookup |
| `tests` | `GeneratedRoundTripTests.g.fs` | Round-trip tests for whitelisted requests |
| `layer-aliases` | `GeneratedLayerAliases.g.fs` | Requires `--layer-base-schema <path>` |
| `client-cids` | `GeneratedClientCid.g.fs` | Flat literal CID table |
| `client-parsers` | `GeneratedResponseParsers.g.fs` | Driven by `[whitelists].client_parsers` |
| `all` | (multi) | Equivalent to `cid,types,writers,coverage,return-types` |

Optional flags:
- `--mtproto-schema <path>` — required by the `cid` target
- `--layer-base-schema <path>` — required by the `layer-aliases` target
- `--tests-namespace <ns>` — override the `tests` module name (default `<ns>.Tests.GeneratedRoundTripTests`)
- `--client-namespace <ns>` — override the namespace for `client-cids`/`client-parsers` (default `<ns>.Client.Api`)

### Removed — embedded `DefaultOverrides.toml`

0.0.0-alpha.0 shipped a 484-line `DefaultOverrides.toml` as an embedded resource
inside the generator binary. It was the SedBot project's whitelist verbatim and was
implicitly applied to every invocation, regardless of who ran the generator. There
was no way to opt out.

0.1.0 removes the embedded resource entirely:
- `Config.loadDefaults()` and `Config.loadWithOverlay(...)` are gone
- `Config.load(path)` and `Config.loadMerged(paths)` replace them
- `--overrides <toml>` is **required** on every invocation
- The previous embedded TOML is preserved verbatim under
  `samples/SedBotOverrides/sedbot-overrides.toml` as a worked example

### Removed — hardcoded `["Message"; "User"; "Chat"]` whitelist

`Pipeline.generateClientParsers` previously had a hardcoded mini-whitelist of three
constructor names baked into the code. It's now driven by `OverrideConfig.ClientParserWhitelist`,
populated from a new TOML field:

```toml
[whitelists]
client_parsers = ["Message", "User", "Chat"]
```

Empty whitelist produces an empty parsers module — no implicit defaults.

### Removed — `Pipeline.generate` (raw types dump)

The all-in-one mode that wrote `MTProtoTypes.g.fs`/`ApiTypes.g.fs`/`ApiFunctions.g.fs`
to a hardcoded sibling project is gone. Use the explicit `--target` flags.

### Internal changes

- All `EmitTemplates.generate*` and `Pipeline.generate*` functions now take an
  explicit namespace parameter (no more hardcoded `"TDesu.Serialization"` etc.)
- `EmitWriters.generateWriterModule` takes a namespace parameter
- `Pipeline.fs` no longer hardcodes `__SOURCE_DIRECTORY__`-relative output paths
- The `samples/CustomOverrides` example shows the F# library API

### Migration

If you were calling `td-tl-gen` directly:

```sh
# 0.0.0-alpha.0
td-tl-gen --writers
td-tl-gen --serialization-types
td-tl-gen --cid-only

# 0.1.0
td-tl-gen --schema cached/api.tl --output ./Generated \
          --namespace YourApp.Serialization \
          --overrides your-overrides.toml \
          --target writers,types
td-tl-gen --schema cached/api.tl --mtproto-schema cached/mtproto.tl \
          --output ./Generated --namespace YourApp.Serialization \
          --overrides your-overrides.toml --target cid
```

If you were calling the F# API directly (via project reference), every
`generate*` function now takes a namespace as its first parameter:

```fsharp
// 0.0.0-alpha.0
let code = EmitTemplates.generateCidModule config mtprotoSchema apiSchema
Pipeline.generateSerializationTypes config apiSchema outputPath

// 0.1.0
let code = EmitTemplates.generateCidModule "YourApp.Serialization" config mtprotoSchema apiSchema
Pipeline.generateSerializationTypes "YourApp.Serialization" config apiSchema outputPath
```
