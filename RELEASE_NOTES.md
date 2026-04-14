# Release notes

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
