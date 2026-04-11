# TDesu.Telegram.MTProto

[![NuGet TL](https://img.shields.io/nuget/v/TDesu.Telegram.TL.svg?label=TDesu.Telegram.TL)](https://www.nuget.org/packages/TDesu.Telegram.TL)
[![NuGet Generator](https://img.shields.io/nuget/v/TDesu.Telegram.TL.Generator.svg?label=td-tl-gen)](https://www.nuget.org/packages/TDesu.Telegram.TL.Generator)
[![Build](https://github.com/techiedesu/TDesu.Telegram.MTProto/actions/workflows/ci.yml/badge.svg)](https://github.com/techiedesu/TDesu.Telegram.MTProto/actions/workflows/ci.yml)
[![License: Unlicense](https://img.shields.io/badge/License-Unlicense-blue.svg)](https://unlicense.org)

F# library and CLI tool for parsing Telegram [TL (Type Language)](https://core.telegram.org/mtproto/TL) schemas and generating F# code.

## Projects

| Project | Description | Target |
|---------|-------------|--------|
| **TDesu.Telegram.TL** | TL schema parser (FParsec-based) | netstandard2.1 |
| **TDesu.Telegram.TL.Generator** | Code generator CLI tool (`td-tl-gen`) | net10.0 |

> **0.1.0 (breaking)** reshapes `td-tl-gen` from a SedBot-internal helper into a
> generic dotnet tool. New required flags (`--schema`, `--output`, `--namespace`,
> `--overrides`, `--target`), no embedded "default" overrides, no hardcoded paths.
> See [`RELEASE_NOTES.md`](RELEASE_NOTES.md) for the migration list.

## Installation

### Library (NuGet)

```sh
dotnet add package TDesu.Telegram.TL
```

### Generator (dotnet tool)

```sh
dotnet tool install --global TDesu.Telegram.TL.Generator
```

## Usage

### Parser library

```fsharp
open TDesu.Telegram.TL

let schema = """
boolFalse#bc799737 = Bool;
boolTrue#997275b5 = Bool;
---functions---
auth.sendCode#a677244f phone_number:string api_id:int api_hash:string settings:CodeSettings = auth.SentCode;
"""

match AstFactory.parse schema with
| Ok result ->
    printfn "%d constructors, %d functions" result.Constructors.Length result.Functions.Length
| Error err ->
    eprintfn "Parse error: %s" err
```

C# API:

```csharp
using TDesu.Telegram.TL;

var schema = TlParser.Parse(schemaText);  // throws FormatException on error
foreach (var ctor in schema.GetConstructors())
    Console.WriteLine($"{ctor.Id.Name} #{ctor.GetConstructorId():X08}");
```

### Generator CLI

Every invocation needs a schema, an output directory, an F# namespace, an overrides
TOML, and at least one target. There is no embedded default config — supply your own.

```sh
td-tl-gen \
  --schema cached/api.tl \
  --output ./Generated \
  --namespace MyApp.Serialization \
  --overrides path/to/overrides.toml \
  --target cid,types,writers,coverage,return-types
```

| Target | Output file | Notes |
|---|---|---|
| `cid` | `GeneratedCid.g.fs` | Constructor ID literals — also needs `--mtproto-schema` |
| `types` | `GeneratedTlRequests.g.fs` | Whitelist-filtered request types with `Serialize`/`Deserialize` |
| `writers` | `GeneratedTlWriters.g.fs` | Standalone `write{X}` functions and `Write*` DUs |
| `coverage` | `GeneratedCoverageValidator.g.fs` | Handler coverage validator |
| `return-types` | `GeneratedReturnTypes.g.fs` | CID → return type lookup |
| `tests` | `GeneratedRoundTripTests.g.fs` | Round-trip tests for whitelisted requests |
| `layer-aliases` | `GeneratedLayerAliases.g.fs` | Cross-layer CID aliases — needs `--layer-base-schema` |
| `client-cids` | `GeneratedClientCid.g.fs` | Flat literal CID table for clients |
| `client-parsers` | `GeneratedResponseParsers.g.fs` | Driven by `[whitelists].client_parsers` |
| `all` | (multi) | Equivalent to `cid,types,writers,coverage,return-types` |

Optional flags:
- `--mtproto-schema <path>` — required by the `cid` target
- `--layer-base-schema <path>` — required by the `layer-aliases` target
- `--tests-namespace <ns>` — module name for the `tests` target (default `<namespace>.Tests.GeneratedRoundTripTests`)
- `--client-namespace <ns>` — namespace for `client-cids`/`client-parsers` (default `<namespace>.Client.Api`)

Schemas are not downloaded automatically — fetch them manually from
[core.telegram.org/schema](https://core.telegram.org/schema) or your TL source.

### Override TOML

The TOML file controls which TL types/functions get generated and how layer-dependent
CIDs are resolved at runtime. See [`samples/SedBotOverrides/sedbot-overrides.toml`](samples/SedBotOverrides/sedbot-overrides.toml)
for a fully worked example. Sections:

- `[[layer_variants]]` — CIDs that vary by negotiated protocol layer
- `[[aliases]]` — multiple known CIDs for one method (older client layers)
- `[[extras]]` — undocumented CIDs not in the public schema
- `[layer_type_info.<TypeName>]` — per-type layer metadata (e.g. `flags2_min_layer`)
- `[whitelists]` — `types` / `writers` / `writer_layer_types` / `stub_types` / `client_parsers`

## Generator architecture

```
TL schema text
  -> AstFactory.parse           FParsec parser -> TlSchema AST
  -> SchemaMapper.mapSchema     TlSchema -> CodeModel IR
  -> Emit*.fs                   CodeModel -> F# code
     EmitTypes.fs                Fantomas AST -> serialization types
     EmitWriters.fs              Fantomas AST -> writer functions
     EmitTemplates.fs            String templates -> CIDs, tests, etc.
```

## Samples

| Sample | Description |
|--------|-------------|
| [CSharpParser](samples/CSharpParser) | C# API: `TlParser.Parse()`, extension methods |
| [FSharpParser](samples/FSharpParser) | F# API: `AstFactory.parse`, pattern matching on AST |
| [CustomOverrides](samples/CustomOverrides) | F# library API: load TOML overrides and generate from code |
| [SedBotOverrides](samples/SedBotOverrides) | Real-world overrides TOML used by the SedBot MTProto server |
| [PingPongBot](samples/PingPongBot) | End-to-end: TL parsing -> serialization -> real MTProto handshake with Telegram DC |

## Building

```sh
dotnet build
dotnet test
```

## Acknowledgments

Built with assistance from [Claude](https://claude.ai) (Anthropic).

## License

[Unlicense](LICENSE)
