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

```sh
# Full generation (types + CIDs + functions)
td-tl-gen

# Individual artifacts
td-tl-gen --cid-only              # Constructor ID constants
td-tl-gen --serialization-types   # F# types with Serialize/Deserialize
td-tl-gen --writers               # Standalone write functions
td-tl-gen --tests                 # Round-trip serialization tests
td-tl-gen --coverage              # Handler coverage validator
td-tl-gen --return-types          # CID -> return type mapping
td-tl-gen --layer-aliases         # Layer CID alias mapping
td-tl-gen --client-cids           # Client-side CID constants
td-tl-gen --client-parsers        # Response parser types

# Custom overrides
td-tl-gen --overrides path/to/overrides.toml
```

The generator downloads TL schemas from [core.telegram.org](https://core.telegram.org/schema) on first run and caches them locally.

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
| [CustomOverrides](samples/CustomOverrides) | TOML override configuration for code generation |
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
