// Sample showing how to load and inspect a TDesu.Telegram.TL.Generator override
// configuration from F# code (rather than via the `td-tl-gen` CLI). Useful when you
// want to drive code generation from a build script or test fixture.
//
// The 0.1.0+ generator has no embedded defaults — every consumer supplies its own
// TOML file. This sample loads my_overrides.toml directly.

open TDesu.Telegram.TL
open TDesu.Telegram.TL.AST
open TDesu.Telegram.TL.Generator
open TDesu.Telegram.TL.Generator.Overrides

// Load overrides from a TOML file
let config = Config.load "my_overrides.toml"

printfn "=== Loaded config ==="
printfn "  Layer variants:    %d" config.LayerVariants.Length
printfn "  Aliases:           %d" config.Aliases.Length
printfn "  Extras:            %d" config.Extras.Length
printfn "  Type whitelist:    %d entries" config.TypeWhitelist.Count
printfn "  Writer whitelist:  %d entries" config.WriterWhitelist.Count
printfn "  Client parsers:    %d entries" config.ClientParserWhitelist.Count

printfn ""
printfn "=== Custom entries from my_overrides.toml ==="
for lv in config.LayerVariants do
    if lv.Name = "MyCustomType" then
        printfn "  Layer variant: %s (%d variants)" lv.Name lv.Variants.Length

for a in config.Aliases do
    if a.Name = "MessagesSendMessage" && a.Cids.Length > 1 then
        printfn "  Alias: %s (%d CIDs)" a.Name a.Cids.Length

for e in config.Extras do
    if e.Name = "InternalPing" then
        printfn "  Extra: %s (0x%08X) — %s" e.Name e.Cid e.Comment

// Parse a tiny inline schema and emit a CID module against the loaded overrides.
let schema = """
boolFalse#bc799737 = Bool;
boolTrue#997275b5 = Bool;
---functions---
messages.sendMessage#dff8042c peer:InputPeer message:string = Updates;
"""

match AstFactory.parse schema with
| Ok parsed ->
    let mtprotoSchema =
        { TDesu.Telegram.TL.AST.TlSchema.Constructors = []
          Functions = []
          Layer = None }
    let cidModule =
        EmitTemplates.generateCidModule "MyApp.Serialization" config mtprotoSchema parsed
    printfn ""
    printfn "=== Generated CID module (first 20 lines) ==="
    cidModule.Split('\n')
    |> Array.take (min 20 (cidModule.Split('\n').Length))
    |> Array.iter (printfn "  %s")
| Error err ->
    eprintfn "Parse error: %s" err
