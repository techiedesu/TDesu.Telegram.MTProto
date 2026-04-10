open TDesu.Telegram.TL
open TDesu.Telegram.TL.AST
open TDesu.Telegram.TL.Generator
open TDesu.Telegram.TL.Generator.Overrides

// Load the embedded default config
let defaults = Config.loadDefaults ()

// Load and merge custom overrides on top
let config = Config.loadWithOverlay "my_overrides.toml"

printfn "=== Default config ==="
printfn "  Layer variants: %d" defaults.LayerVariants.Length
printfn "  Aliases: %d" defaults.Aliases.Length
printfn "  Extras: %d" defaults.Extras.Length
printfn "  Type whitelist: %d entries" defaults.TypeWhitelist.Count
printfn "  Writer whitelist: %d entries" defaults.WriterWhitelist.Count

printfn ""
printfn "=== Merged config (defaults + my_overrides.toml) ==="
printfn "  Layer variants: %d" config.LayerVariants.Length
printfn "  Aliases: %d" config.Aliases.Length
printfn "  Extras: %d" config.Extras.Length
printfn "  Type whitelist: %d entries" config.TypeWhitelist.Count
printfn "  Writer whitelist: %d entries" config.WriterWhitelist.Count

// Show custom additions
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

// Parse a schema and generate CID module with custom config
let schema = """
boolFalse#bc799737 = Bool;
boolTrue#997275b5 = Bool;
---functions---
messages.sendMessage#dff8042c peer:InputPeer message:string = Updates;
"""

match AstFactory.parse schema with
| Ok parsed ->
    let mtprotoSchema = { TDesu.Telegram.TL.AST.TlSchema.Constructors = []; Functions = []; Layer = None }
    let cidModule = EmitTemplates.generateCidModule config mtprotoSchema parsed
    printfn ""
    printfn "=== Generated CID module (first 20 lines) ==="
    cidModule.Split('\n') |> Array.take (min 20 (cidModule.Split('\n').Length)) |> Array.iter (printfn "  %s")
| Error err ->
    eprintfn "Parse error: %s" err
