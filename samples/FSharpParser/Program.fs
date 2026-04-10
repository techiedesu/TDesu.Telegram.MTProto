open TDesu.Telegram.TL
open TDesu.Telegram.TL.AST

let [<Literal>] schema = """
boolFalse#bc799737 = Bool;
boolTrue#997275b5 = Bool;

inputPeerEmpty#7f3b18ea = InputPeer;
inputPeerSelf#7da07ec9 = InputPeer;
inputPeerChat#35a95cb9 chat_id:long = InputPeer;

message#5a686d7c flags:# out:flags.1?true text:string entities:flags.0?Vector<MessageEntity> = Message;

---functions---
messages.sendMessage#dff8042c peer:InputPeer message:string = Updates;
auth.signIn#8d52a951 phone_number:string phone_code_hash:string phone_code:string = auth.Authorization;
"""

match AstFactory.parse schema with
| Error err ->
    eprintfn "Parse error: %s" err
    exit 1
| Ok result ->

    printfn "Parsed: %d constructors, %d functions" result.Constructors.Length result.Functions.Length
    printfn "Layer: %A" result.Layer
    printfn ""

    // Pattern match on type expressions
    let rec formatType = function
        | TlTypeExpr.Bare id | TlTypeExpr.Boxed id ->
            match id.Namespace with
            | Some ns -> $"{ns}.{id.Name}"
            | None -> id.Name
        | TlTypeExpr.Vector inner -> $"Vector<{formatType inner}>"
        | TlTypeExpr.Conditional(field, bit, inner) -> $"{field}.{bit}?{formatType inner}"
        | TlTypeExpr.TypeVar name -> $"!{name}"
        | TlTypeExpr.Nat -> "#"

    printfn "=== Constructors ==="
    for c in result.Constructors do
        let name =
            match c.Id.Namespace with
            | Some ns -> $"{ns}.{c.Id.Name}"
            | None -> c.Id.Name
        let cid =
            match c.ConstructorId with
            | Some(TlConstructorId id) -> $"#%08X{id}"
            | None -> ""
        printfn "  %s%s -> %s" name cid (formatType c.ResultType)
        for p in c.Params do
            printfn "    %s: %s" p.Name (formatType p.Type)

    printfn ""
    printfn "=== Functions ==="
    for f in result.Functions do
        let name =
            match f.Id.Namespace with
            | Some ns -> $"{ns}.{f.Id.Name}"
            | None -> f.Id.Name
        printfn "  %s -> %s" name (formatType f.ResultType)
