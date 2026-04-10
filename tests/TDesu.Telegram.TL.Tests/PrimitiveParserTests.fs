module TDesu.Telegram.TL.Tests.PrimitiveParserTests

open NUnit.Framework
open FParsec
open TDesu.Telegram.TL.AST
open TDesu.Telegram.TL.AST.Helpers
open TDesu.Telegram.TL.AST.Parsers.PrimitiveParsers
open TDesu.Telegram.TL.Tests

let private runParser p input =
    match run (p .>> eof) input with
    | Success(result, _, _) -> Some result
    | Failure _ -> None

[<Test>]
let ``hexUint32 parses valid hex`` () =
    let result = runParser hexUint32 "a8509bda"
    equals result (Some 0xa8509bdau)

[<Test>]
let ``hexUint32 parses 05162463`` () =
    let result = runParser hexUint32 "05162463"
    equals result (Some 0x05162463u)

[<Test>]
let ``constructorId parses hash followed by hex`` () =
    let result = runParser constructorId "#05162463"
    equals result (Some(TlConstructorId 0x05162463u))

[<Test>]
let ``constructorId parses bc799737`` () =
    let result = runParser constructorId "#bc799737"
    equals result (Some(TlConstructorId 0xbc799737u))

[<Test>]
let ``natType parses bare hash`` () =
    let result = runParser natType "#"
    equals result (Some TlTypeExpr.Nat)

[<Test>]
let ``natType does not parse hash followed by hex`` () =
    let result = runParser natType "#abc"
    equals result None

[<Test>]
let ``typeVar parses exclamation mark identifier`` () =
    let result = runParser typeVar "!X"
    equals result (Some(TlTypeExpr.TypeVar "X"))

[<Test>]
let ``bareType parses lowercase identifier`` () =
    let result = runParser bareType "int"
    equals result (Some(TlTypeExpr.Bare { Namespace = None; Name = "int" }))

[<Test>]
let ``bareType parses int128`` () =
    let result = runParser bareType "int128"
    equals result (Some(TlTypeExpr.Bare { Namespace = None; Name = "int128" }))

[<Test>]
let ``bareType parses int256`` () =
    let result = runParser bareType "int256"
    equals result (Some(TlTypeExpr.Bare { Namespace = None; Name = "int256" }))

[<Test>]
let ``boxedType parses uppercase identifier`` () =
    let result = runParser boxedType "Bool"
    equals result (Some(TlTypeExpr.Boxed { Namespace = None; Name = "Bool" }))

[<Test>]
let ``boxedType parses namespaced identifier`` () =
    let result = runParser boxedType "InputPeer"
    equals result (Some(TlTypeExpr.Boxed { Namespace = None; Name = "InputPeer" }))

[<Test>]
let ``lcIdent parses lowercase starting identifier`` () =
    let result = runParser lcIdent "resPQ"
    equals result (Some "resPQ")

[<Test>]
let ``ucIdent parses uppercase starting identifier`` () =
    let result = runParser ucIdent "ResPQ"
    equals result (Some "ResPQ")

[<Test>]
let ``namespacedIdent parses simple name`` () =
    let result = runParser namespacedIdent "boolFalse"
    equals result (Some { Namespace = None; Name = "boolFalse" })

[<Test>]
let ``namespacedIdent parses dotted name`` () =
    let result = runParser namespacedIdent "auth.sendCode"
    equals result (Some { Namespace = Some "auth"; Name = "sendCode" })
