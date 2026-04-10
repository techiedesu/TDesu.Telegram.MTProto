module TDesu.Telegram.TL.Tests.TypeExprParserTests

open NUnit.Framework
open FParsec
open TDesu.Telegram.TL.AST
open TDesu.Telegram.TL.AST.Parsers.TypeParsers
open TDesu.Telegram.TL.Tests

let private runParser p input =
    match run (p .>> eof) input with
    | Success(result, _, _) -> Some result
    | Failure _ -> None

[<Test>]
let ``typeExpr parses bare type`` () =
    let result = runParser typeExpr "int"
    equals result (Some(TlTypeExpr.Bare { Namespace = None; Name = "int" }))

[<Test>]
let ``typeExpr parses bare type int128`` () =
    let result = runParser typeExpr "int128"
    equals result (Some(TlTypeExpr.Bare { Namespace = None; Name = "int128" }))

[<Test>]
let ``typeExpr parses boxed type`` () =
    let result = runParser typeExpr "Bool"
    equals result (Some(TlTypeExpr.Boxed { Namespace = None; Name = "Bool" }))

[<Test>]
let ``typeExpr parses typevar`` () =
    let result = runParser typeExpr "!X"
    equals result (Some(TlTypeExpr.TypeVar "X"))

[<Test>]
let ``typeExpr parses nat`` () =
    let result = runParser typeExpr "#"
    equals result (Some TlTypeExpr.Nat)

[<Test>]
let ``typeExpr parses Vector of long`` () =
    let result = runParser typeExpr "Vector<long>"
    let expected = TlTypeExpr.Vector(TlTypeExpr.Bare { Namespace = None; Name = "long" })
    equals result (Some expected)

[<Test>]
let ``typeExpr parses vector lowercase`` () =
    let result = runParser typeExpr "vector<int>"
    let expected = TlTypeExpr.Vector(TlTypeExpr.Bare { Namespace = None; Name = "int" })
    equals result (Some expected)

[<Test>]
let ``typeExpr parses Vector of Boxed`` () =
    let result = runParser typeExpr "Vector<User>"
    let expected = TlTypeExpr.Vector(TlTypeExpr.Boxed { Namespace = None; Name = "User" })
    equals result (Some expected)

[<Test>]
let ``typeExpr parses conditional type`` () =
    let result = runParser typeExpr "flags.0?string"
    let expected = TlTypeExpr.Conditional("flags", 0, TlTypeExpr.Bare { Namespace = None; Name = "string" })
    equals result (Some expected)

[<Test>]
let ``typeExpr parses conditional with flags2`` () =
    let result = runParser typeExpr "flags2.3?Bool"
    let expected = TlTypeExpr.Conditional("flags2", 3, TlTypeExpr.Boxed { Namespace = None; Name = "Bool" })
    equals result (Some expected)

[<Test>]
let ``typeExpr parses conditional Vector`` () =
    let result = runParser typeExpr "flags.1?Vector<User>"
    let inner = TlTypeExpr.Vector(TlTypeExpr.Boxed { Namespace = None; Name = "User" })
    let expected = TlTypeExpr.Conditional("flags", 1, inner)
    equals result (Some expected)
