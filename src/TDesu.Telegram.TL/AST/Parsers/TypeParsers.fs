module TDesu.Telegram.TL.AST.Parsers.TypeParsers

open FParsec
open TDesu.Telegram.TL.AST
open TDesu.Telegram.TL.AST.Helpers
open TDesu.Telegram.TL.AST.Parsers.PrimitiveParsers

let typeExpr, typeExprRef = createParserForwardedToRef<TlTypeExpr, unit> ()

/// Parse `Vector<T>` or `vector<T>` → TlTypeExpr.Vector
let private vectorType : Parser<TlTypeExpr, unit> =
    (pstringCI "Vector") >>. pchar '<' >>. typeExpr .>> pchar '>'
    |>> TlTypeExpr.Vector

/// Parse `fieldRef.bitIndex?Type` → TlTypeExpr.Conditional
let private conditionalType : Parser<TlTypeExpr, unit> =
    attempt (
        lcIdent .>> pchar '.' .>>. pint32 .>> pchar '?' .>>. typeExpr
        |>> fun ((fieldRef, bitIndex), innerType) ->
            TlTypeExpr.Conditional(fieldRef, bitIndex, innerType)
    )

do
    typeExprRef.Value <-
        choice [
            vectorType
            conditionalType
            typeVar
            natType
            boxedType
            bareType
        ]
