module TDesu.Telegram.TL.AST.Parsers.TypeParsers

open FParsec
open TDesu.Telegram.TL.AST
open TDesu.Telegram.TL.AST.Helpers
open TDesu.Telegram.TL.AST.Parsers.PrimitiveParsers

let typeExpr, typeExprRef = createParserForwardedToRef<TlTypeExpr, unit> ()

/// Parse `Vector<T>` (boxed) or `vector<T>` (bare) → TlTypeExpr.Vector.
///
/// The case IS the distinction — `Vector<T>` carries the `0x1CB5C415`
/// constructor id on the wire, `vector<T>` carries only the element count —
/// so this must not fold with `pstringCI`. `attempt` because an identifier
/// that merely starts with "vector" (or "Vector") is not one: without it the
/// consumed prefix would sink the whole `choice`.
let private vectorType : Parser<TlTypeExpr, unit> =
    let of' (keyword: string) (isBare: bool) =
        attempt (pstring keyword >>. pchar '<') >>. typeExpr .>> pchar '>'
        |>> fun inner -> TlTypeExpr.Vector(isBare, inner)
    of' "Vector" false <|> of' "vector" true

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
