module TDesu.Telegram.TL.AST.Parsers.PrimitiveParsers

open FParsec
open TDesu.Telegram.TL.AST
open TDesu.Telegram.TL.AST.Helpers

/// Parse `#hexdigits` → TlConstructorId
let constructorId<'a> : Parser<TlConstructorId, 'a> =
    pchar '#' >>. hexUint32 |>> TlConstructorId

/// Parse bare `#` (not followed by hex digits) → TlTypeExpr.Nat
let natType<'a> : Parser<TlTypeExpr, 'a> =
    pchar '#' .>> notFollowedBy (satisfy isHex) >>% TlTypeExpr.Nat

/// Parse `!X` → TlTypeExpr.TypeVar
let typeVar<'a> : Parser<TlTypeExpr, 'a> =
    pchar '!' >>. ident |>> TlTypeExpr.TypeVar

/// Lowercase identifier → TlTypeExpr.Bare
let bareType<'a> : Parser<TlTypeExpr, 'a> =
    lcIdent |>> fun name -> TlTypeExpr.Bare { Namespace = None; Name = name }

/// Uppercase or namespaced identifier → TlTypeExpr.Boxed
/// Handles: `Type`, `Ns.Type`, and `ns.Type` (lowercase namespace + uppercase name)
let boxedType<'a> : Parser<TlTypeExpr, 'a> =
    // namespace.Name where namespace can be lowercase or uppercase
    attempt (
        ident .>> pchar '.' .>>. ucIdent
        |>> fun (ns, name) -> TlTypeExpr.Boxed { Namespace = Some ns; Name = name }
    )
    <|> (ucIdent |>> fun name -> TlTypeExpr.Boxed { Namespace = None; Name = name })
