namespace TDesu.Telegram.TL.AST

module Helpers =

    open FParsec

    /// Skip horizontal whitespace (spaces and tabs, not newlines)
    let ws<'a> : Parser<unit, 'a> = skipMany (skipAnyOf [| ' '; '\t' |])

    /// Skip whitespace including newlines
    let wsNl<'a> : Parser<unit, 'a> = spaces

    /// Parse a hex string and return it as uint32
    let hexUint32<'a> : Parser<uint32, 'a> =
        many1Satisfy (fun c -> isHex c) |>> fun s -> System.Convert.ToUInt32(s, 16)

    /// Lowercase identifier: [a-z][a-zA-Z0-9_]*
    let lcIdent<'a> : Parser<string, 'a> =
        let isFirstChar c = c >= 'a' && c <= 'z'
        let isTailChar c = isAsciiLetter c || isDigit c || c = '_'
        many1Satisfy2 isFirstChar isTailChar

    /// Uppercase identifier: [A-Z][a-zA-Z0-9_]*
    let ucIdent<'a> : Parser<string, 'a> =
        let isFirstChar c = c >= 'A' && c <= 'Z'
        let isTailChar c = isAsciiLetter c || isDigit c || c = '_'
        many1Satisfy2 isFirstChar isTailChar

    /// Any identifier (lowercase or uppercase start)
    let ident<'a> : Parser<string, 'a> =
        let isFirstChar c = isAsciiLetter c
        let isTailChar c = isAsciiLetter c || isDigit c || c = '_'
        many1Satisfy2 isFirstChar isTailChar

    /// Parse `namespace.name` or just `name` into TlIdentifier
    let namespacedIdent<'a> : Parser<TlIdentifier, 'a> =
        attempt (
            ident .>>. (pchar '.' >>. ident)
            |>> fun (ns, name) -> { Namespace = Some ns; Name = name }
        )
        <|> (ident |>> fun name -> { Namespace = None; Name = name })
