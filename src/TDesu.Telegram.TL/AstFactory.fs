namespace TDesu.Telegram.TL

module AstFactory =

    open TDesu.Telegram.TL.AST
    open TDesu.Telegram.TL.AST.Parsers.CombinatorParsers

    let parse (input: string) : Result<TlSchema, string> =
        match FParsec.CharParsers.run schema input with
        | FParsec.CharParsers.Success(result, _, _) -> Ok result
        | FParsec.CharParsers.Failure(errorMsg, _, _) -> Error errorMsg
