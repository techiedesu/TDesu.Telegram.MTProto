namespace TDesu.Telegram.TL.Generator

/// F# identifier naming conventions: case conversion and keyword escaping.
module Naming =

    let pascalCase (name: string) =
        name.Split([| '_'; '.' |])
        |> Array.map (fun s ->
            if s.Length = 0 then s
            else (string (System.Char.ToUpperInvariant(s[0]))) + s[1..])
        |> String.concat ""

    let camelCase (name: string) =
        let p = pascalCase name
        if p.Length = 0 then p
        else (string (System.Char.ToLowerInvariant(p[0]))) + p[1..]

    /// F# keywords that must be backtick-escaped when used as identifiers.
    let fsharpKeywords = set [
        "type"; "in"; "to"; "do"; "done"; "let"; "rec"; "with"; "match"
        "if"; "then"; "else"; "elif"; "end"; "for"; "while"; "true"; "false"
        "fun"; "function"; "val"; "mutable"; "static"; "member"; "class"
        "abstract"; "override"; "interface"; "module"; "namespace"; "open"
        "new"; "null"; "not"; "or"; "and"; "base"; "begin"; "default"
        "delegate"; "downcast"; "downto"; "exception"; "extern"; "fixed"
        "global"; "inherit"; "inline"; "internal"; "lazy"; "of"; "private"
        "public"; "return"; "select"; "struct"; "try"; "upcast"; "use"
        "void"; "when"; "yield"
        "params"; "process"; "method"; "checked"; "event"
        "protected"; "finally"; "raise"
    ]

    let escapeKeyword (name: string) =
        if fsharpKeywords.Contains name then $"``%s{name}``" else name
