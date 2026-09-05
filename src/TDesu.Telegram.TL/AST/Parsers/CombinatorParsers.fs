module TDesu.Telegram.TL.AST.Parsers.CombinatorParsers

open FParsec
open TDesu.FSharp
open TDesu.FSharp.Operators
open TDesu.Telegram.TL.AST
open TDesu.Telegram.TL.AST.Helpers
open TDesu.Telegram.TL.AST.Parsers.PrimitiveParsers
open TDesu.Telegram.TL.AST.Parsers.TypeParsers

/// Parse `name:Type` → TlParam (allows uppercase starts like A:bytes, M1:bytes)
let param : Parser<TlParam, unit> =
    ident .>> pchar ':' .>>. typeExpr
    |>> fun (name, typ) -> { Name = name; Type = typ }

/// Parse `{X:Type}` → string (captures the variable name)
let typeParam : Parser<string, unit> =
    between (pchar '{') (pchar '}') (ident .>> pchar ':' .>> ident)

/// Parse a single-line comment: `//` to end of line
let comment<'a> : Parser<unit, 'a> =
    pstring "//" >>. skipRestOfLine true

/// Parse `// LAYER N` and extract N
let private layerComment<'a> : Parser<int, 'a> =
    pstring "//" >>. ws >>. pstringCI "LAYER" >>. ws >>. pint32 .>> skipRestOfLine true

/// Section markers
let private typesSection<'a> : Parser<unit, 'a> =
    pstring "---types---" >>. skipRestOfLine true

let private functionsSection<'a> : Parser<unit, 'a> =
    pstring "---functions---" >>. skipRestOfLine true

/// Field names referenced by every `flags.N?Type` conditional nested inside a type
/// expression — including one wrapped in `Vector<...>`/`vector<...>`, since the
/// vector still has to know which flag bit gates its presence.
let rec private conditionalFieldRefs (t: TlTypeExpr) : string list =
    match t with
    | TlTypeExpr.Conditional(fieldRef, _, inner) -> fieldRef :: conditionalFieldRefs inner
    | TlTypeExpr.Vector(_, inner) -> conditionalFieldRefs inner
    | TlTypeExpr.Bare _
    | TlTypeExpr.Boxed _
    | TlTypeExpr.TypeVar _
    | TlTypeExpr.Nat -> []

/// Parse a full combinator line:
/// `name#hexid {X:Type} param1:type1 param2:type2 = ResultType;`
///
/// Validates every `flags.N?Type` reference against the combinator's own `Params`
/// as the last parsing step: the grammar alone accepts any identifier before the
/// `.`, so a typo'd or renamed flags field (`flags2.0?T` in a combinator that only
/// declares `flags:#`) used to parse cleanly and fail only once the emitter tried
/// to read a field that isn't there. Failing here names the constructor and the
/// bad reference at the point the schema is read, not three stages downstream.
let combinator : Parser<TlCombinator, unit> =
    let combinatorId =
        namespacedIdent .>>. opt (attempt constructorId)

    let typeParams =
        many (attempt (ws >>. typeParam))

    let params' =
        many (attempt (ws >>. param))

    let resultType =
        ws >>. pchar '=' >>. ws >>. typeExpr .>> ws .>> pchar ';'

    combinatorId .>>. typeParams .>>. params' .>>. resultType
    >>= fun ((((ident, ctorId), typePs), parms), resType) ->
        let paramNames = parms |> List.map (fun p -> p.Name) |> Set.ofList
        let badRefs =
            parms
            |> List.collect (fun p -> conditionalFieldRefs p.Type)
            |> List.distinct
            |> List.filter (fun name -> not (paramNames.Contains name))

        match badRefs with
        | [] ->
            preturn {
                Id = ident
                ConstructorId = ctorId
                TypeParams = typePs
                Params = parms
                ResultType = resType
            }
        | bad ->
            let ctorName =
                match ident.Namespace with
                | Some ns -> $"%s{ns}.%s{ident.Name}"
                | None -> ident.Name
            let badList = String.concat ", " bad
            fail
                $"combinator '%s{ctorName}': flags reference(s) %s{badList} \
                  do not name a parameter declared on this constructor"

/// Represents a parsed line in the schema
type private SchemaLine =
    | TypesSectionMarker
    | FunctionsSectionMarker
    | Combinator of TlCombinator
    | LayerDirective of int
    | BlankOrComment

/// Parse a single schema line
let private schemaLine : Parser<SchemaLine, unit> =
    let blankLine = ws >>. skipNewline >>% BlankOrComment
    let commentLine = ws >>. comment >>% BlankOrComment
    let layerLine = ws >>. layerComment |>> LayerDirective
    let typesLine = ws >>. typesSection >>% TypesSectionMarker
    let functionsLine = ws >>. functionsSection >>% FunctionsSectionMarker
    let combinatorLine = ws >>. combinator |>> Combinator

    choice [
        attempt layerLine
        attempt commentLine
        attempt blankLine
        attempt typesLine
        attempt functionsLine
        combinatorLine
    ]

/// Parse an entire TL schema
let schema : Parser<TlSchema, unit> =
    many schemaLine .>> wsNl .>> eof
    |>> fun lines ->
        let (constructors, functions, layer, _) =
            lines |> List.fold (fun (ctors, funcs, layer, inFunctions) line ->
                match line with
                | TypesSectionMarker -> (ctors, funcs, layer, false)
                | FunctionsSectionMarker -> (ctors, funcs, layer, true)
                | LayerDirective n -> (ctors, funcs, Some n, inFunctions)
                | Combinator c ->
                    if inFunctions then (ctors, c :: funcs, layer, inFunctions)
                    else (c :: ctors, funcs, layer, inFunctions)
                | BlankOrComment -> (ctors, funcs, layer, inFunctions)
            ) ([], [], None, false)

        {
            Constructors = List.rev constructors
            Functions = List.rev functions
            Layer = layer
        }
