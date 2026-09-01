namespace TDesu.Telegram.TL

open System
open System.Runtime.CompilerServices
open TDesu.Telegram.TL.AST

/// C#-friendly API for TL schema parsing.
[<AbstractClass; Sealed>]
type TlParser =

    /// Comment out the declarations in a real Telegram schema that the grammar does not accept.
    ///
    /// They are all TL describing its own primitives rather than Telegram describing its API: the
    /// `vector` container with a type parameter and `[ t ]` multiplicity, `int ? = Int;`, the
    /// `4*[ int ]` fixed-width forms, and the `vector<%Message>` bare sigil. None of them carries a
    /// constructor anyone deserializes, and every one of them appears in the first twenty lines of
    /// `api.tl`.
    ///
    /// Here rather than in a caller because `AstFactory.parse` on an unmodified `api.tl` fails on
    /// line 6, which makes the documented entry point useless against the only schema anyone has.
    /// The generator carried this privately for exactly that reason; a second consumer would have
    /// copied it and then drifted.
    static member Preprocess(text: string) : string =
        text.Split('\n')
        |> Array.map (fun line ->
            let trimmed = line.TrimStart()

            let unparseable =
                (trimmed.StartsWith "vector#" && trimmed.Contains "[ t ]")
                || (trimmed.StartsWith "vector " && trimmed.Contains "[ t ]")
                || trimmed.Contains " ? = "
                || trimmed.Contains "*[ "
                || trimmed.Contains "<%"

            if unparseable then "//" + line else line)
        |> String.concat "\n"

    /// Parse a TL schema string as given. Throws FormatException on parse error.
    ///
    /// For a schema downloaded from Telegram use `ParseSchema`, which preprocesses first.
    static member Parse(input: string) : TlSchema =
        match AstFactory.parse input with
        | Ok schema -> schema
        | Error msg -> raise (FormatException($"TL schema parse error: %s{msg}"))

    /// Parse a real Telegram schema: preprocess, then parse.
    static member ParseSchema(input: string) : TlSchema =
        TlParser.Parse(TlParser.Preprocess input)

    /// Try to parse a TL schema string. Returns true on success.
    static member TryParse(input: string, [<Runtime.InteropServices.Out>] schema: TlSchema byref) : bool =
        match AstFactory.parse input with
        | Ok s -> schema <- s; true
        | Error _ -> schema <- Unchecked.defaultof<_>; false

/// Extensions for option types used in AST — makes them nullable-friendly for C#.
[<Extension>]
type TlAstExtensions =

    /// Get the constructor ID as a nullable uint32.
    [<Extension>]
    static member GetConstructorId(combinator: TlCombinator) : Nullable<uint32> =
        match combinator.ConstructorId with
        | Some(TlConstructorId id) -> Nullable id
        | None -> Nullable()

    /// Get the namespace, or null if none.
    [<Extension>]
    static member GetNamespace(id: TlIdentifier) : string =
        match id.Namespace with
        | Some ns -> ns
        | None -> null

    /// Get the layer number as a nullable int.
    [<Extension>]
    static member GetLayer(schema: TlSchema) : Nullable<int> =
        match schema.Layer with
        | Some n -> Nullable n
        | None -> Nullable()

    /// Get constructors as an array (friendlier than F# list for C#).
    [<Extension>]
    static member GetConstructors(schema: TlSchema) : TlCombinator array =
        schema.Constructors |> List.toArray

    /// Get functions as an array.
    [<Extension>]
    static member GetFunctions(schema: TlSchema) : TlCombinator array =
        schema.Functions |> List.toArray

    /// Get params as an array.
    [<Extension>]
    static member GetParams(combinator: TlCombinator) : TlParam array =
        combinator.Params |> List.toArray
