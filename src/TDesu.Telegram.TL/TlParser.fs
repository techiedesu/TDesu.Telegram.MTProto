namespace TDesu.Telegram.TL

open System
open System.Runtime.CompilerServices
open TDesu.Telegram.TL.AST

/// C#-friendly API for TL schema parsing.
[<AbstractClass; Sealed>]
type TlParser =

    /// Parse a TL schema string. Throws FormatException on parse error.
    static member Parse(input: string) : TlSchema =
        match AstFactory.parse input with
        | Ok schema -> schema
        | Error msg -> raise (FormatException($"TL schema parse error: %s{msg}"))

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
