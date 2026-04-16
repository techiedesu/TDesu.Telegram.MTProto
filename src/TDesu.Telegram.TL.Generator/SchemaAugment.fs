namespace TDesu.Telegram.TL.Generator

open FParsec
open Microsoft.Extensions.Logging
open TDesu.FSharp.Utilities
open TDesu.Telegram.TL.AST
open TDesu.Telegram.TL.AST.Parsers.CombinatorParsers
open TDesu.Telegram.TL.Generator.Overrides

/// Fold `[[extra_combinators]]` entries from the overrides config into a
/// `TlSchema` by parsing each `raw` combinator and appending to the
/// appropriate section (Constructors/Functions).
///
/// Conflict policy: if an extra combinator's ConstructorId collides with one
/// already present in the schema, the extra wins and the schema entry is
/// dropped with a warning. Extras without a `#hexid` are rejected — anonymous
/// combinators would compute a CRC-32 ID from the raw text, which is fragile
/// and surprising here.
module SchemaAugment =

    let private log = Logger.get "SchemaAugment"

    let private parseRaw (raw: string) : Result<TlCombinator, string> =
        match run combinator raw with
        | Success(c, _, _) ->
            match c.ConstructorId with
            | Some _ -> Result.Ok c
            | None ->
                Result.Error $"missing explicit `#hexid` in extra combinator: {raw}"
        | Failure(msg, _, _) ->
            Result.Error $"parse error in extra combinator: {msg}"

    let private ctorIdOf (c: TlCombinator) : uint32 option =
        c.ConstructorId |> Option.map (fun (TlConstructorId id) -> id)

    /// Apply every `[[extra_combinators]]` entry from `config` to `schema`.
    /// Returns the augmented schema. Failures on any individual extra are
    /// fatal so the build fails loudly rather than silently dropping entries.
    let fold (config: OverrideConfig) (schema: TlSchema) : TlSchema =
        if List.isEmpty config.ExtraCombinators then
            schema
        else
            let parsed =
                config.ExtraCombinators
                |> List.map (fun entry ->
                    match parseRaw entry.Raw with
                    | Result.Ok c -> c, entry.Section
                    | Result.Error msg -> failwith msg)

            let extraCids =
                parsed
                |> List.choose (fun (c, _) -> ctorIdOf c)
                |> Set.ofList

            let removeColliding (xs: TlCombinator list) : TlCombinator list =
                xs
                |> List.filter (fun c ->
                    match ctorIdOf c with
                    | Some cid when extraCids.Contains cid ->
                        log.LogWarning(
                            "schema combinator {Name} with CID 0x{Cid:x8} overridden by [[extra_combinators]]",
                            c.Id.Name, cid)
                        false
                    | _ -> true)

            let baseConstructors = removeColliding schema.Constructors
            let baseFunctions = removeColliding schema.Functions

            let extraConstructors =
                parsed |> List.choose (fun (c, s) -> if s = Constructors then Some c else None)
            let extraFunctions =
                parsed |> List.choose (fun (c, s) -> if s = Functions then Some c else None)

            log.LogInformation(
                "augmented schema: +{Ctors} constructors, +{Fns} functions from [[extra_combinators]]",
                extraConstructors.Length, extraFunctions.Length)

            { schema with
                Constructors = baseConstructors @ extraConstructors
                Functions = baseFunctions @ extraFunctions }
