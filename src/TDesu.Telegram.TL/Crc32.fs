namespace TDesu.Telegram.TL

open TDesu.FSharp.Operators

module Crc32 =

    open TDesu.Telegram.TL.AST

    /// Standard CRC32 lookup table (ISO 3309 / ITU-T V.42, polynomial 0xEDB88320)
    let private table =
        [| for i in 0u .. 255u do
            let mutable crc = i
            for _ in 0 .. 7 do
                if crc &&& 1u <> 0u then
                    crc <- (crc >>> 1) ^^^ 0xEDB88320u
                else
                    crc <- crc >>> 1
            crc |]

    /// Compute CRC32 for the given string (UTF-8 encoded)
    let compute (input: string) : uint32 =
        let bytes = System.Text.Encoding.UTF8.GetBytes(input)
        let crc =
            bytes |> Array.fold (fun crc b ->
                let index = (crc ^^^ uint32 b) &&& 0xFFu
                (crc >>> 8) ^^^ table[int index]) 0xFFFFFFFFu
        crc ^^^ 0xFFFFFFFFu

    /// Format a TlTypeExpr as its canonical string representation
    let private formatTypeExpr (expr: TlTypeExpr) =
        let rec fmt expr =
            match expr with
            | TlTypeExpr.Bare id ->
                match id.Namespace with
                | Some ns -> $"{ns}.{id.Name}"
                | None -> id.Name
            | TlTypeExpr.Boxed id ->
                match id.Namespace with
                | Some ns -> $"{ns}.{id.Name}"
                | None -> id.Name
            | TlTypeExpr.TypeVar name -> $"!{name}"
            | TlTypeExpr.Vector inner -> $"Vector {fmt inner}"
            | TlTypeExpr.Nat -> "#"
            | TlTypeExpr.Conditional (fieldRef, bitIndex, inner) ->
                $"{fieldRef}.{bitIndex}?{fmt inner}"
        fmt expr

    /// Format a TlIdentifier
    let private formatIdent (id: TlIdentifier) =
        match id.Namespace with
        | Some ns -> $"{ns}.{id.Name}"
        | None -> id.Name

    /// Build the canonical string for a combinator and compute its CRC32.
    /// Canonical form: `name params = ResultType`
    let computeForCombinator (c: TlCombinator) : uint32 =
        let sb = System.Text.StringBuilder()

        %sb.Append(formatIdent c.Id)

        for p in c.Params do
            %sb.Append(' ').Append(p.Name).Append(':').Append(formatTypeExpr p.Type)

        %sb.Append(" = ").Append(formatTypeExpr c.ResultType)

        compute (sb.ToString())
