namespace TDesu.Telegram.TL.Generator

open System.IO
open System.Net.Http
open Microsoft.Extensions.Logging
open FSharp.Data
open TDesu.FSharp
open TDesu.FSharp.Operators
open TDesu.FSharp.IO
open TDesu.FSharp.Utilities

module Downloader =

    let private hc = new HttpClient()

    let private mtprotoUrl = "https://core.telegram.org/schema/mtproto"
    let private apiUrl = "https://core.telegram.org/schema"

    let private downloadAndExtractPre (url: string) (cachePath: string) = task {
        let log = Logger.get "Downloader.downloadAndExtractPre"
        log.LogInformation("Downloading {Url}", url)
        let! html = hc.GetStringAsync(url)
        let doc = HtmlDocument.Parse(html)
        let preText =
            doc.Descendants("pre")
            |> Seq.head
            |> HtmlNode.innerText
        do! File.WriteAllTextAsync(cachePath, preText)
        log.LogInformation("Cached to {Path}", cachePath)
    }

    let downloadIfNotCached () = task {
        let log = Logger.get "Downloader.downloadIfNotCached"

        if Directory.notExists "cached" then
            Directory.create "cached"
            log.LogInformation("Created cached directory")

        if File.notExists "cached/mtproto.tl" then
            do! downloadAndExtractPre mtprotoUrl "cached/mtproto.tl"

        if File.notExists "cached/api.tl" then
            do! downloadAndExtractPre apiUrl "cached/api.tl"

        log.LogInformation("Done!")
    }

    /// Preprocess TL schema: comment out lines the parser can't handle.
    let preprocess (text: string) =
        text.Split('\n')
        |> Array.map (fun line ->
            let trimmed = line.TrimStart()
            // vector#... {t:Type} # [ t ] = Vector t; — special syntax not in FParsec grammar
            if trimmed.StartsWith("vector#") && trimmed.Contains("[ t ]") then
                "//" + line
            // Bare type "vector {t:Type} # [ t ] = Vector t;" (no CID)
            elif trimmed.StartsWith("vector ") && trimmed.Contains("[ t ]") then
                "//" + line
            // Primitive type definitions: "int ? = Int;", "long ? = Long;", etc.
            elif trimmed.Contains(" ? = ") then
                "//" + line
            // Array-style definitions: "int128 4*[ int ] = Int128;", etc.
            elif trimmed.Contains("*[ ") then
                "//" + line
            // Bare type sigil: vector<%Message> — not handled by parser
            elif trimmed.Contains("<%") then
                "//" + line
            else line)
        |> String.concat "\n"

    let getMtprotoSchema () =
        File.ReadAllText("cached/mtproto.tl") |> preprocess

    let getApiSchema () =
        File.ReadAllText("cached/api.tl") |> preprocess
