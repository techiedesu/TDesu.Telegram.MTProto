namespace TDesu.Telegram.TL.Generator

open System.IO
open System.Net.Http
open Microsoft.Extensions.Logging
open FSharp.Data
open TDesu.FSharp
open TDesu.FSharp.Operators
open TDesu.FSharp.IO
open TDesu.FSharp.Utilities
open TDesu.Telegram.TL

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
    ///
    /// Delegated to `TlParser.Preprocess`. It used to live here, which meant `AstFactory.parse` was
    /// unusable against an unmodified api.tl for anyone outside this project — and a second consumer
    /// would have copied these five rules and then drifted from them.
    let preprocess (text: string) = TlParser.Preprocess text

    let getMtprotoSchema () =
        File.ReadAllText("cached/mtproto.tl") |> preprocess

    let getApiSchema () =
        File.ReadAllText("cached/api.tl") |> preprocess
