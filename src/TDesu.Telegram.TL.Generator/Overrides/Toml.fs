namespace TDesu.Telegram.TL.Generator.Overrides

open System
open System.IO
open System.Reflection
open Tomlyn
open Tomlyn.Model

/// Load OverrideConfig from TOML format.
module Toml =

    let private getStringOr (key: string) (defaultValue: string) (table: TomlTable) =
        match table.TryGetValue(key) with
        | true, (:? string as v) -> v
        | _ -> defaultValue

    let private getString (key: string) (table: TomlTable) =
        match table.TryGetValue(key) with
        | true, (:? string as v) -> v
        | _ -> failwith $"Missing required string key '{key}'"

    let private getUint32 (key: string) (table: TomlTable) : uint32 =
        match table.TryGetValue(key) with
        | true, (:? int64 as v) -> uint32 v
        | true, (:? int32 as v) -> uint32 v
        | _ -> failwith $"Missing required uint32 key '{key}'"

    let private getInt (key: string) (table: TomlTable) : int =
        match table.TryGetValue(key) with
        | true, (:? int64 as v) -> int v
        | true, (:? int32 as v) -> v
        | _ -> failwith $"Missing required int key '{key}'"

    let private tryGetInt (key: string) (table: TomlTable) : int option =
        match table.TryGetValue(key) with
        | true, (:? int64 as v) -> Some (int v)
        | true, (:? int32 as v) -> Some v
        | _ -> None

    let private getStringArray (key: string) (table: TomlTable) : string list =
        match table.TryGetValue(key) with
        | true, (:? TomlArray as arr) ->
            [ for item in arr -> item :?> string ]
        | _ -> []

    let private getTableArray (key: string) (table: TomlTable) : TomlTable list =
        match table.TryGetValue(key) with
        | true, (:? TomlTableArray as arr) ->
            [ for item in arr -> item ]
        | _ -> []

    let private parseLayerVariant (table: TomlTable) : LayerVariant =
        let name = table |> getString "name"
        let variants =
            match table.TryGetValue("variants") with
            | true, (:? TomlArray as arr) ->
                [ for item in arr do
                    let vt = item :?> TomlTable
                    let maxLayer = vt |> tryGetInt "max_layer"
                    let cid = vt |> getUint32 "cid"
                    yield maxLayer, cid ]
            | _ -> []
        { Name = name; Variants = variants }

    let private parseAlias (table: TomlTable) : CidAlias =
        let name = table |> getString "name"
        let cids =
            match table.TryGetValue("cids") with
            | true, (:? TomlArray as arr) ->
                [ for item in arr -> uint32 (item :?> int64) ]
            | _ -> []
        { Name = name; Cids = cids }

    let private parseExtra (table: TomlTable) : ExtraCid =
        { Name = table |> getString "name"
          Cid = table |> getUint32 "cid"
          Comment = table |> getStringOr "comment" "" }

    let private parseLayerTypeInfo (table: TomlTable) : Map<string, LayerTypeFlags2> =
        match table.TryGetValue("layer_type_info") with
        | true, (:? TomlTable as info) ->
            [ for kv in info do
                let typeName = kv.Key
                let props = kv.Value :?> TomlTable
                let flags2 = { Flags2MinLayer = props |> getInt "flags2_min_layer" }
                yield typeName, flags2 ]
            |> Map.ofList
        | _ -> Map.empty

    let private parseWhitelists (table: TomlTable) =
        match table.TryGetValue("whitelists") with
        | true, (:? TomlTable as wl) ->
            let types = wl |> getStringArray "types" |> Set.ofList
            let writers = wl |> getStringArray "writers" |> Set.ofList
            let writerLayerTypes = wl |> getStringArray "writer_layer_types" |> Set.ofList
            let stubTypes = wl |> getStringArray "stub_types" |> Set.ofList
            let clientParsers = wl |> getStringArray "client_parsers" |> Set.ofList
            let writerRecordPerCaseUnions = wl |> getStringArray "writer_record_per_case_unions" |> Set.ofList
            types, writers, writerLayerTypes, stubTypes, clientParsers, writerRecordPerCaseUnions
        | _ -> Set.empty, Set.empty, Set.empty, Set.empty, Set.empty, Set.empty

    /// Parse TOML string into OverrideConfig.
    let load (toml: string) : OverrideConfig =
        let doc = Tomlyn.TomlSerializer.Deserialize<TomlTable>(toml)

        let layerVariants = doc |> getTableArray "layer_variants" |> List.map parseLayerVariant
        let aliases = doc |> getTableArray "aliases" |> List.map parseAlias
        let extras = doc |> getTableArray "extras" |> List.map parseExtra
        let layerTypeInfo = doc |> parseLayerTypeInfo
        let typeWhitelist, writerWhitelist, writerLayerTypes, stubTypes, clientParsers, writerRecordPerCaseUnions =
            doc |> parseWhitelists

        { LayerVariants = layerVariants
          Aliases = aliases
          Extras = extras
          LayerTypeInfo = layerTypeInfo
          TypeWhitelist = typeWhitelist
          WriterWhitelist = writerWhitelist
          WriterLayerTypes = writerLayerTypes
          StubTypes = stubTypes
          ClientParserWhitelist = clientParsers
          WriterRecordPerCaseUnions = writerRecordPerCaseUnions }

    /// Load OverrideConfig from a TOML file.
    let loadFile (path: string) : OverrideConfig =
        File.ReadAllText(path) |> load
