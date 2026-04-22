namespace TDesu.Telegram.TL.Generator.Overrides

/// F# computation expression builder for override configuration.
module Dsl =

    type OverrideBuilder() =
        member _.Yield(_) = OverrideConfig.empty

        [<CustomOperation("layerVariant")>]
        member _.LayerVariant(config, name, variants) =
            { config with
                LayerVariants = config.LayerVariants @ [ { Name = name; Variants = variants } ] }

        [<CustomOperation("structuralOverlay")>]
        member _.StructuralOverlay(config, name, maxOldLayer, extras) =
            { config with
                StructuralOverlays =
                    config.StructuralOverlays @ [ { Name = name; MaxOldLayer = maxOldLayer; ExtraFields = extras } ] }

        [<CustomOperation("alias")>]
        member _.Alias(config, name, cids) =
            { config with
                Aliases = config.Aliases @ [ { Name = name; Cids = cids } ] }

        [<CustomOperation("extra")>]
        member _.Extra(config, name, cid, comment) =
            { config with
                Extras = config.Extras @ [ { Name = name; Cid = cid; Comment = comment } ] }

        [<CustomOperation("extraCombinator")>]
        member _.ExtraCombinator(config, raw: string, section: SchemaSection, comment: string) =
            { config with
                ExtraCombinators =
                    config.ExtraCombinators @ [ { Raw = raw; Section = section; Comment = comment } ] }

        [<CustomOperation("layerTypeInfo")>]
        member _.LayerTypeInfo(config, name, flags2MinLayer) =
            { config with
                LayerTypeInfo = config.LayerTypeInfo |> Map.add name { Flags2MinLayer = flags2MinLayer } }

        [<CustomOperation("whitelistTypes")>]
        member _.WhitelistTypes(config, types: string list) =
            { config with
                TypeWhitelist = Set.union config.TypeWhitelist (Set.ofList types) }

        [<CustomOperation("whitelistWriters")>]
        member _.WhitelistWriters(config, writers: string list) =
            { config with
                WriterWhitelist = Set.union config.WriterWhitelist (Set.ofList writers) }

        [<CustomOperation("writerLayerTypes")>]
        member _.WriterLayerTypes(config, types: string list) =
            { config with
                WriterLayerTypes = Set.union config.WriterLayerTypes (Set.ofList types) }

        [<CustomOperation("stubTypes")>]
        member _.StubTypes(config, types: string list) =
            { config with
                StubTypes = Set.union config.StubTypes (Set.ofList types) }

    let overrides = OverrideBuilder()
