namespace TDesu.Telegram.TL.Generator.Overrides

/// Data model for CID override configuration.
/// Loaded from TOML files or constructed via the F# DSL.
[<AutoOpen>]
module Types =

    /// A CID that varies by negotiated layer.
    type LayerVariant = {
        Name: string
        /// (maxLayer option * cid) — None means "default" (no upper bound).
        Variants: (int option * uint32) list
    }

    /// Multiple known CIDs for the same RPC method (different client layers).
    type CidAlias = {
        /// PascalCase name used in code (e.g. "MessagesSendMessage")
        Name: string
        /// All known CIDs for this method. First is the "primary".
        Cids: uint32 list
    }

    /// Extra CID not present in the schema at all.
    type ExtraCid = {
        Name: string
        Cid: uint32
        Comment: string
    }

    /// Layer-dependent type metadata for response serialization.
    type LayerTypeFlags2 = {
        /// Minimum layer at which flags2 field exists.
        Flags2MinLayer: int
    }

    /// Complete override configuration.
    type OverrideConfig = {
        LayerVariants: LayerVariant list
        Aliases: CidAlias list
        Extras: ExtraCid list
        LayerTypeInfo: Map<string, LayerTypeFlags2>
        TypeWhitelist: Set<string>
        WriterWhitelist: Set<string>
        WriterLayerTypes: Set<string>
        StubTypes: Set<string>
    }

    module OverrideConfig =
        let empty = {
            LayerVariants = []
            Aliases = []
            Extras = []
            LayerTypeInfo = Map.empty
            TypeWhitelist = Set.empty
            WriterWhitelist = Set.empty
            WriterLayerTypes = Set.empty
            StubTypes = Set.empty
        }

        /// Merge overlay on top of base config.
        /// Lists are concatenated, sets are unioned, maps are merged (overlay wins).
        let merge (baseConfig: OverrideConfig) (overlay: OverrideConfig) : OverrideConfig = {
            LayerVariants = baseConfig.LayerVariants @ overlay.LayerVariants
            Aliases = baseConfig.Aliases @ overlay.Aliases
            Extras = baseConfig.Extras @ overlay.Extras
            LayerTypeInfo =
                Map.fold (fun acc k v -> Map.add k v acc) baseConfig.LayerTypeInfo overlay.LayerTypeInfo
            TypeWhitelist = Set.union baseConfig.TypeWhitelist overlay.TypeWhitelist
            WriterWhitelist = Set.union baseConfig.WriterWhitelist overlay.WriterWhitelist
            WriterLayerTypes = Set.union baseConfig.WriterLayerTypes overlay.WriterLayerTypes
            StubTypes = Set.union baseConfig.StubTypes overlay.StubTypes
        }
