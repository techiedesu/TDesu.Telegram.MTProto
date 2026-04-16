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

    /// Which section of the TL schema an extra combinator belongs to.
    type SchemaSection =
        | Constructors
        | Functions

    /// A full TL combinator (`name#cid f:T ... = Result;`) not present in the
    /// primary schema, contributed via overrides. Parsed at load time by the
    /// same `CombinatorParsers.combinator` used for the main schema, then
    /// folded into `TlSchema.Constructors` or `.Functions` based on `Section`.
    type ExtraCombinator = {
        /// The raw TL combinator syntax. Parsed verbatim; the leading `---types---`
        /// / `---functions---` framing is NOT supported here — use `Section` instead.
        Raw: string
        /// Which list to fold this combinator into. Required because the
        /// combinator grammar alone doesn't carry that distinction.
        Section: SchemaSection
        /// Optional human-readable note (not emitted anywhere).
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
        ExtraCombinators: ExtraCombinator list
        LayerTypeInfo: Map<string, LayerTypeFlags2>
        TypeWhitelist: Set<string>
        WriterWhitelist: Set<string>
        WriterLayerTypes: Set<string>
        StubTypes: Set<string>
        /// Constructor names to emit response parsers for (used by the
        /// `client-parsers` target). When empty, the target produces an empty
        /// module. Replaces the previously hardcoded ["Message"; "User"; "Chat"]
        /// in Pipeline.generateClientParsers.
        ClientParserWhitelist: Set<string>
        /// PascalCase result-type names for which the writers target should
        /// emit a per-case record `Write{Type}{Case}Params` and reference it
        /// from the union case (`| {Case} of Write{Type}{Case}Params`),
        /// instead of the default positional union form
        /// (`| {Case} of f1: T1 * ... * fn: Tn`). Useful for unions with many
        /// fields where positional construction at callsites is unreadable
        /// — callers can use record-with syntax instead.
        WriterRecordPerCaseUnions: Set<string>
    }

    module OverrideConfig =
        let empty = {
            LayerVariants = []
            Aliases = []
            Extras = []
            ExtraCombinators = []
            LayerTypeInfo = Map.empty
            TypeWhitelist = Set.empty
            WriterWhitelist = Set.empty
            WriterLayerTypes = Set.empty
            StubTypes = Set.empty
            ClientParserWhitelist = Set.empty
            WriterRecordPerCaseUnions = Set.empty
        }

        /// Merge overlay on top of base config.
        /// Lists are concatenated, sets are unioned, maps are merged (overlay wins).
        let merge (baseConfig: OverrideConfig) (overlay: OverrideConfig) : OverrideConfig = {
            LayerVariants = baseConfig.LayerVariants @ overlay.LayerVariants
            Aliases = baseConfig.Aliases @ overlay.Aliases
            Extras = baseConfig.Extras @ overlay.Extras
            ExtraCombinators = baseConfig.ExtraCombinators @ overlay.ExtraCombinators
            LayerTypeInfo =
                Map.fold (fun acc k v -> Map.add k v acc) baseConfig.LayerTypeInfo overlay.LayerTypeInfo
            TypeWhitelist = Set.union baseConfig.TypeWhitelist overlay.TypeWhitelist
            WriterWhitelist = Set.union baseConfig.WriterWhitelist overlay.WriterWhitelist
            WriterLayerTypes = Set.union baseConfig.WriterLayerTypes overlay.WriterLayerTypes
            StubTypes = Set.union baseConfig.StubTypes overlay.StubTypes
            ClientParserWhitelist = Set.union baseConfig.ClientParserWhitelist overlay.ClientParserWhitelist
            WriterRecordPerCaseUnions = Set.union baseConfig.WriterRecordPerCaseUnions overlay.WriterRecordPerCaseUnions
        }
