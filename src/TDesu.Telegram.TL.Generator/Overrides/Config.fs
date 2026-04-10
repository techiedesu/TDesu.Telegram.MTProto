namespace TDesu.Telegram.TL.Generator.Overrides

/// Load and access override configuration.
module Config =
    /// Load defaults from embedded TOML resource.
    let loadDefaults () : OverrideConfig = Toml.loadEmbedded ()

    /// Load defaults and merge with overlay from file.
    let loadWithOverlay (path: string) : OverrideConfig =
        let baseConfig = Toml.loadEmbedded ()
        let overlay = Toml.loadFile path
        OverrideConfig.merge baseConfig overlay
