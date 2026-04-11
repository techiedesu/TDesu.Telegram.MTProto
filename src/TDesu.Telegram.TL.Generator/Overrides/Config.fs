namespace TDesu.Telegram.TL.Generator.Overrides

/// Load and access override configuration.
///
/// As of 0.1.0 the generator no longer ships with embedded defaults — every
/// invocation supplies its own TOML overrides via `--overrides`. The
/// `samples/sedbot-overrides.toml` file documents the schema and is the
/// previous embedded default verbatim if you need a starting point.
module Config =
    /// Load an OverrideConfig from a TOML file.
    let load (path: string) : OverrideConfig = Toml.loadFile path

    /// Load multiple TOML files and merge them in order (later overrides earlier).
    /// Useful when composing a base config with project-specific tweaks.
    let loadMerged (paths: string list) : OverrideConfig =
        paths
        |> List.map Toml.loadFile
        |> List.fold OverrideConfig.merge OverrideConfig.empty
