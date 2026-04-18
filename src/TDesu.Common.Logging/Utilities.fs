module TDesu.FSharp.Utilities

open System
open TDesu.FSharp.Operators
open Microsoft.Extensions.Logging
open Serilog
open Serilog.Extensions.Logging
open Serilog.Sinks.Grafana.Loki
open Serilog.Sinks.SystemConsole.Themes

[<RequireQualifiedAccess>]
module Logger =
    let getSerilogLogger () =
        LoggerConfiguration()

    /// Log-level resolution:
    ///   LOG_VERBOSE=1       → Verbose (dev debugging)
    ///   LOG_LEVEL=<name>    → explicit override (Verbose/Debug/Information/Warning/Error/Fatal)
    ///   otherwise           → Information (default, quiet enough for normal dev + prod)
    let private resolveMinimumLevel () =
        match Environment.GetEnvironmentVariable "LOG_LEVEL" with
        | null | "" ->
            match Environment.GetEnvironmentVariable "LOG_VERBOSE" with
            | null | "" | "0" | "false" -> Serilog.Events.LogEventLevel.Information
            | _ -> Serilog.Events.LogEventLevel.Verbose
        | s ->
            match s.Trim() with
            | "Verbose" | "Trace" -> Serilog.Events.LogEventLevel.Verbose
            | "Debug" -> Serilog.Events.LogEventLevel.Debug
            | "Information" | "Info" -> Serilog.Events.LogEventLevel.Information
            | "Warning" | "Warn" -> Serilog.Events.LogEventLevel.Warning
            | "Error" -> Serilog.Events.LogEventLevel.Error
            | "Fatal" -> Serilog.Events.LogEventLevel.Fatal
            | _ -> Serilog.Events.LogEventLevel.Information

    let configureSerilogLogger (factory: LoggerConfiguration) =
        let config =
            factory
                .Enrich.FromLogContext()
                .MinimumLevel.Is(resolveMinimumLevel ())
                .WriteTo.Console(
                    outputTemplate = "[{Timestamp:HH:mm:ss} {Level:u3} {SourceContext}] {Message:lj}{NewLine}{Exception}",
                    theme = AnsiConsoleTheme.Code
                )

        match Environment.GetEnvironmentVariable("LOKI_URL") with
        | null | "" -> config
        | lokiUrl ->
            let labels = [ LokiLabel(Key = "service_name", Value = "SedBot") ]
            config.WriteTo.GrafanaLoki(lokiUrl, labels = labels)

    let private resolveLoggerFactoryLevel () =
        match resolveMinimumLevel () with
        | Serilog.Events.LogEventLevel.Verbose -> LogLevel.Trace
        | Serilog.Events.LogEventLevel.Debug -> LogLevel.Debug
        | Serilog.Events.LogEventLevel.Information -> LogLevel.Information
        | Serilog.Events.LogEventLevel.Warning -> LogLevel.Warning
        | Serilog.Events.LogEventLevel.Error -> LogLevel.Error
        | _ -> LogLevel.Information

    let configureLoggerFactory (builder: ILoggingBuilder) =
        %builder
            .ClearProviders()
            .AddProvider(new SerilogLoggerProvider())
            .SetMinimumLevel(resolveLoggerFactoryLevel ())
        builder

    let private factory =
        let logger =
            getSerilogLogger ()
            |> configureSerilogLogger
            |> _.CreateLogger()

        Log.Logger <- logger

        LoggerFactory.Create(fun builder ->
            %configureLoggerFactory builder
        )

    /// Get the shared ILoggerFactory (for DI threading).
    let getFactory () : ILoggerFactory = factory

    /// Create an ILogger by category name (convenience, uses shared factory).
    let get name = factory.CreateLogger(name)
