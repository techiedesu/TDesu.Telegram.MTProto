module TDesu.FSharp.Utilities

open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module Logger =
    let private factory =
        LoggerFactory.Create(fun builder ->
            builder
                .SetMinimumLevel(LogLevel.Debug)
                .AddSimpleConsole(fun opts ->
                    opts.SingleLine <- true
                    opts.TimestampFormat <- "[HH:mm:ss] "
                )
            |> ignore
        )

    let get name = factory.CreateLogger(name)
