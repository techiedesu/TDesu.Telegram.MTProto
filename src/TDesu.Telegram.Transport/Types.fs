namespace TDesu.Transport

open System.Net

[<RequireQualifiedAccess>]
type TransportError =
    | ConnectionFailed of message: string
    | ConnectionClosed
    | ReadError of message: string
    | WriteError of message: string
    | InvalidFrame of message: string
    | Timeout

/// IPAddress doesn't satisfy F# structural-comparison constraint, so the
/// record can't get auto-derived comparison either. Equality stays.
[<NoComparison>]
type DataCenter = {
    Id: int
    Address: IPAddress
    Port: int
}

module DataCenters =

    let private dc id address port = {
        Id = id
        Address = IPAddress.Parse(address: string)
        Port = port
    }

    let production = [
        dc 1 "149.154.175.53" 443
        dc 2 "149.154.167.51" 443
        dc 3 "149.154.175.100" 443
        dc 4 "149.154.167.91" 443
        dc 5 "91.108.56.130" 443
    ]

    let test = [
        dc 1 "149.154.175.10" 443
        dc 2 "149.154.167.40" 443
        dc 3 "149.154.175.117" 443
    ]

    let defaultDc = production[1]
    let defaultTestDc = test[1]
