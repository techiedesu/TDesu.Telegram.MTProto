namespace TDesu.MTProto

open System
open System.IO
open System.IO.Compression

/// Why `IGzipInflater.Inflate` could not produce a result.
[<RequireQualifiedAccess>]
type InflateError =
    /// The input is not a well-formed gzip member, or fails whatever validation the
    /// implementation itself performs (a corrupt header, a truncated stream it does detect, a
    /// CRC/ISIZE mismatch, ...). `reason` is diagnostic only; nothing matches on its text.
    | Malformed of reason: string
    /// Decompressing would need more than `maxOutputBytes`, the ceiling passed to `Inflate`.
    | ExceedsLimit of limit: int

/// Decompresses the `packed_data:bytes` of a gzip_packed message.
///
/// A gzip member can expand its compressed size by orders of magnitude, and the unpacked body can
/// itself be another gzip_packed, so an implementation MUST stop at or before `maxOutputBytes`
/// decompressed bytes rather than materialising an unbounded amount first — on a process that
/// never restarts, one small frame would otherwise become unbounded memory. Malformed input MUST
/// come back as `Error`, never as a thrown exception: `MtProtoClient` drops the one message that
/// failed to decompress and keeps the connection, which only works if a bad `packed_data` cannot
/// unwind the caller instead.
type IGzipInflater =
    abstract Inflate: packed: ReadOnlySpan<byte> * maxOutputBytes: int -> Result<byte[], InflateError>

/// Built-in `IGzipInflater` implementations.
module GzipInflater =

    /// Unwinds `platform`'s read loop into an `ExceedsLimit` result instead of letting a
    /// `failwith` tear down the caller — the one behaviour change versus the `inflate` this
    /// replaced.
    exception private OutputLimitExceeded

    /// `System.IO.Compression.GZipStream`, capped at `maxOutputBytes`. This was `MtProtoClient`'s
    /// only inflater before inflaters became pluggable, and is still its default.
    ///
    /// Two things worth knowing before comparing this against another `IGzipInflater`: `GZipStream`
    /// decodes every concatenated gzip member it finds back to back (RFC 1952 permits but does not
    /// require this of a conformant decompressor), and it does not reliably detect a stream that
    /// simply ends early — for a cut-off member it tends to return `Ok` with however many bytes it
    /// had already decoded rather than `Error`. Both are `GZipStream`'s own behaviour, unchanged by
    /// this wrapper.
    let platform: IGzipInflater =
        { new IGzipInflater with
            member _.Inflate(packed: ReadOnlySpan<byte>, maxOutputBytes: int) =
                try
                    // GZipStream has no Span-based constructor; this copy is the one the client
                    // has always paid here, now explicit at the boundary instead of hidden inside it.
                    use input = new MemoryStream(packed.ToArray())
                    use gz = new GZipStream(input, CompressionMode.Decompress)

                    // Grown once to a plausible size rather than doubled from nothing. A
                    // MemoryStream that doubles into a multi-megabyte result allocates every
                    // intermediate size on the large object heap, on a process that never restarts.
                    use output = new MemoryStream(int (min (int64 packed.Length * 4L) (int64 (max 0 maxOutputBytes))))
                    let buffer = Array.zeroCreate<byte> 81920
                    let mutable total = 0
                    let mutable read = gz.Read(buffer, 0, buffer.Length)

                    while read > 0 do
                        // Compared before adding, so a ceiling near Int32.MaxValue cannot wrap.
                        if read > maxOutputBytes - total then
                            raise OutputLimitExceeded

                        total <- total + read

                        output.Write(buffer, 0, read)
                        read <- gz.Read(buffer, 0, buffer.Length)

                    Ok(output.ToArray())
                with
                | OutputLimitExceeded -> Error(InflateError.ExceedsLimit maxOutputBytes)
                | :? InvalidDataException as ex -> Error(InflateError.Malformed ex.Message)
                | :? IOException as ex -> Error(InflateError.Malformed ex.Message) }
