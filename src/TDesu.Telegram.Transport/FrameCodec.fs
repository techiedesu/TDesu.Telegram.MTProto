namespace TDesu.Transport

open System
open TDesu.FSharp
open TDesu.FSharp.Operators
/// TCP Intermediate transport frame codec.
/// Header (sent once): 0xeeeeeeee
/// Frame format: 4-byte LE payload length + payload
module FrameCodec =

    let [<Literal>] private IntermediateHeader = 0xeeeeeeeeu

    /// Generate the one-time intermediate transport header
    let encodeHeader () : byte[] =
        BitConverter.GetBytes(IntermediateHeader)

    /// Encode a payload into a frame: 4-byte LE length prefix + payload
    let encodeFrame (payload: byte[]) : byte[] =
        let length = payload.Length
        let frame = Array.zeroCreate<byte> (4 + length)
        let lengthBytes = BitConverter.GetBytes(int32 length)
        Buffer.BlockCopy(lengthBytes, 0, frame, 0, 4)
        Buffer.BlockCopy(payload, 0, frame, 4, length)
        frame

    /// Decode frame length from the 4-byte LE length prefix
    let decodeFrameLength (header: byte[]) : Result<int, TransportError> =
        if header.Length < 4 then
            Error (TransportError.InvalidFrame "Frame header must be at least 4 bytes")
        else
            let length = BitConverter.ToInt32(header, 0)
            if length <= 0 || length > 16 * 1024 * 1024 then
                Error (TransportError.InvalidFrame $"Invalid frame length: %d{length}")
            else
                Ok length
