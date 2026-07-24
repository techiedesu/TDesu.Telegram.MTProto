namespace TDesu.Transport

open System
open TDesu.FSharp
open TDesu.FSharp.Operators
/// MTProto transport frame codecs.
///
/// Intermediate: one-time header 0xeeeeeeee, then 4-byte LE payload length + payload.
/// Abridged:     one-time header 0xef, then a 1- or 4-byte length (payload_len / 4).
module FrameCodec =

    let [<Literal>] private MaxFrameLength = 16 * 1024 * 1024

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
            if length <= 0 || length > MaxFrameLength then
                Error (TransportError.InvalidFrame $"Invalid frame length: %d{length}")
            else
                Ok length

    /// Abridged framing: length is the payload length divided by 4, encoded as a
    /// single byte when < 0x7f, otherwise 0x7f followed by a 3-byte LE count.
    /// MTProto payloads are always a multiple of 4, so the division is exact.
    module Abridged =

        let [<Literal>] private AbridgedHeader = 0xefuy
        let [<Literal>] private Extended = 0x7fuy

        /// Generate the one-time abridged transport header (single 0xef byte).
        let encodeHeader () : byte[] = [| AbridgedHeader |]

        /// Encode a payload into an abridged frame.
        let encodeFrame (payload: byte[]) : byte[] =
            let words = payload.Length / 4

            if words < int Extended then
                let frame = Array.zeroCreate<byte> (1 + payload.Length)
                frame[0] <- byte words
                Buffer.BlockCopy(payload, 0, frame, 1, payload.Length)
                frame
            else
                let frame = Array.zeroCreate<byte> (4 + payload.Length)
                frame[0] <- Extended
                frame[1] <- byte (words &&& 0xff)
                frame[2] <- byte ((words >>> 8) &&& 0xff)
                frame[3] <- byte ((words >>> 16) &&& 0xff)
                Buffer.BlockCopy(payload, 0, frame, 4, payload.Length)
                frame

        /// True when the first length byte signals a 3-byte extended length follows.
        let isExtended (firstByte: byte) : bool = firstByte = Extended

        /// Payload length from the single length byte (non-extended form).
        let shortLength (firstByte: byte) : Result<int, TransportError> =
            let length = int firstByte * 4
            if length <= 0 || length > MaxFrameLength then
                Error (TransportError.InvalidFrame $"Invalid abridged frame length: %d{length}")
            else
                Ok length

        /// Payload length from the 3-byte LE extended length.
        let extendedLength (bytes: byte[]) : Result<int, TransportError> =
            if bytes.Length < 3 then
                Error (TransportError.InvalidFrame "Abridged extended length must be 3 bytes")
            else
                let length = (int bytes[0] ||| (int bytes[1] <<< 8) ||| (int bytes[2] <<< 16)) * 4
                if length <= 0 || length > MaxFrameLength then
                    Error (TransportError.InvalidFrame $"Invalid abridged frame length: %d{length}")
                else
                    Ok length

    /// Randomized ("secure"/padded) intermediate framing (obfuscation tag 0xdddddddd):
    /// intermediate framing plus 0-3 random padding bytes folded into the length. The
    /// decoder strips `length % 4` trailing bytes since MTProto payloads are 4-aligned.
    module RandomizedIntermediate =

        let private rng = System.Random.Shared

        /// Encode a payload with 0-3 random padding bytes appended.
        let encodeFrame (payload: byte[]) : byte[] =
            let padLen = rng.Next(0, 4)
            let total = payload.Length + padLen
            let frame = Array.zeroCreate<byte> (4 + total)
            Buffer.BlockCopy(BitConverter.GetBytes(int32 total), 0, frame, 0, 4)
            Buffer.BlockCopy(payload, 0, frame, 4, payload.Length)

            if padLen > 0 then
                let pad = Array.zeroCreate<byte> padLen
                rng.NextBytes pad
                Buffer.BlockCopy(pad, 0, frame, 4 + payload.Length, padLen)

            frame

        /// Drop the trailing padding (length % 4 bytes) from a decoded frame body.
        let stripPadding (body: byte[]) : byte[] =
            let padLen = body.Length % 4
            if padLen = 0 then body else body[.. body.Length - padLen - 1]
