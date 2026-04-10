namespace TDesu.Serialization

open System
open System.Buffers.Binary

/// Minimal TL write buffer for MTProto binary serialization.
type TlWriteBuffer() =
    let mutable buf = Array.zeroCreate<byte> 256
    let mutable pos = 0

    let ensureCapacity n =
        if pos + n > buf.Length then
            let newBuf = Array.zeroCreate (max (buf.Length * 2) (pos + n))
            Buffer.BlockCopy(buf, 0, newBuf, 0, pos)
            buf <- newBuf

    member _.WriteConstructorId(cid: uint32) =
        ensureCapacity 4
        BinaryPrimitives.WriteUInt32LittleEndian(buf.AsSpan(pos), cid)
        pos <- pos + 4

    member _.WriteInt32(v: int32) =
        ensureCapacity 4
        BinaryPrimitives.WriteInt32LittleEndian(buf.AsSpan(pos), v)
        pos <- pos + 4

    member _.WriteInt64(v: int64) =
        ensureCapacity 8
        BinaryPrimitives.WriteInt64LittleEndian(buf.AsSpan(pos), v)
        pos <- pos + 8

    member _.WriteDouble(v: double) =
        ensureCapacity 8
        BinaryPrimitives.WriteInt64LittleEndian(buf.AsSpan(pos), BitConverter.DoubleToInt64Bits v)
        pos <- pos + 8

    member _.WriteBool(v: bool) =
        ensureCapacity 4
        let cid = if v then 0x997275B5u else 0xBC799737u
        BinaryPrimitives.WriteUInt32LittleEndian(buf.AsSpan(pos), cid)
        pos <- pos + 4

    member _.WriteString(v: string) =
        let bytes = Text.Encoding.UTF8.GetBytes(v)
        let len = bytes.Length
        if len < 254 then
            ensureCapacity (1 + len + (4 - (1 + len) % 4) % 4)
            buf[pos] <- byte len
            pos <- pos + 1
        else
            ensureCapacity (4 + len + (4 - len % 4) % 4)
            buf[pos] <- 254uy
            buf[pos + 1] <- byte (len &&& 0xFF)
            buf[pos + 2] <- byte ((len >>> 8) &&& 0xFF)
            buf[pos + 3] <- byte ((len >>> 16) &&& 0xFF)
            pos <- pos + 4
        Buffer.BlockCopy(bytes, 0, buf, pos, len)
        pos <- pos + len
        let padding = (4 - pos % 4) % 4
        for _ in 0 .. padding - 1 do
            buf[pos] <- 0uy
            pos <- pos + 1

    member _.WriteBytes(v: byte array) =
        let len = v.Length
        if len < 254 then
            ensureCapacity (1 + len + (4 - (1 + len) % 4) % 4)
            buf[pos] <- byte len
            pos <- pos + 1
        else
            ensureCapacity (4 + len + (4 - len % 4) % 4)
            buf[pos] <- 254uy
            buf[pos + 1] <- byte (len &&& 0xFF)
            buf[pos + 2] <- byte ((len >>> 8) &&& 0xFF)
            buf[pos + 3] <- byte ((len >>> 16) &&& 0xFF)
            pos <- pos + 4
        Buffer.BlockCopy(v, 0, buf, pos, len)
        pos <- pos + len
        let padding = (4 - pos % 4) % 4
        for _ in 0 .. padding - 1 do
            buf[pos] <- 0uy
            pos <- pos + 1

    member _.WriteRawBytes(v: byte array) =
        ensureCapacity v.Length
        Buffer.BlockCopy(v, 0, buf, pos, v.Length)
        pos <- pos + v.Length

    member _.ToArray() = buf[.. pos - 1]

    interface IDisposable with
        member _.Dispose() = ()


/// Minimal TL read buffer for MTProto binary deserialization.
type TlReadBuffer(data: byte array) =
    let mutable pos = 0
    let span () = ReadOnlySpan(data, pos, data.Length - pos)

    member _.ReadConstructorId() : uint32 =
        let v = BinaryPrimitives.ReadUInt32LittleEndian(span())
        pos <- pos + 4
        v

    member _.ReadInt32() : int32 =
        let v = BinaryPrimitives.ReadInt32LittleEndian(span())
        pos <- pos + 4
        v

    member _.ReadInt64() : int64 =
        let v = BinaryPrimitives.ReadInt64LittleEndian(span())
        pos <- pos + 8
        v

    member _.ReadDouble() : double =
        let v = BinaryPrimitives.ReadInt64LittleEndian(span())
        pos <- pos + 8
        BitConverter.Int64BitsToDouble v

    member _.ReadRawBytes(count: int) : byte array =
        let result = data[pos .. pos + count - 1]
        pos <- pos + count
        result

    member _.ReadBool() : bool =
        let cid = BinaryPrimitives.ReadUInt32LittleEndian(span())
        pos <- pos + 4
        cid = 0x997275B5u

    member _.ReadString() : string =
        let firstByte = int data[pos]
        let len, headerSize =
            if firstByte < 254 then firstByte, 1
            else
                let l = int data[pos+1] ||| (int data[pos+2] <<< 8) ||| (int data[pos+3] <<< 16)
                l, 4
        pos <- pos + headerSize
        let s = Text.Encoding.UTF8.GetString(data, pos, len)
        pos <- pos + len
        let padding = (4 - pos % 4) % 4
        pos <- pos + padding
        s

    member _.ReadBytes() : byte array =
        let firstByte = int data[pos]
        let len, headerSize =
            if firstByte < 254 then firstByte, 1
            else
                let l = int data[pos+1] ||| (int data[pos+2] <<< 8) ||| (int data[pos+3] <<< 16)
                l, 4
        pos <- pos + headerSize
        let result = data[pos .. pos + len - 1]
        pos <- pos + len
        let padding = (4 - pos % 4) % 4
        pos <- pos + padding
        result
