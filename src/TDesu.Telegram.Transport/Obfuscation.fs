namespace TDesu.Transport

open System
open System.Security.Cryptography
open TDesu.FSharp
open TDesu.FSharp.Operators

/// AES-256-CTR stream cipher (big-endian 128-bit counter), as used by MTProto's
/// transport obfuscation. .NET has no built-in CTR mode, so it is composed from
/// AES-ECB over the incrementing counter block XORed with the data. State (counter
/// + partial keystream) is retained across calls so a connection's byte stream is
/// one continuous keystream.
type Aes256Ctr(key: byte[], iv: byte[]) =
    let aes =
        let a = Aes.Create()
        a.Key <- key
        a.Mode <- CipherMode.ECB
        a.Padding <- PaddingMode.None
        a

    let ecb = aes.CreateEncryptor()
    let counter = Array.copy iv
    let keystream = Array.zeroCreate<byte> 16
    let mutable ksPos = 16 // force a refill on first byte

    let incrementCounter () =
        let mutable i = 15
        let mutable carry = true

        while carry && i >= 0 do
            counter[i] <- counter[i] + 1uy
            carry <- counter[i] = 0uy
            i <- i - 1

    /// XOR data with the continuing keystream, advancing cipher state.
    member _.Process(data: byte[]) : byte[] =
        let out = Array.zeroCreate<byte> data.Length

        for j in 0 .. data.Length - 1 do
            if ksPos = 16 then
                %ecb.TransformBlock(counter, 0, 16, keystream, 0)
                incrementCounter ()
                ksPos <- 0

            out[j] <- data[j] ^^^ keystream[ksPos]
            ksPos <- ksPos + 1

        out

    interface IDisposable with
        member _.Dispose() =
            ecb.Dispose()
            aes.Dispose()

/// MTProto transport obfuscation ("obfuscation2"): a 64-byte init packet whose
/// derived key material seeds a per-direction AES-256-CTR stream. After the init
/// is sent, every frame in each direction is CTR-encrypted with the continuing
/// keystream. The 4-byte protocol tag inside the init selects the inner framing.
module Obfuscation =

    /// Protocol tag written at offset 56 of the init packet.
    [<Literal>]
    let AbridgedTag = 0xefefefefu

    [<Literal>]
    let IntermediateTag = 0xeeeeeeeeu

    /// Padded ("secure"/randomized intermediate) tag — required by MTProxy fake-TLS.
    [<Literal>]
    let SecureTag = 0xddddddddu

    /// First-int values the init must avoid so the server can't confuse it with a
    /// known plaintext transport header (HTTP verbs, TLS, and the framing tags).
    let private forbiddenFirstInt =
        set [ 0x44414548u; 0x54534f50u; 0x20544547u; 0x4954504fu; 0xeeeeeeeeu; 0xddddddddu; 0x02010316u ]

    /// Ciphers plus the ready-to-send init packet. The send cipher has already
    /// consumed the 64-byte init (its state continues from block 4); the receive
    /// cipher starts fresh for incoming server bytes.
    [<NoComparison; NoEquality>]
    type State = {
        Send: Aes256Ctr
        Recv: Aes256Ctr
        InitPacket: byte[]
    }

    let private sha256 (data: byte[]) : byte[] =
        use h = SHA256.Create()
        h.ComputeHash data

    /// Build a 64-byte obfuscation init tagged for the given inner protocol, derive
    /// the directional CTR ciphers, and return the packet to send (last 8 bytes
    /// replaced by their encrypted form). `keyOf` transforms the 32-byte key material
    /// before it seeds AES (identity for direct connections; sha256(k+secret) for MTProxy).
    let private createWith (tag: uint32) (dcId: int) (keyOf: byte[] -> byte[]) : State =
        use rng = RandomNumberGenerator.Create()
        let init = Array.zeroCreate<byte> 64
        let mutable ok = false

        while not ok do
            rng.GetBytes init

            if
                init[0] <> 0xefuy
                && not (forbiddenFirstInt.Contains(BitConverter.ToUInt32(init, 0)))
                && BitConverter.ToUInt32(init, 4) <> 0u
            then
                ok <- true

        // Protocol tag (bytes 56..60) and dc_id as int16 LE (bytes 60..62).
        Buffer.BlockCopy(BitConverter.GetBytes tag, 0, init, 56, 4)
        Buffer.BlockCopy(BitConverter.GetBytes(int16 dcId), 0, init, 60, 2)

        let send = new Aes256Ctr(keyOf init[8..39], init[40..55])
        let reversed = Array.rev init
        let recv = new Aes256Ctr(keyOf reversed[8..39], reversed[40..55])

        let encryptedInit = send.Process init
        let packet = Array.copy init
        Array.blit encryptedInit 56 packet 56 8

        { Send = send; Recv = recv; InitPacket = packet }

    /// Direct-connection obfuscation (Telegram DC endpoints): raw key material.
    let create (tag: uint32) (dcId: int) : State = createWith tag dcId id

    /// MTProxy obfuscation: CTR keys are sha256(keyMaterial ++ secret), IV stays raw.
    let createMtproxy (tag: uint32) (dcId: int) (secret: byte[]) : State =
        createWith tag dcId (fun k -> sha256 (Array.append k secret))
