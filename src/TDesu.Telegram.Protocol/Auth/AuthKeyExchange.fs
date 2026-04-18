namespace TDesu.MTProto.Auth

open System
open System.Numerics
open TDesu.FSharp
open TDesu.FSharp.Operators
open TDesu.FSharp.Buffers
open TDesu.MTProto
open TDesu.Serialization
open TDesu.Crypto
open TDesu.Transport

/// MTProto DH key exchange (Creating an Authorization Key).
/// Flow: req_pq_multi → resPQ → req_DH_params → server_DH_params_ok → set_client_DH_params → dh_gen_ok
module AuthKeyExchange =

    // Constructor IDs for auth exchange messages
    [<Literal>]
    let private ReqPqMulti = 0xbe7e8ef1u
    [<Literal>]
    let private ResPQ = 0x05162463u
    [<Literal>]
    let private PQInnerData = 0x83c95aecu
    [<Literal>]
    let private ReqDHParams = 0xd712e4beu
    [<Literal>]
    let private ServerDHParamsOk = 0xd0e8075cu
    [<Literal>]
    let private ServerDHInnerData = 0xb5890dbau
    [<Literal>]
    let private ClientDHInnerData = 0x6643b654u
    [<Literal>]
    let private SetClientDHParams = 0xf5045f1fu
    [<Literal>]
    let private DhGenOk = 0x3bcbf734u
    [<Literal>]
    let private DhGenRetry = 0x46dc1fb9u
    [<Literal>]
    let private DhGenFail = 0xa69dae02u

    /// Factorize pq into p and q (p < q) using Pollard's rho algorithm
    let factorizePQ (pq: uint64) : uint64 * uint64 =
        if pq % 2UL = 0UL then
            (2UL, pq / 2UL)
        else
            let mutable x = 2UL
            let mutable y = 2UL
            let mutable d = 1UL
            let mutable c = 1UL

            let f xv =
                let bx = BigInteger(uint64 xv)
                (BigInteger.Multiply(bx, bx) + BigInteger(uint64 c)) % BigInteger(uint64 pq) |> uint64

            while d = 1UL do
                x <- f x
                y <- f (f y)
                d <- BigInteger.GreatestCommonDivisor(BigInteger(int64 (if x > y then x - y else y - x)), BigInteger(uint64 pq)) |> uint64

            if d = pq then
                c <- c + 1UL
                x <- 2UL
                y <- 2UL
                d <- 1UL
                while d = 1UL do
                    x <- f x
                    y <- f (f y)
                    d <- BigInteger.GreatestCommonDivisor(BigInteger(int64 (if x > y then x - y else y - x)), BigInteger(uint64 pq)) |> uint64

            let other = pq / d
            if d < other then (d, other) else (other, d)

    /// Serialize req_pq_multi message
    let serializeReqPqMulti (nonce: byte[]) : byte[] =
        use w = new TlWriteBuffer()
        w.WriteConstructorId(ReqPqMulti)
        w.WriteRawBytes(nonce)
        w.ToArray()

    let serializePQInnerData (pq: byte[]) (p: byte[]) (q: byte[]) (nonce: byte[]) (serverNonce: byte[]) (newNonce: byte[]) : byte[] =
        use w = new TlWriteBuffer()
        w.WriteConstructorId(PQInnerData)
        w.WriteBytes(pq)
        w.WriteBytes(p)
        w.WriteBytes(q)
        w.WriteRawBytes(nonce)
        w.WriteRawBytes(serverNonce)
        w.WriteRawBytes(newNonce)
        w.ToArray()

    let serializeReqDHParams (nonce: byte[]) (serverNonce: byte[]) (p: byte[]) (q: byte[]) (fingerprint: int64) (encryptedData: byte[]) : byte[] =
        use w = new TlWriteBuffer()
        w.WriteConstructorId(ReqDHParams)
        w.WriteRawBytes(nonce)
        w.WriteRawBytes(serverNonce)
        w.WriteBytes(p)
        w.WriteBytes(q)
        w.WriteInt64(fingerprint)
        w.WriteBytes(encryptedData)
        w.ToArray()

    let serializeClientDHInnerData (nonce: byte[]) (serverNonce: byte[]) (retryId: int64) (gB: byte[]) : byte[] =
        use w = new TlWriteBuffer()
        w.WriteConstructorId(ClientDHInnerData)
        w.WriteRawBytes(nonce)
        w.WriteRawBytes(serverNonce)
        w.WriteInt64(retryId)
        w.WriteBytes(gB)
        w.ToArray()

    let serializeSetClientDHParams (nonce: byte[]) (serverNonce: byte[]) (encryptedData: byte[]) : byte[] =
        use w = new TlWriteBuffer()
        w.WriteConstructorId(SetClientDHParams)
        w.WriteRawBytes(nonce)
        w.WriteRawBytes(serverNonce)
        w.WriteBytes(encryptedData)
        w.ToArray()

    /// Derive tmp_aes_key and tmp_aes_iv from server_nonce and new_nonce
    let deriveTmpAes (serverNonce: byte[]) (newNonce: byte[]) : byte[] * byte[] =
        let hash1 = AuthKeyId.sha1 (Bytes.concat2 newNonce serverNonce)
        let hash2 = AuthKeyId.sha1 (Bytes.concat2 serverNonce newNonce)
        let hash3 = AuthKeyId.sha1 (Bytes.concat2 newNonce newNonce)
        let tmpAesKey = Bytes.concat2 hash1 hash2[0..11]
        let tmpAesIv = Bytes.concat3 hash2[12..19] hash3 newNonce[0..3]
        (tmpAesKey, tmpAesIv)

    let private uint64ToBeBytes (v: uint64) =
        let bytes = BitConverter.GetBytes(v)
        if BitConverter.IsLittleEndian then Array.rev bytes else bytes
        |> Array.skipWhile (fun b -> b = 0uy)
        |> fun a -> if a.Length = 0 then [| 0uy |] else a

    /// Perform the full auth key exchange
    let performExchange (transport: TcpTransport) (ct: System.Threading.CancellationToken) : System.Threading.Tasks.Task<Result<AuthKey * int64 * int32, MtProtoError>> = task {
        // Step 1: req_pq_multi
        let nonce = Padding.randomBytes 16
        let reqPq = serializeReqPqMulti nonce
        let msgId = DateTimeOffset.UtcNow.ToUnixTimeSeconds() <<< 32 ||| 1L

        let unencrypted = UnencryptedMessage.serialize msgId reqPq
        match! transport.SendAsync(unencrypted, ct) with
        | Error e -> return Error (MtProtoError.TransportError e)
        | Ok () ->

        // Step 2: Receive resPQ
        match! transport.ReceiveAsync(ct) with
        | Error e -> return Error (MtProtoError.TransportError e)
        | Ok responseData ->

        match UnencryptedMessage.deserialize responseData with
        | Error e -> return Error e
        | Ok (_resMsgId, resBody) ->

        use resReader = new TlReadBuffer(resBody)
        let constructorId = resReader.ReadConstructorId()
        if constructorId <> ResPQ then
            return Error (MtProtoError.AuthKeyExchangeFailed $"Expected resPQ, got 0x%08x{constructorId}")
        else

        let resNonce = resReader.ReadRawBytes(16)
        if resNonce <> nonce then
            return Error (MtProtoError.AuthKeyExchangeFailed "Nonce mismatch in resPQ")
        else

        let serverNonce = resReader.ReadRawBytes(16)
        let pqBytes = resReader.ReadBytes()
        let fingerprints = resReader.ReadVector(fun r -> r.ReadInt64())

        let pqValue =
            pqBytes |> Array.fold (fun v b -> (v <<< 8) ||| uint64 b) 0UL

        let (p, q) = factorizePQ pqValue

        let rsaKey =
            Rsa.allKeys ()
            |> List.tryFind (fun k -> fingerprints |> Array.exists (fun f -> f = k.Fingerprint))
        match rsaKey with
        | None -> return Error (MtProtoError.AuthKeyExchangeFailed "No matching RSA key found")
        | Some key ->

        // Step 3: Build p_q_inner_data
        let newNonce = Padding.randomBytes 32
        let pBytes = uint64ToBeBytes p
        let qBytes = uint64ToBeBytes q

        let pqInner = serializePQInnerData pqBytes pBytes qBytes nonce serverNonce newNonce

        let dataWithHash = Bytes.concat2 (AuthKeyId.sha1 pqInner) pqInner
        let padded =
            if dataWithHash.Length < 255 then
                Bytes.concat2 dataWithHash (Padding.randomBytes (255 - dataWithHash.Length))
            else
                dataWithHash
        let encryptedData = Rsa.encrypt padded key

        // Step 4: req_DH_params
        let reqDH = serializeReqDHParams nonce serverNonce pBytes qBytes key.Fingerprint encryptedData
        let msgId2 = DateTimeOffset.UtcNow.ToUnixTimeSeconds() <<< 32 ||| 5L
        let unencrypted2 = UnencryptedMessage.serialize msgId2 reqDH
        match! transport.SendAsync(unencrypted2, ct) with
        | Error e -> return Error (MtProtoError.TransportError e)
        | Ok () ->

        // Step 5: Receive server_DH_params
        match! transport.ReceiveAsync(ct) with
        | Error e -> return Error (MtProtoError.TransportError e)
        | Ok responseData2 ->

        match UnencryptedMessage.deserialize responseData2 with
        | Error e -> return Error e
        | Ok (_resMsgId2, resBody2) ->

        use resReader2 = new TlReadBuffer(resBody2)
        let constructorId2 = resReader2.ReadConstructorId()
        if constructorId2 <> ServerDHParamsOk then
            return Error (MtProtoError.AuthKeyExchangeFailed $"Expected server_DH_params_ok, got 0x%08x{constructorId2}")
        else

        let _resNonce2 = resReader2.ReadRawBytes(16)
        let _resServerNonce2 = resReader2.ReadRawBytes(16)
        let encryptedAnswer = resReader2.ReadBytes()

        let (tmpAesKey, tmpAesIv) = deriveTmpAes serverNonce newNonce
        let answerDecrypted = AesIge.decrypt encryptedAnswer tmpAesKey tmpAesIv

        use innerReader = new TlReadBuffer(answerDecrypted[20..])
        let innerConstructor = innerReader.ReadConstructorId()
        if innerConstructor <> ServerDHInnerData then
            return Error (MtProtoError.AuthKeyExchangeFailed "Invalid server_DH_inner_data")
        else

        let _innerNonce = innerReader.ReadRawBytes(16)
        let _innerServerNonce = innerReader.ReadRawBytes(16)
        let g = innerReader.ReadInt32()
        let dhPrime = innerReader.ReadBytes()
        let gA = innerReader.ReadBytes()
        let serverTime = innerReader.ReadInt32()

        // Step 6: Generate b, compute g_b and auth_key
        let b = DiffieHellman.generateA ()
        let gB = DiffieHellman.computeGA g b dhPrime
        let authKeyData = DiffieHellman.computeAuthKey gA b dhPrime

        let timeOffset = serverTime - int32 (DateTimeOffset.UtcNow.ToUnixTimeSeconds())

        // Step 7: Build client_DH_inner_data
        let clientDHInner = serializeClientDHInnerData nonce serverNonce 0L gB
        let clientDHInnerWithHash = Bytes.concat2 (AuthKeyId.sha1 clientDHInner) clientDHInner
        // Handshake spec: 0..15 padding (NOT the 12..1024 message-padding rule
        // — TDLib-strict servers reject > 15 with "Too much pad").
        let clientDHPadded = Padding.addHandshakePadding clientDHInnerWithHash
        let clientDHEncrypted = AesIge.encrypt clientDHPadded tmpAesKey tmpAesIv

        // Step 8: set_client_DH_params
        let setDH = serializeSetClientDHParams nonce serverNonce clientDHEncrypted
        let msgId3 = DateTimeOffset.UtcNow.ToUnixTimeSeconds() <<< 32 ||| 9L
        let unencrypted3 = UnencryptedMessage.serialize msgId3 setDH
        match! transport.SendAsync(unencrypted3, ct) with
        | Error e -> return Error (MtProtoError.TransportError e)
        | Ok () ->

        // Step 9: Receive dh_gen_ok
        match! transport.ReceiveAsync(ct) with
        | Error e -> return Error (MtProtoError.TransportError e)
        | Ok responseData3 ->

        match UnencryptedMessage.deserialize responseData3 with
        | Error e -> return Error e
        | Ok (_resMsgId3, resBody3) ->

        use resReader3 = new TlReadBuffer(resBody3)
        let constructorId3 = resReader3.ReadConstructorId()

        if constructorId3 = DhGenOk then
            let authKeyId = AuthKeyId.compute authKeyData
            let auxHash = BitConverter.ToInt64(AuthKeyId.sha1(authKeyData), 0)

            let salt =
                [| 0..7 |] |> Array.fold (fun s i ->
                    let bv = newNonce[i] ^^^ serverNonce[i]
                    s ||| (int64 bv <<< (i * 8))) 0L

            let authKey = {
                Data = authKeyData
                Id = authKeyId
                AuxHash = auxHash
            }
            return Ok (authKey, salt, timeOffset)
        elif constructorId3 = DhGenRetry then
            return Error (MtProtoError.AuthKeyExchangeFailed "DH gen retry requested")
        elif constructorId3 = DhGenFail then
            return Error (MtProtoError.AuthKeyExchangeFailed "DH gen failed")
        else
            return Error (MtProtoError.AuthKeyExchangeFailed $"Unexpected response: 0x%08x{constructorId3}")
    }
