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
    let private PQInnerDataDc = 0xa9f55f95u
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

    /// p_q_inner_data_dc — the current variant carrying the target dc_id, required by the
    /// RSA_PAD flow (the server returns -404/-444 otherwise).
    let serializePQInnerDataDc (pq: byte[]) (p: byte[]) (q: byte[]) (nonce: byte[]) (serverNonce: byte[]) (newNonce: byte[]) (dcId: int) : byte[] =
        use w = new TlWriteBuffer()
        w.WriteConstructorId(PQInnerDataDc)
        w.WriteBytes(pq)
        w.WriteBytes(p)
        w.WriteBytes(q)
        w.WriteRawBytes(nonce)
        w.WriteRawBytes(serverNonce)
        w.WriteRawBytes(newNonce)
        w.WriteInt32(dcId)
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
    let performExchange (transport: ITransport) (dcId: int) (ct: System.Threading.CancellationToken) : System.Threading.Tasks.Task<Result<AuthKey * int64 * int32, MtProtoError>> = task {
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

        // Pick the key in the server's advertised order — the first fingerprint is the one the
        // server prefers (the RSA_PAD key), so honour that instead of our local key order.
        let rsaKey =
            fingerprints
            |> Array.tryPick (fun f -> Rsa.allKeys () |> List.tryFind (fun k -> k.Fingerprint = f))
        match rsaKey with
        | None -> return Error (MtProtoError.AuthKeyExchangeFailed "No matching RSA key found")
        | Some key ->

        // Step 3: Build p_q_inner_data
        let newNonce = Padding.randomBytes 32
        let pBytes = uint64ToBeBytes p
        let qBytes = uint64ToBeBytes q

        let pqInner = serializePQInnerDataDc pqBytes pBytes qBytes nonce serverNonce newNonce dcId

        // Encrypt p_q_inner_data with RSA_PAD (MTProto's current, hardened scheme). The classic
        // sha1(data)+data scheme is still available as Rsa.encrypt for callers that need it.
        let encryptedData = Rsa.encryptPad pqInner key

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

        let resNonce2 = resReader2.ReadRawBytes(16)
        if resNonce2 <> nonce then
            return Error (MtProtoError.AuthKeyExchangeFailed "Nonce mismatch in server_DH_params")
        else

        let resServerNonce2 = resReader2.ReadRawBytes(16)
        if resServerNonce2 <> serverNonce then
            return Error (MtProtoError.AuthKeyExchangeFailed "server_nonce mismatch in server_DH_params")
        else

        let encryptedAnswer = resReader2.ReadBytes()

        let (tmpAesKey, tmpAesIv) = deriveTmpAes serverNonce newNonce
        let answerDecrypted = AesIge.decrypt encryptedAnswer tmpAesKey tmpAesIv

        use innerReader = new TlReadBuffer(answerDecrypted[20..])
        let innerConstructor = innerReader.ReadConstructorId()
        if innerConstructor <> ServerDHInnerData then
            return Error (MtProtoError.AuthKeyExchangeFailed "Invalid server_DH_inner_data")
        else

        let innerNonce = innerReader.ReadRawBytes(16)
        if innerNonce <> nonce then
            return Error (MtProtoError.AuthKeyExchangeFailed "Nonce mismatch in server_DH_inner_data")
        else

        let innerServerNonce = innerReader.ReadRawBytes(16)
        if innerServerNonce <> serverNonce then
            return Error (MtProtoError.AuthKeyExchangeFailed "server_nonce mismatch in server_DH_inner_data")
        else

        let g = innerReader.ReadInt32()
        let dhPrime = innerReader.ReadBytes()
        let gA = innerReader.ReadBytes()
        let serverTime = innerReader.ReadInt32()

        // Verify the SHA1 integrity prefix of the decrypted answer. Only a server holding
        // the RSA private key could produce a validly-hashed inner block, so this both
        // authenticates the server and guarantees the DH values were not tampered with.
        let innerLen = innerReader.Position
        let answerHashOk =
            answerDecrypted.Length >= 20 + innerLen
            && AuthKeyId.sha1 answerDecrypted[20 .. 20 + innerLen - 1] = answerDecrypted[0..19]
        if not answerHashOk then
            return Error (MtProtoError.AuthKeyExchangeFailed "server_DH_inner_data hash mismatch")
        else

        // Validate DH parameters: g must be one of the six generators Telegram allows and
        // dh_prime must be a safe prime of exactly 2048 bits. The explicit {2..7} bound is a
        // defense-in-depth check because the library validator does not enforce it on its own;
        // without this a malicious server could pick a weak prime and recover the auth key.
        if g < 2 || g > 7 then
            return Error (MtProtoError.AuthKeyExchangeFailed $"g out of allowed range {{2..7}}: %d{g}")
        elif not (DiffieHellman.validateDhParams g dhPrime) then
            return Error (MtProtoError.AuthKeyExchangeFailed "Invalid DH parameters (g / dh_prime)")
        else

        // Validate g_a lies in [2^{2048-64}, dh_prime - 2^{2048-64}] to exclude the
        // degenerate small-subgroup values (0, 1, dh_prime-1) that force a known auth key.
        if not (DiffieHellman.validateGARange gA dhPrime) then
            return Error (MtProtoError.AuthKeyExchangeFailed "g_a out of range")
        else

        // Step 6: Generate b, compute g_b and auth_key
        let b = DiffieHellman.generateA ()
        let gB = DiffieHellman.computeGA g b dhPrime

        // Our own g_b must satisfy the same range constraint before we transmit it.
        if not (DiffieHellman.validateGARange gB dhPrime) then
            return Error (MtProtoError.AuthKeyExchangeFailed "computed g_b out of range")
        else

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
            let genNonce = resReader3.ReadRawBytes(16)
            let genServerNonce = resReader3.ReadRawBytes(16)
            let newNonceHash1 = resReader3.ReadRawBytes(16)

            let authKeySha1 = AuthKeyId.sha1 authKeyData
            let authKeyId = AuthKeyId.compute authKeyData
            let auxHashBytes = authKeySha1[0..7]
            let auxHash = BitConverter.ToInt64(authKeySha1, 0)

            // new_nonce_hash1 = substr(SHA1(new_nonce + 0x01 + auth_key_aux_hash), 4, 16).
            // Confirms the server derived the SAME auth key; a mismatch means a corrupted or
            // tampered exchange, so the key MUST NOT be trusted.
            let expectedHash = (AuthKeyId.sha1 (Bytes.concat3 newNonce [| 1uy |] auxHashBytes))[4..19]

            if genNonce <> nonce then
                return Error (MtProtoError.AuthKeyExchangeFailed "Nonce mismatch in dh_gen_ok")
            elif genServerNonce <> serverNonce then
                return Error (MtProtoError.AuthKeyExchangeFailed "server_nonce mismatch in dh_gen_ok")
            elif newNonceHash1 <> expectedHash then
                return Error (MtProtoError.AuthKeyExchangeFailed "new_nonce_hash1 mismatch — server derived a different auth key")
            else

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
