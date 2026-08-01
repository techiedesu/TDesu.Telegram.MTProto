module TDesu.Telegram.TL.Tests.Crc32Tests

open NUnit.Framework
open TDesu.Telegram.TL
open TDesu.Telegram.TL.AST
open TDesu.Telegram.TL.Tests

[<Test>]
let ``compute CRC32 for known string`` () =
    // CRC32 of "boolFalse = Bool" should match known value
    let crc = Crc32.compute "boolFalse = Bool"
    equals crc 0xbc799737u

[<Test>]
let ``compute CRC32 for boolTrue`` () =
    let crc = Crc32.compute "boolTrue = Bool"
    equals crc 0x997275b5u

[<Test>]
let ``computeForCombinator produces correct CRC32`` () =
    let c = {
        Id = { Namespace = None; Name = "boolFalse" }
        ConstructorId = None
        TypeParams = []
        Params = []
        ResultType = TlTypeExpr.Boxed { Namespace = None; Name = "Bool" }
    }
    let crc = Crc32.computeForCombinator c
    equals crc 0xbc799737u

[<Test>]
let ``computeForCombinator for boolTrue`` () =
    let c = {
        Id = { Namespace = None; Name = "boolTrue" }
        ConstructorId = None
        TypeParams = []
        Params = []
        ResultType = TlTypeExpr.Boxed { Namespace = None; Name = "Bool" }
    }
    let crc = Crc32.computeForCombinator c
    equals crc 0x997275b5u

[<Test>]
let ``computeForCombinator with params`` () =
    // resPQ nonce:int128 server_nonce:int128 pq:string server_public_key_fingerprints:Vector<long> = ResPQ
    let c = {
        Id = { Namespace = None; Name = "resPQ" }
        ConstructorId = None
        TypeParams = []
        Params = [
            { Name = "nonce"; Type = TlTypeExpr.Bare { Namespace = None; Name = "int128" } }
            { Name = "server_nonce"; Type = TlTypeExpr.Bare { Namespace = None; Name = "int128" } }
            { Name = "pq"; Type = TlTypeExpr.Bare { Namespace = None; Name = "string" } }
            { Name = "server_public_key_fingerprints"; Type = TlTypeExpr.Vector(false, TlTypeExpr.Bare { Namespace = None; Name = "long" }) }
        ]
        ResultType = TlTypeExpr.Boxed { Namespace = None; Name = "ResPQ" }
    }
    let crc = Crc32.computeForCombinator c
    equals crc 0x05162463u

/// The bare/boxed distinction is part of the declaration text an id is
/// computed over, and Telegram's own published id is the arbiter: this
/// declaration hashes to `ae500895` only when the keyword is spelled
/// `vector` (lowercase, space-separated). Folding it to `Vector` — which is
/// what a case-insensitive parser produced — gives 0x94E2D547 instead.
///
/// It matters beyond bookkeeping: `resPQ` above pins the BOXED spelling
/// against a published id too, so the pair proves the two forms are distinct
/// and that each one is right, rather than that one of them happens to work.
[<Test>]
let ``computeForCombinator distinguishes a bare vector from a boxed one`` () =
    // future_salts#ae500895 req_msg_id:long now:int salts:vector<future_salt> = FutureSalts
    let c = {
        Id = { Namespace = None; Name = "future_salts" }
        ConstructorId = None
        TypeParams = []
        Params = [
            { Name = "req_msg_id"; Type = TlTypeExpr.Bare { Namespace = None; Name = "long" } }
            { Name = "now"; Type = TlTypeExpr.Bare { Namespace = None; Name = "int" } }
            { Name = "salts"
              Type = TlTypeExpr.Vector(true, TlTypeExpr.Bare { Namespace = None; Name = "future_salt" }) }
        ]
        ResultType = TlTypeExpr.Boxed { Namespace = None; Name = "FutureSalts" }
    }
    equals (Crc32.computeForCombinator c) 0xAE500895u

    let asBoxed =
        { c with
            Params =
                c.Params
                |> List.map (fun p ->
                    match p.Type with
                    | TlTypeExpr.Vector(_, inner) -> { p with Type = TlTypeExpr.Vector(false, inner) }
                    | _ -> p) }
    Assert.That(Crc32.computeForCombinator asBoxed, Is.Not.EqualTo 0xAE500895u)

[<Test>]
let ``compute CRC32 empty string`` () =
    let crc = Crc32.compute ""
    equals crc 0u

[<Test>]
let ``computeForCombinator with namespaced id`` () =
    // auth.sentCode phone_code_hash:string = auth.SentCode
    let c = {
        Id = { Namespace = Some "auth"; Name = "sentCode" }
        ConstructorId = None
        TypeParams = []
        Params = [
            { Name = "phone_code_hash"; Type = TlTypeExpr.Bare { Namespace = None; Name = "string" } }
        ]
        ResultType = TlTypeExpr.Boxed { Namespace = Some "auth"; Name = "SentCode" }
    }
    let crc = Crc32.computeForCombinator c
    // Verify the canonical string is correct
    let expected = Crc32.compute "auth.sentCode phone_code_hash:string = auth.SentCode"
    equals crc expected
