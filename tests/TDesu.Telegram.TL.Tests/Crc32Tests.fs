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
            { Name = "server_public_key_fingerprints"; Type = TlTypeExpr.Vector(TlTypeExpr.Bare { Namespace = None; Name = "long" }) }
        ]
        ResultType = TlTypeExpr.Boxed { Namespace = None; Name = "ResPQ" }
    }
    let crc = Crc32.computeForCombinator c
    equals crc 0x05162463u

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
