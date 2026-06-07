namespace TDesu.Telegram.Protocol.Tests

open System.IO
open NUnit.Framework
open NUnit.Framework.Legacy
open TDesu.MTProto

[<TestFixture>]
module PersistenceTests =

    let private sample =
        { DcId = 2
          AuthKey =
            { Data = Array.init 256 (fun i -> byte (i % 256))
              Id = 0x1122334455667788L
              AuxHash = 0x0102030405060708L }
          Salt = 0x7fffffffffffffffL
          TimeOffset = -3
          UserId = 1234567890L }

    [<Test>]
    let ``serialize then deserialize preserves every field`` () =
        let blob = PersistedSession.serialize sample

        match PersistedSession.tryDeserialize blob with
        | Some r ->
            Assert.That(r.DcId, Is.EqualTo sample.DcId)
            Assert.That(r.UserId, Is.EqualTo sample.UserId)
            Assert.That(r.Salt, Is.EqualTo sample.Salt)
            Assert.That(r.TimeOffset, Is.EqualTo sample.TimeOffset)
            Assert.That(r.AuthKey.Id, Is.EqualTo sample.AuthKey.Id)
            Assert.That(r.AuthKey.AuxHash, Is.EqualTo sample.AuthKey.AuxHash)
            CollectionAssert.AreEqual(sample.AuthKey.Data, r.AuthKey.Data)
        | None -> Assert.Fail("tryDeserialize returned None for a valid blob")

    [<Test>]
    let ``tryDeserialize rejects truncated / garbage data`` () =
        Assert.That(PersistedSession.tryDeserialize [||] |> Option.isNone, Is.True)
        Assert.That(PersistedSession.tryDeserialize [| 1uy; 2uy; 3uy |] |> Option.isNone, Is.True)

    [<Test>]
    let ``FileSessionStore round-trips a blob`` () =
        let path = Path.Combine(Path.GetTempPath(), $"persist-test-{Path.GetRandomFileName()}.bin")
        let store = FileSessionStore(path) :> ISessionStore

        try
            Assert.That(store.Load() |> Option.isNone, Is.True)
            let blob = PersistedSession.serialize sample
            store.Save blob

            match store.Load() with
            | Some loaded -> CollectionAssert.AreEqual(blob, loaded)
            | None -> Assert.Fail("Load returned None after Save")

            store.Clear()
            Assert.That(store.Load() |> Option.isNone, Is.True)
        finally
            if File.Exists path then File.Delete path

    [<Test>]
    let ``dispatcher rekey reroutes completion to the original task`` () =
        let d = RpcDispatcher()
        let body = [| 1uy; 2uy; 3uy |]
        let pendingTask = d.RegisterRequest(100L, body)

        match d.TryGetBody 100L with
        | Some b -> CollectionAssert.AreEqual(body, b)
        | None -> Assert.Fail("registered body should be retrievable")

        Assert.That(d.Rekey(100L, 200L), Is.True)
        Assert.That(d.TryGetBody 100L |> Option.isNone, Is.True)
        // a response under the *new* msg_id must still complete the caller's original task
        Assert.That(d.CompleteRequest(200L, [| 9uy |]), Is.True)
        CollectionAssert.AreEqual([| 9uy |], pendingTask.Result)

    [<Test>]
    let ``generateMsgId yields divisible-by-4, strictly increasing ids`` () =
        // Regression: msg_id must be ≡ 0 (mod 4) for client→server messages, else the
        // server rejects with bad_msg_notification 18. A prior version OR-ed in 1.
        let sess = Session.createSession ()
        let ids = [ for _ in 1..1000 -> Session.generateMsgId sess ]
        Assert.That(ids |> List.forall (fun id -> id % 4L = 0L), Is.True)
        let increasing = List.pairwise ids |> List.forall (fun (a, b) -> b > a)
        Assert.That(increasing, Is.True)

    [<Test>]
    let ``directory store keeps one blob per key and lists them`` () =
        let dir = Path.Combine(Path.GetTempPath(), $"dss-{Path.GetRandomFileName()}")
        let store = DirectorySessionStore(dir)

        try
            Assert.That(store.Keys(), Is.Empty)
            (store.For "111").Save(PersistedSession.serialize { sample with UserId = 111L })
            (store.For "222").Save(PersistedSession.serialize { sample with UserId = 222L })
            CollectionAssert.AreEqual([ "111"; "222" ], store.Keys() |> List.sort)

            match (store.For "111").Load() |> Option.bind PersistedSession.tryDeserialize with
            | Some s -> Assert.That(s.UserId, Is.EqualTo 111L)
            | None -> Assert.Fail("saved session should load back")

            store.Remove "111"
            CollectionAssert.AreEqual([ "222" ], store.Keys())
        finally
            if Directory.Exists dir then
                Directory.Delete(dir, true)
