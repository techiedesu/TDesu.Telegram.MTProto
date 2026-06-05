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
