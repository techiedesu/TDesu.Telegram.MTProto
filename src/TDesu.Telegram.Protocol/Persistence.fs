namespace TDesu.MTProto

open System
open System.IO

/// A persisted MTProto session — enough to reconnect to the same DC and reuse the
/// established auth key without re-running the DH exchange or logging in again.
type PersistedSession =
    { DcId: int
      AuthKey: AuthKey
      Salt: int64
      TimeOffset: int32
      UserId: int64 }

[<RequireQualifiedAccess>]
module PersistedSession =

    [<Literal>]
    let private CurrentVersion = 1

    /// Serialize to a self-describing byte blob (little-endian fixed layout, version-prefixed).
    let serialize (s: PersistedSession) : byte[] =
        use ms = new MemoryStream()
        use w = new BinaryWriter(ms)
        w.Write(CurrentVersion)
        w.Write(s.DcId)
        w.Write(s.UserId)
        w.Write(s.Salt)
        w.Write(s.TimeOffset)
        w.Write(s.AuthKey.Id)
        w.Write(s.AuthKey.AuxHash)
        w.Write(s.AuthKey.Data.Length)
        w.Write(s.AuthKey.Data)
        w.Flush()
        ms.ToArray()

    /// Parse a blob produced by `serialize`. None if empty, corrupt, or a different version.
    let tryDeserialize (data: byte[]) : PersistedSession option =
        try
            use ms = new MemoryStream(data)
            use r = new BinaryReader(ms)
            let version = r.ReadInt32()

            if version <> CurrentVersion then
                None
            else
                let dcId = r.ReadInt32()
                let userId = r.ReadInt64()
                let salt = r.ReadInt64()
                let timeOffset = r.ReadInt32()
                let keyId = r.ReadInt64()
                let auxHash = r.ReadInt64()
                let len = r.ReadInt32()
                let keyData = r.ReadBytes(len)

                Some
                    { DcId = dcId
                      UserId = userId
                      Salt = salt
                      TimeOffset = timeOffset
                      AuthKey =
                        { Data = keyData
                          Id = keyId
                          AuxHash = auxHash } }
        with :? EndOfStreamException ->
            None

/// Abstraction over where the session blob lives. Inject a custom one to encrypt it,
/// keep it in a DB, etc. The default is a plain file (`FileSessionStore`).
type ISessionStore =
    abstract member Load: unit -> byte[] option
    abstract member Save: byte[] -> unit
    abstract member Clear: unit -> unit

/// Stores the session blob in a single file.
type FileSessionStore(path: string) =
    interface ISessionStore with
        member _.Load() =
            if File.Exists path then Some(File.ReadAllBytes path) else None

        member _.Save(data) =
            let dir = Path.GetDirectoryName(path)

            if not (String.IsNullOrEmpty dir) && not (Directory.Exists dir) then
                Directory.CreateDirectory dir |> ignore

            File.WriteAllBytes(path, data)

        member _.Clear() =
            if File.Exists path then
                File.Delete path

/// Keeps one session blob per key (account) under `<dir>/<key>.session`. Use for multi-account
/// setups: `store.For key` hands back a single-session store, and `store.Keys()` enumerates the
/// accounts already saved (so an app can restore every logged-in account on startup).
type DirectorySessionStore(dir: string) =
    let ext = ".session"
    let pathFor (key: string) = Path.Combine(dir, key + ext)

    /// A single-session store bound to the given account key.
    member _.For(key: string) : ISessionStore = FileSessionStore(pathFor key) :> ISessionStore

    /// Keys of all currently saved sessions.
    member _.Keys() : string list =
        if Directory.Exists dir then
            Directory.GetFiles(dir, "*" + ext)
            |> Seq.map Path.GetFileNameWithoutExtension
            |> List.ofSeq
        else
            []

    member _.Remove(key: string) =
        let p = pathFor key

        if File.Exists p then
            File.Delete p
