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

    /// The MTProto auth key is always 2048-bit. A key of any other size deserializes fine but
    /// throws inside key derivation on every send and receive, so it is rejected up front.
    [<Literal>]
    let private AuthKeyLength = 256

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

    /// Parse a blob produced by `serialize`. None if empty, corrupt, truncated, or a different version.
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

                if len <> AuthKeyLength then
                    None
                else
                    // ReadBytes returns a SHORT array instead of throwing when the file was
                    // truncated, so the declared length must be checked against what arrived —
                    // otherwise a half-written key is handed back as a usable session and the
                    // client reconnects forever.
                    let keyData = r.ReadBytes(len)

                    if keyData.Length <> len then
                        None
                    else
                        Some
                            { DcId = dcId
                              UserId = userId
                              Salt = salt
                              TimeOffset = timeOffset
                              AuthKey =
                                { Data = keyData
                                  Id = keyId
                                  AuxHash = auxHash } }
        // Any malformed blob is "no session": a parse failure here must never crash startup.
        with _ ->
            None

/// Abstraction over where the session blob lives. Inject a custom one to encrypt it,
/// keep it in a DB, etc. The default is a plain file (`FileSessionStore`).
type ISessionStore =
    abstract member Load: unit -> byte[] option
    abstract member Save: byte[] -> unit
    abstract member Clear: unit -> unit

/// Stores the session blob in a single file.
///
/// SECURITY: the blob contains the raw auth key (full account access) and is written in
/// PLAINTEXT. On Unix the file is created with 0600 (owner-only). For at-rest encryption,
/// inject a custom `ISessionStore` that wraps this and encrypts the bytes.
/// `Save` throws `IOException` rather than persisting the key on a Unix filesystem that refuses
/// the owner-only mode; a silent fallback there would publish the account to every local user.
type FileSessionStore(path: string) =
    interface ISessionStore with
        member _.Load() =
            if File.Exists path then Some(File.ReadAllBytes path) else None

        member _.Save(data) =
            let dir = Path.GetDirectoryName(path)

            if not (String.IsNullOrEmpty dir) && not (Directory.Exists dir) then
                let di = Directory.CreateDirectory dir

                if not (OperatingSystem.IsWindows()) then
                    try di.UnixFileMode <- UnixFileMode.UserRead ||| UnixFileMode.UserWrite ||| UnixFileMode.UserExecute
                    with _ -> ()

            // Write to a sibling temp file and rename it over the target. Truncating the target
            // in place loses the auth key outright if the process dies mid-write, which costs an
            // interactive re-login; the rename either happens or it does not.
            let tmp = path + ".tmp"

            try
                if not (OperatingSystem.IsWindows()) then
                    // Restrict the file BEFORE the secret lands in it, so it is never briefly
                    // world-readable.
                    use fs = new FileStream(tmp, FileMode.Create, FileAccess.Write, FileShare.None)

                    try
                        File.SetUnixFileMode(fs.SafeFileHandle, UnixFileMode.UserRead ||| UnixFileMode.UserWrite)
                    with e ->
                        // A filesystem that cannot restrict the file would leave the raw auth key —
                        // full account access — readable by every local user. Fail loudly instead.
                        raise (
                            IOException(
                                $"Cannot restrict session file '{tmp}' to owner-only access; refusing to write the auth key in plaintext.",
                                e
                            )
                        )

                    fs.Write(data, 0, data.Length)
                    // Reach the disk before the rename, so the target never points at empty blocks.
                    fs.Flush(true)
                else
                    // On Windows the file inherits the (typically user-private) directory ACL.
                    File.WriteAllBytes(tmp, data)

                File.Move(tmp, path, true)
            with _ ->
                try File.Delete tmp with _ -> ()
                reraise ()

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
