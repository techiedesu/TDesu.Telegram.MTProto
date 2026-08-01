// What the C# backend actually puts on the wire, for two builds of the emitter,
// from the same declarations.
//
//   dotnet fsi tools/wire-probe.fsx                    # working tree vs HEAD
//   dotnet fsi tools/wire-probe.fsx --baseline c55e303 # vs any git ref
//   dotnet fsi tools/wire-probe.fsx --baseline none    # working tree only
//
// Only the working tree is asserted; the baseline is printed for contrast and
// is allowed to be wrong. `--baseline c55e303` is the commit before #116/#117
// were fixed, and reproduces what those bugs looked like:
//
//   future_salts     baseline BAD 44  working ok 36   -8   (bare vector + bare element)
//   accessPointRule  baseline BAD 32  working ok 28   -4   (bare vector, boxed element)
//   msgs_ack         baseline  ok 28  working ok 28    0   CONTROL, boxed, must not move
//   req_pq_multi     baseline BAD 24  working ok 20   -4   (int128 as length-prefixed bytes)
//
// ── why a byte probe, when the suite is green ─────────────────────────────────
//
// Measured, not argued (Altergram #118). A round-trip harness over all 2506
// generated types — populate, Serialize, Deserialize, Serialize, compare —
// passes 2506/2506 with `future_salts` 8 bytes too long. An encoding that is
// wrong the same way on read and write agrees with itself, so no oracle derived
// from the generated code can see it. `EmitCSharpTests` asserts the emitted
// TOKENS, which catches the shape but never states a length; Roslyn parses the
// text and a parser sees syntax, never the wire.
//
// So this script does the one thing neither does: it compiles the emitted code
// and counts the bytes, against lengths derived here from the declaration and
// core.telegram.org/mtproto/serialize and from nothing the generator produced.
//
// ── the expected lengths, derived by hand ─────────────────────────────────────
//
//   future_salts#ae500895 req_msg_id:long now:int salts:vector<future_salt>
//   future_salt#0949d9dc  valid_since:int valid_until:int salt:long
//
//     cid           4     ae500895
//     req_msg_id    8
//     now           4
//     salts         4     element count. `vector` is BARE — no 0x1CB5C415.
//       [0]        16     int + int + long, BARE — no 0x0949d9dc.
//                 ---
//                  36     and WTelegramClient sends 36 for the same call.
//
//   msgs_ack#62d6b459 msg_ids:Vector<long>   — the control, and the reason a
//   "no 0x1CB5C415 anywhere" check would be worthless on its own:
//
//     cid           4     62d6b459
//     msg_ids       4     0x1CB5C415. `Vector` is BOXED and keeps its header.
//                   4     element count
//                  16     2 x long
//                 ---
//                  28     unchanged by the #117 fix, in both builds.
//
//   req_pq_multi#be7e8ef1 nonce:int128      — #116, on the same run:
//
//     cid           4     be7e8ef1
//     nonce        16     RAW. Not the `bytes` primitive: no length byte, no
//                 ---     padding to 4, which cost +4 and made it 24.
//                  20
//
// A mismatch here is a wire defect regardless of what any other gate says.

open System
open System.Diagnostics
open System.IO
open System.Reflection
open System.Text

let repo = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, ".."))
let generatorProject = Path.Combine(repo, "src/TDesu.Telegram.TL.Generator/TDesu.Telegram.TL.Generator.fsproj")
// The declarations are the ones tdlib publishes, vendored under TestData —
// a synthetic snippet could be quietly reshaped to suit the answer. Both files,
// paired the way a consumer pairs them: `mtproto.tl` alone collides
// `rpc_drop_answer` with itself and only the merge resolves that.
let apiSchema = Path.Combine(repo, "tests/TDesu.Telegram.TL.Tests/TestData/tdlib_telegram_api.tl")
let mtprotoSchema = Path.Combine(repo, "tests/TDesu.Telegram.TL.Tests/TestData/tdlib_mtproto.tl")

#r "../src/TDesu.Telegram.TL.Generator/bin/Debug/net10.0/Microsoft.CodeAnalysis.dll"
#r "../src/TDesu.Telegram.TL.Generator/bin/Debug/net10.0/Microsoft.CodeAnalysis.CSharp.dll"

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp

let args = fsi.CommandLineArgs |> Array.skip 1

let argValue name =
    args |> Array.tryFindIndex ((=) name) |> Option.bind (fun i -> args |> Array.tryItem (i + 1))

let baseline = argValue "--baseline" |> Option.defaultValue "HEAD"

// ── the runtime the emitted code is compiled against ─────────────────────────
//
// The contract EmitCSharp's module docstring states, and only the members these
// three declarations reach. Deliberately the dullest possible implementation:
// this file is the oracle, so it has to be readable in one sitting. Nothing
// here is generated, and nothing here is shared with the emitter — if both were
// wrong the same way the probe would prove nothing (CLAUDE.md's sixth way).

let runtimeSource =
    """
using System;
using System.Buffers.Binary;
using System.Collections.Generic;
using System.Text;

namespace Altergram.Schema
{
    public interface ITlObject { uint ConstructorId { get; } void Serialize(TlWriteBuffer w); }

    public sealed class TlWriteBuffer
    {
        private byte[] _b = new byte[256];
        private int _n;
        private void Ensure(int k) { if (_n + k > _b.Length) Array.Resize(ref _b, Math.Max(_b.Length * 2, _n + k)); }

        public void WriteInt32(int v) { Ensure(4); BinaryPrimitives.WriteInt32LittleEndian(_b.AsSpan(_n), v); _n += 4; }
        public void WriteUInt32(uint v) { Ensure(4); BinaryPrimitives.WriteUInt32LittleEndian(_b.AsSpan(_n), v); _n += 4; }
        public void WriteConstructorId(uint v) => WriteUInt32(v);
        public void WriteInt64(long v) { Ensure(8); BinaryPrimitives.WriteInt64LittleEndian(_b.AsSpan(_n), v); _n += 8; }
        public void WriteDouble(double v) { Ensure(8); BinaryPrimitives.WriteDoubleLittleEndian(_b.AsSpan(_n), v); _n += 8; }
        public void WriteBool(bool v) => WriteUInt32(v ? 0x997275B5u : 0xBC799737u);
        public void WriteRawBytes(ReadOnlySpan<byte> d) { Ensure(d.Length); d.CopyTo(_b.AsSpan(_n)); _n += d.Length; }

        // TL `bytes`: 1 length byte for <= 253 else 0xFE + 3, then the body,
        // then zero padding to a 4-byte boundary. This is the encoding int128
        // was wrongly getting.
        public void WriteBytes(ReadOnlySpan<byte> d)
        {
            int written;
            if (d.Length <= 253) { Ensure(1); _b[_n++] = (byte)d.Length; written = 1 + d.Length; }
            else { Ensure(4); _b[_n++] = 254; _b[_n++] = (byte)d.Length; _b[_n++] = (byte)(d.Length >> 8); _b[_n++] = (byte)(d.Length >> 16); written = 4 + d.Length; }
            WriteRawBytes(d);
            for (var i = 0; i < (4 - written % 4) % 4; i++) { Ensure(1); _b[_n++] = 0; }
        }

        public void WriteString(string s) => WriteBytes(Encoding.UTF8.GetBytes(s));

        public void WriteVector<T>(IReadOnlyList<T> items, Action<TlWriteBuffer, T> writeItem)
        {
            WriteUInt32(0x1CB5C415u);
            WriteInt32(items.Count);
            foreach (var it in items) writeItem(this, it);
        }

        public byte[] ToArray() { var r = new byte[_n]; Array.Copy(_b, r, _n); return r; }
    }

    public sealed class TlReadBuffer
    {
        private readonly byte[] _d;
        private int _p;
        public TlReadBuffer(byte[] d) { _d = d; }
        public int Remaining => _d.Length - _p;

        public int ReadInt32() { var v = BinaryPrimitives.ReadInt32LittleEndian(_d.AsSpan(_p)); _p += 4; return v; }
        public uint ReadUInt32() { var v = BinaryPrimitives.ReadUInt32LittleEndian(_d.AsSpan(_p)); _p += 4; return v; }
        public uint ReadConstructorId() => ReadUInt32();
        public long ReadInt64() { var v = BinaryPrimitives.ReadInt64LittleEndian(_d.AsSpan(_p)); _p += 8; return v; }
        public double ReadDouble() { var v = BinaryPrimitives.ReadDoubleLittleEndian(_d.AsSpan(_p)); _p += 8; return v; }
        public bool ReadBool() => ReadUInt32() == 0x997275B5u;
        public byte[] ReadRawBytes(int n) { var r = new byte[n]; Array.Copy(_d, _p, r, 0, n); _p += n; return r; }

        public byte[] ReadBytes()
        {
            int len, consumed;
            var first = _d[_p++];
            if (first <= 253) { len = first; consumed = 1 + len; }
            else { len = _d[_p] | (_d[_p + 1] << 8) | (_d[_p + 2] << 16); _p += 3; consumed = 4 + len; }
            var r = new byte[len];
            Array.Copy(_d, _p, r, 0, len);
            _p += len + (4 - consumed % 4) % 4;
            return r;
        }

        public string ReadString() => Encoding.UTF8.GetString(ReadBytes());

        public T[] ReadVector<T>(Func<TlReadBuffer, T> readItem)
        {
            var header = ReadUInt32();
            if (header != 0x1CB5C415u) throw new System.IO.InvalidDataException($"expected vector#1cb5c415, got 0x{header:x8}");
            var n = ReadInt32();
            var r = new T[n];
            for (var i = 0; i < n; i++) r[i] = readItem(this);
            return r;
        }
    }
}
"""

// ── driving a generator build ────────────────────────────────────────────────

let run (exe: string) (argv: string list) (cwd: string) =
    let psi = ProcessStartInfo(exe, RedirectStandardOutput = true, RedirectStandardError = true, WorkingDirectory = cwd)
    for a in argv do psi.ArgumentList.Add a
    use p = Process.Start psi
    let out = p.StandardOutput.ReadToEnd()
    let err = p.StandardError.ReadToEnd()
    p.WaitForExit()
    p.ExitCode, out + err

/// Emit the C# for the two schemas using the generator project at `project`,
/// and return the single-file source. Shelling out rather than referencing the
/// generator assembly is what makes two builds comparable in one run: the two
/// declare the same namespaces and could never be loaded side by side.
let emitWith (label: string) (project: string) =
    let outDir = Path.Combine(Path.GetTempPath(), "tdesu-wire-probe", label)
    Directory.CreateDirectory outDir |> ignore
    let code, log =
        run
            "dotnet"
            [ "run"; "--project"; project; "-c"; "Debug"; "--"
              "--schema"; apiSchema
              "--mtproto-schema"; mtprotoSchema
              "--output"; outDir
              "--namespace"; "Altergram.Schema"
              "--overrides"; Path.Combine(repo, "tools/wire-probe-overrides.toml")
              "--target"; "csharp" ]
            repo
    if code <> 0 then failwithf "%s: generator exited %d\n%s" label code log
    File.ReadAllText(Path.Combine(outDir, "GeneratedTl.g.cs"))

// ── compiling and probing ────────────────────────────────────────────────────

let refs =
    let trusted = AppContext.GetData "TRUSTED_PLATFORM_ASSEMBLIES" :?> string
    trusted.Split(Path.PathSeparator)
    |> Array.filter (fun p -> p.EndsWith ".dll")
    |> Array.map (fun p -> MetadataReference.CreateFromFile p :> MetadataReference)

let compile (label: string) (generated: string) : Assembly =
    let parse (s: string) = CSharpSyntaxTree.ParseText(s, CSharpParseOptions(LanguageVersion.Latest))
    let options =
        CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary, optimizationLevel = OptimizationLevel.Release)
    let comp =
        CSharpCompilation.Create($"WireProbe_{label}", [ parse runtimeSource; parse generated ], refs, options)
    use ms = new MemoryStream()
    let result = comp.Emit ms
    if not result.Success then
        let errs =
            result.Diagnostics
            |> Seq.filter (fun d -> d.Severity = DiagnosticSeverity.Error)
            |> Seq.truncate 10
            |> Seq.map string
            |> String.concat "\n  "
        failwithf "%s: emitted C# does not compile\n  %s" label errs
    Assembly.Load(ms.ToArray())

let hex (b: byte[]) =
    b |> Array.map (sprintf "%02X") |> Array.chunkBySize 4 |> Array.map (String.concat "") |> String.concat " "

let serialize (asm: Assembly) (typeName: string) (fill: obj -> unit) =
    let t = asm.GetType("Altergram.Schema." + typeName, true)
    let o = Activator.CreateInstance t
    fill o
    let wt = asm.GetType("Altergram.Schema.TlWriteBuffer", true)
    let w = Activator.CreateInstance wt
    t.GetMethod("Serialize").Invoke(o, [| w |]) |> ignore
    wt.GetMethod("ToArray").Invoke(w, [||]) :?> byte[]

let set (o: obj) (field: string) (v: obj) = o.GetType().GetField(field).SetValue(o, v)

/// One measurement: a named case, the length the declaration requires, and the
/// length the build produced.
type Probe = { Case: string; Expected: int; Actual: byte[] }

let probesOf (asm: Assembly) : Probe list =
    // future_salts: one salt, bare vector of a bare element.
    let saltType = asm.GetType("Altergram.Schema.FutureSalt", true)
    let salt = Activator.CreateInstance saltType
    set salt "ValidSince" 0x60000000
    set salt "ValidUntil" 0x60000E10
    set salt "Salt" 0x0102030405060708L
    let saltArray = Array.CreateInstance(saltType, 1)
    saltArray.SetValue(salt, 0)

    let futureSalts =
        serialize asm "FutureSalts" (fun o ->
            set o "ReqMsgId" 0x51E57AC42770964AL
            set o "Now" 0x60000000
            set o "Salts" saltArray)

    // msgs_ack: the boxed control.
    let msgsAck =
        serialize asm "MsgsAck" (fun o -> set o "MsgIds" [| 0x51E57AC42770964AL; 0x51E57AC42770964BL |])

    // req_pq_multi: a fixed-width scalar.
    let reqPq = serialize asm "ReqPqMulti" (fun o -> set o "Nonce" (Array.zeroCreate<byte> 16))

    // accessPointRule: the two bits are independent — a BARE vector whose
    // elements are BOXED. Drop the wrong one and this case still moves.
    //
    //   cid 4 + phone_prefix_rules "ru" (1 len + 2 + 1 pad) 4 + dc_id 4
    //   + count 4 + ipPort#d433ad73 (cid 4 + int 4 + int 4) 12  =  28
    let ipPortType = asm.GetType("Altergram.Schema.IpPort", true)
    let ipPort = Activator.CreateInstance ipPortType
    set ipPort "Ipv4" 0x7F000001
    set ipPort "Port" 443
    let ips = Array.CreateInstance(asm.GetType("Altergram.Schema.IpPortBase", true), 1)
    ips.SetValue(ipPort, 0)

    let accessPointRule =
        serialize asm "AccessPointRule" (fun o ->
            set o "PhonePrefixRules" "ru"
            set o "DcId" 2
            set o "Ips" ips)

    [ { Case = "future_salts     vector<future_salt>  bare vector, bare element"; Expected = 36; Actual = futureSalts }
      { Case = "accessPointRule  vector<IpPort>       bare vector, BOXED element"; Expected = 28; Actual = accessPointRule }
      { Case = "msgs_ack         Vector<long>         boxed vector  [CONTROL]"; Expected = 28; Actual = msgsAck }
      { Case = "req_pq_multi     nonce:int128         raw fixed width"; Expected = 20; Actual = reqPq } ]

// ── the run ──────────────────────────────────────────────────────────────────

// The generator refuses to start without one, and an empty file is what
// Altergram ships: every declaration then comes from the .tl and nowhere else.
let overrides = Path.Combine(repo, "tools/wire-probe-overrides.toml")
if not (File.Exists overrides) then
    File.WriteAllText(overrides, "# Intentionally empty: --overrides is mandatory and this probe wants\n# every byte to come from the .tl declaration.\n")

printfn "wire-probe — lengths required by the declaration, and what each build emits\n"
printfn "  schema   %s" (Path.GetRelativePath(repo, apiSchema))
printfn "           %s" (Path.GetRelativePath(repo, mtprotoSchema))

let working = compile "working" (emitWith "working" generatorProject)

/// The other build. A throwaway worktree so the working tree is never touched.
let baselineAsm =
    if baseline = "none" then
        None
    else
        let wt = Path.Combine(Path.GetTempPath(), "tdesu-wire-probe", "baseline-src")
        if Directory.Exists wt then
            run "git" [ "worktree"; "remove"; wt; "--force" ] repo |> ignore
        let code, log = run "git" [ "worktree"; "add"; "-f"; "--detach"; wt; baseline ] repo
        if code <> 0 then failwithf "cannot create a worktree at %s\n%s" baseline log
        try
            let project = Path.Combine(wt, "src/TDesu.Telegram.TL.Generator/TDesu.Telegram.TL.Generator.fsproj")
            Some(baseline, compile "baseline" (emitWith "baseline" project))
        finally
            run "git" [ "worktree"; "remove"; wt; "--force" ] repo |> ignore

printfn "  baseline %s\n" (match baselineAsm with Some(r, _) -> r | None -> "(skipped)")

let mutable failures = 0

let report (label: string) (p: Probe) (isWorking: bool) =
    let ok = p.Actual.Length = p.Expected
    let verdict = if ok then "ok " else "BAD"
    printfn "  %-10s %s  %2d bytes (declaration requires %d)  %s" label verdict p.Actual.Length p.Expected (hex p.Actual)
    // Only the working tree is asserted. The baseline is shown for contrast and
    // is allowed to be wrong — that is usually the whole point of running this.
    if isWorking && not ok then failures <- failures + 1

let baselineProbes = baselineAsm |> Option.map (snd >> probesOf)

probesOf working
|> List.iteri (fun i p ->
    printfn "\n  %s" p.Case
    match baselineProbes with
    | Some bs ->
        let b = bs[i]
        report "baseline" b false
        report "working" p true
        if b.Actual <> p.Actual then
            let d = p.Actual.Length - b.Actual.Length
            printfn "             %+d bytes vs baseline" d
    | None -> report "working" p true)

printfn ""

if failures = 0 then
    printfn "all %d cases match the length their declaration requires" (List.length (probesOf working))
    exit 0
else
    printfn "%d case(s) disagree with the declaration — this is a wire defect" failures
    exit 1
