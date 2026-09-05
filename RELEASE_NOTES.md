# Release notes

## 0.13.0

### Generator

**`GeneratedLayerCid` now advertises the schema's own layer instead of a hand-typed
literal.** `EmitTemplates.fs` baked `DefaultLayer = 223` into every emitted
`GeneratedCid.g.fs` regardless of the parsed `.tl` — the audit's measurement found a
real consumer running against layer 229 while the generated file still said 223, six
layers stale, because the parser already extracted `// LAYER N` and nothing read it.
`GeneratedLayerCid` now emits `[<Literal>] let Layer = <parsed layer>` and
`DefaultLayer` equal to it; a schema with no `// LAYER N` directive fails the `cid`
target outright instead of guessing. `MinSupportedLayer` (190) is unchanged. Pin
`CurrentLayer` to `GeneratedLayerCid.Layer` to turn the drift into a compile-time
equality.

**Removed dead surface named in the audit: `[layer_type_info]` overrides and the
`client-parsers` target.** `[layer_type_info]` was parsed by `Toml.fs` and read by
no target — confirmed against the generator's own `ignoredInputs` table, which
already warned every run that populated it. `client-parsers` had become an empty
module by construction once the whitelist's transitive closure shipped: `types`
already emits everything a response parser would need, so a `client-parsers` run
produced 20 lines of bare header. Both are gone — TOML config, DSL, CLI target,
`--client-namespace`'s second consumer, and the samples. `--target client-parsers`
now fails naming the removal and pointing at `types`; a stray `[layer_type_info]`
section is simply unread now, the same as any other TOML key the generator never
queries.

**Parser validation: `flags.N?Type` must name a real parameter.** The grammar
accepted any identifier before the `.`, so a typo'd or renamed flags field (writing
`flags2.0?T` in a combinator that only declares `flags:#`) parsed cleanly and only
failed once the emitter tried to read a field that was never there. `combinator`
now rejects the reference at parse time, naming the constructor and the bad
reference, for every `flags.N?Type` — including one nested inside `Vector<...>`.

**`--version`, and a `--split-by-class` inconsistency closed.** `td-tl-gen
--version` prints the tool's own informational version (MinVer-stamped) and exits;
previously the flag answered "unknown flag". `--split-by-class` without `--target
csharp` used to only warn that the flag was ignored — the same "flag silently did
nothing" shape `--split-by-scc` without `--split-by-domain` already treats as a hard
error. It is now the same hard error.

The generated-file banner already dropped the tool's own version in an earlier
fix on this branch (it used to rewrite every consumer file on a generator bump with
no code change) — confirmed unchanged here; nothing further to do for that item.

## 0.12.1

**`msg_detailed_info` is transport bookkeeping, and it was being published as an update.**
`msg_detailed_info#276d3ec6` and `msg_new_detailed_info#809db6df` come from `mtproto.tl`, not the
API schema, and neither appeared in the service-message dispatch — so both fell through to the
branch that treats an unrecognised constructor as a server push. Every one of them reached update
subscribers, which then failed deserializing an `Updates` and logged a genuine error for a message
that never carried an update. Measured on a live account: 28 such errors in one hour, which is also
how they buried real parse failures.

They arrived 28 times rather than once for a second reason. Both constructors name an *answer* the
server is holding, and the protocol expects that answer's `msg_id` to be acknowledged — not the
notification's own. Unacknowledged, the server simply keeps re-announcing it. Both now ack
`answer_msg_id` through the existing ack queue, which is what stops the repetition.

Pinned by two tests that put the service message in a container beside the `rpc_result` the call is
waiting for: when the call returns, the service message has already been processed, so counting what
the subscriber saw cannot race the receive loop. Both fail against the unfixed client.

## 0.12.0

**`rpc_error` now fails the request instead of completing it.** The dispatcher wrote every
reply — success or `rpc_error#2144ca19` — into the same completion slot and handed the raw
bytes straight to the caller as a successful result. Every consumer therefore saw `Ok` for a
server-side error and then failed deserializing an unrecognised constructor id, and no
`Error(MtProtoError.RpcError(code, message))` match anywhere in application code could ever
fire — silently disabling flood-wait handling and `SESSION_PASSWORD_NEEDED` detection in every
client built on this library. Found from a client that had to hex-dump a
`USERNAME_NOT_OCCUPIED` response to work out why it was arriving as success.

`RpcAsync` now completes an `rpc_error` reply as
`Error(MtProtoError.RpcError(errorCode, errorMessage))`, finally honouring the method's own
doc comment ("failures arrive as Error Results, never thrown") for the one case that never
actually kept that promise.

**Breaking, and it will not show up as a compile error.** `RpcAsync` already returned
`Result<byte[], MtProtoError>` and `RpcError` already existed as a case, so every call site
still type-checks — only the runtime routing changed. Audit every place that pattern-matches
on an `RpcAsync` result: it needs its own arm for
`Error(MtProtoError.RpcError(code, message))`, in particular flood-wait (`message` shaped like
`FLOOD_WAIT_<seconds>`) and `SESSION_PASSWORD_NEEDED`. **A call helper that maps every
`Error _` to "no answer" — logs it, retries on a fixed interval, or otherwise treats it as a
generic transport hiccup — will keep compiling and keep failing to back off on flood-wait**,
just for a different reason than before: it used to never see the error at all, and now it
sees the error but throws away the one field (`errorMessage`) that says how long to wait.
Either way the bot gets rate-limited harder the more it retries.

### Transport

**The WebSocket carrier and its AES-CTR keystream now run under WebAssembly.** Two independent
faults blocked a browser client: `Aes256Ctr` built its keystream with `Aes.Create()`, which
raises `PlatformNotSupported` on browser-wasm (`TDesu.Crypto.AesEcb` now supplies the cipher,
falling back to a managed implementation where the platform has none); and `WsTransport`'s
receive path carried a mutable result across an `await` inside a `while` loop, which stops F#
from generating a resumable state machine — the synchronous fallback it silently lands on
blocks the calling thread on every read, and a browser's single thread cannot wait at all
(`Monitor` raised "Cannot wait on monitors on this runtime" mid-handshake). The read path is
recursive now (`PumpAsync`), and a read that completes synchronously with zero bytes yields
once instead of spinning the only available thread until the connection times out.

### Protocol

**The encrypted receive loop is WASM-safe too**, for the same reason and in the same shape as
the transport fix above: the `while`/`try`/`await` loop is now a tail-recursive step
(`receiveStep`), so a session no longer comes up and immediately tears itself down one message
after `new_session_created` on a single-threaded runtime.

### Generator

**New `ergonomics` target.** `td-tl-gen --target ergonomics` emits `static member Create` on
records with optional/flag fields (required params positional, `flags.N?T` and presence-bools
as `?arg`), a `Create<CaseSuffix>` factory per non-empty union case, and `[<return: Struct>]`
active patterns for zero-allocation field extraction across union cases that share a field
name and type. Smoke-tested against api.tl: 160 `Create` members on records, 943 per-case DU
factories, 48 field-extractor active patterns.

**Transitive whitelist closure removes the practical need for `--no-whitelist`.** The `types`
target's seed set now also closes over every constructor named in `[whitelists].writers` /
`writer_layer_types`, so `writers`'s generated converters can reference only names `types`
actually emits, without falling back to emitting the full schema. Widening the closure
surfaced two latent `EmitWriters` bugs that `--no-whitelist` had always masked (a full schema
made the writer-whitelisted subset trivially equal to the true one): a converter's
record-vs-union shape decision was keyed off the writer-whitelisted count instead of the
schema's true count, and a field outside the writer whitelist was passed through unconverted
into a slot `EmitWriters` had already resolved to raw bytes. Both are fixed; an unsupported
request-side case now fails loudly at generation time instead of emitting a type mismatch.
Regenerating against a real whitelist-scoped config changes output shape where either bug
previously fired; a full-schema (`--no-whitelist`) regen is byte-for-byte unaffected.

**`--split-by-scc` shards an oversized `Base` domain file.** Combined with
`--split-by-domain`, a domain bucket over roughly 400 types / 1 MB is bin-packed into
`Base.NN.g.fs` shards along the same Tarjan-SCC order the single-file emitter already used for
`and`-chains, so a mutually recursive cluster never straddles two files. Opt-in; off by
default, and rejected with an error if passed without `--split-by-domain`.

**Managed-file hygiene.** Every emitted file now carries a stable `<auto-generated>` banner and
a `//# td-tl-gen-managed` marker, so the regeneration sweep deletes only files the tool itself
wrote — a hand-added `.fs` alongside the generated ones survives. (An alpha in this line
briefly renamed the extension to `.Generated.fs`; it is back to `.g.fs`, so a consumer coming
from 0.9.0 sees no extension change at all.)

**Breaking for direct library callers of the generator (not `td-tl-gen` CLI users).**
`Pipeline.generateSerializationTypesSplit` gained a required trailing `SccSplitConfig option`
parameter — pass `None` for the previous one-file-per-domain behaviour.
`EmitTypes.PerDomainOutput` gained `Types` and `Functions` fields, which only breaks a literal
record construction (`{ Domain = ...; Filename = ...; Code = ... }`); reading the existing
fields by pattern or by name is unaffected.

Also: field names that collide with F# keywords no longer emit a backtick-prefixed active
pattern the compiler rejects; active-pattern names are prefixed with their union's name so two
unions sharing a field no longer collide; the ergonomics emitter no longer `open`s
`TDesu.Serialization`, which was shadowing generated types of the same name; and generation
skips fields with ambiguous types across union cases and the unreachable `ValueNone` arm on
exhaustive DUs instead of emitting code that doesn't compile.

### Dependencies

- `TDesu.Telegram.Crypto` → 0.3.1 (adds the `AesEcb` fallback the WASM transport fix depends
  on, plus a safe-prime pin).

0.10.0 and 0.11.0 were never published as stable — both stayed on alpha prereleases while work
continued underneath them. This release is tagged 0.12.0, matching the prerelease line already
on this commit (`0.12.0-alpha.1` through `.4`), so nothing already pinned to one of those
prereleases sees a numerically lower "stable" release.

## 0.8.0

### Generator — C# backend

**`--target csharp` now honours `--mtproto-schema`.** The flag was global and only the `cid`
target ever read it: passing it to `csharp` parsed nothing, emitted nothing, warned about nothing
and exited 0. A consumer who followed its own documented regeneration command therefore got the
api.tl surface only, silently — and if it had a committed tree built by a build that did merge
mtproto.tl, regenerating deleted every transport type. The merge is now real: `bind_auth_key_inner`,
`msgs_ack`, `pong`, `rpc_error`, the ResPQ/DH handshake set and the rest land in the emitted C#,
with api.tl winning every collision on the emitted C# name (the loser is skipped and logged at
info). Against the real pair exactly one declaration is skipped — mtproto's `rpc_drop_answer`
function, which loses to the union of the same name.

**The C# emitter is a Roslyn tree, not a StringBuilder.** 428 lines of hand-counted indentation
became `SyntaxFactory` declarations with statement bodies parsed from fragments, and the finished
compilation unit is rejected if it carries a single parse diagnostic — the same contract the F#
side has via Fantomas. It caught `void` going through `ParseTypeName` on the first run.

**Wire fix: several fields behind one flag bit.** TL routinely puts more than one field on a
single bit (`my_boost` + `my_boost_slots`, and five more in api.tl). The bit was OR-ed from each
field independently, so a half-filled object set the bit and wrote no payload, desyncing the
reader from that offset on. The bit is derived once per (word, bit) now and disagreement throws
at serialize time.

**`GeneratedReturnTypes.g.cs`.** Request cid → the constructor ids its response may legally
carry, so a consumer can assert that a handler answers with the type its method declares.

**Emitted C# is LF-only on every OS.** `NormalizeWhitespace` defaults to `Environment.NewLine`,
so the same generator, schema and flags produced different bytes on Windows and Linux. Generated
trees get committed; the bytes are now the same everywhere. Nothing but line endings changed.

### Packaging

The README ships inside the packages, and CI is off the deprecated Node 20 runtime.

## 0.7.0

### Transport

**The WebSocket carrier can reach a data centre other than Telegram's.** `WsTransport` built its
URL from `dc.Id` alone and ignored `dc.Address`, so `TransportKind.WebSocket` could only ever dial
`wss://<name>.web.telegram.org/apiws` — a self-hosted or proxied deployment was unreachable over
wss, and the only way out was writing an `ITransport` by hand. The case now carries the endpoint:
`TransportKind.WebSocket of endpoint: Uri option`, where `None` keeps resolving Telegram's gateway
for `dc.Id`. An address and port cannot stand in for it, since a WebSocket needs a scheme, host
and path.

`WsTransport.Endpoint` exposes the resolved URL. `TransportKind` is `[<NoComparison>]` now, as
`Uri` is not structurally comparable; equality is unaffected.

Source-breaking for `TransportKind.WebSocket`: pass `None` for the previous behaviour.

## 0.6.1

### Protocol

**The keepalive now outpaces the WebSocket gateway's idle timeout.** Telegram closes an apiws
connection roughly 30 seconds after the last frame; the ping loop ran every 60, so on wss the
client never reached its own keepalive — it just got closed. Production saw a drop every 31-36
seconds, twenty in twenty minutes, each costing a reconnect, a gap-recovery pass and whatever RPCs
were in flight. The interval now follows the carrier: 20s on WebSocket and HTTP, 60s on raw TCP,
with `disconnect_delay` scaled to match.

## 0.6.0

An audit of the whole stack against the MTProto specification. The headline is a framing bug that
silently dropped messages on the WebSocket carrier; the rest is the long tail behind it.

### Transport

**The WebSocket carrier lost frames.** `WsTransport` treated one WebSocket message as exactly one
MTProto frame: anything past the first frame in a coalesced message was discarded without a word,
and a frame split across two messages failed as `InvalidFrame` and forced a reconnect. The gateway
relays an obfuscated *byte stream* — message boundaries mean nothing. It now decrypts chunks in
arrival order into a carry-over buffer and hands back one frame at a time. Anything that ran on
this carrier was losing RPC replies and push updates at a rate set by the gateway's batching.

**A failed read or write now retires the connection.** In every obfuscated carrier the CTR
keystream advances with the bytes, so a cancelled or partial read — or a write that failed after
`Process` — leaves the stream permanently out of step with the peer, while `IsConnected` kept
reporting true. `WsTransport` additionally derives `IsConnected` from the socket state instead of
a flag that only a Close frame cleared.

**HTTP carrier**: `Content-Length` is parsed and capped against `FrameCodec.MaxFrameLength`
(previously any 32-bit value went straight to an allocation), the inbound channel is rebuilt on
connect (a disconnect used to complete it permanently, so a revived transport dropped every reply
into a closed channel), the exchange lock is acquired inside the error contract, and the response
read has a deadline so one silent server cannot block every sender.

**Both TCP carriers** publish their state only after the transport header is written, and dispose
the half-built socket and ciphers when a connect fails. `FrameCodec.MaxFrameLength` is public.

### Protocol

**`new_session_created` is a gap notification, not a salt update.** The server had thrown the
session away, taking every in-flight request with it, and the client only wrote down the new salt:
the abandoned requests hung to their deadline and the updates missed in the gap were never
recovered. It now re-sends everything below `first_msg_id` and raises `Reconnected`, which is the
signal applications already use to re-run gap recovery.

**`bad_msg_notification` 16/17 corrects the time offset.** Clock skew (an NTP step, a suspended
host, a stale persisted offset) made the server reject every message the client sent, forever —
the client only logged it. It now recomputes `TimeOffset` from the notification's own msg_id,
clears the monotonic clamp and re-sends.

**The replay guard covers messages inside a container.** It was applied only to the outer msg_id,
but a retransmission keeps its own id and travels inside a *new* container: every re-sent update
was applied twice.

**A rekeyed request can still be timed out.** After `bad_server_salt`, `Rekey` moved the entry to
a new msg_id while the caller still waited on the old one, so its timeout removed nothing and the
task plus request body leaked until the next `FailAll`.

Also: unacknowledged `msgs_ack` ids are requeued instead of dropped (and chunked to the spec's
8192), a throwing update subscriber or unparseable message no longer fails every in-flight RPC and
forces a reconnect, a superseded receive loop cannot fail requests belonging to the connection
that replaced it, `SendUnencryptedAsync` takes the send lock, `RpcAsync` returns `Error` instead
of throwing when there is no session, and per-session state is cleared when a session is created.

### Hardening

Bounds on everything read off the wire that had none: gzip expansion is capped at the frame size
(nested `gzip_packed` could otherwise multiply into gigabytes), container count is limited to the
spec's 1024 with inner lengths checked against the frame, and the unencrypted-message body length
is validated before it is used — a negative one used to rewind the read cursor and return success.

The handshake no longer faults its task: `performExchange` maps every malformed response to
`Error`, the `resPQ` fingerprint vector count is bounded (it came from an unauthenticated message
straight into an allocation), `encrypted_answer` is length-checked before AES-IGE, a `pq` longer
than 8 bytes or smaller than 3 is rejected (the latter made Pollard rho spin forever), handshake
msg_ids are divisible by 4 as the spec requires, and `auth_key_aux_hash` is read little-endian.

`MessageFraming.decrypt` adds the spec's alignment and padding-range checks. Randomized
intermediate padding uses a cryptographic RNG.

**Session files are written atomically** and a corrupt or truncated one is rejected instead of
yielding a short auth key that breaks every send and receive. A refused `chmod 0600` is now an
error rather than a silent world-readable auth key.

## 0.5.2

### Protocol

**An RPC on a downed transport reconnects instead of failing forever.** Reconnects were driven
only by the receive loop, so once its three attempts were exhausted — or once something cancelled
them — the client kept a live-looking session with a dead carrier and answered every later call
with `ConnectionClosed` until the process was restarted. `RpcAsync` now starts a reconnect itself
when the transport is down, on a token of its own so one caller's timeout cannot abort a reconnect
every other caller is waiting for.

## 0.5.1

### Protocol

**A cancelled reconnect no longer wedges the client for good.** `reconnectInternal` set
`isReconnecting` and cleared it only on the way out; a cancelled backoff delay threw straight past
that, leaving the flag set forever. From then on no reconnect was ever attempted again and every
`RpcAsync` spent its reconnect wait (15s before the send, 15s after) before failing — a client that
looks alive, never recovers, and needs a process restart. The flag is now cleared in a `finally`.

**A disconnected client fails fast.** `Disconnect` marks the client closed, so `RpcAsync` answers
`ConnectionClosed` immediately instead of waiting for a reconnect that is not coming. A later
`ConnectAsync` / `ConnectWithAuthKeyAsync` revives the same instance.

## 0.5.0

### Transport

**The carrier now travels with the data centre.** `DataCenter` gained a `Transport:
TransportKind` field, `Transports.create` is the default factory, and `MtProtoClient` builds the
carrier the `DataCenter` asks for — at connect time and on every reconnect:

```fsharp
use client = new MtProtoClient(dc |> DataCenters.over TransportKind.WebSocket)
```

This replaces having to name a concrete transport type: a consumer that only ever passes a
`DataCenter` down its own stack can now choose the carrier where it initialises the connection.
An explicit `transportFactory` still overrides the kind.

**Breaking:** `DataCenter` has a fourth field, so records built literally need
`Transport = TransportKind.Tcp` (or `DataCenters.over`). `DataCenters.production` / `test` are
unchanged and stay on raw TCP.

## 0.4.0

### Transport

**Four new transports behind a shared `ITransport` abstraction**, selectable with
`MtProtoClient(dc, transportFactory = …)`:
- `TcpObfuscatedTransport` — obfuscated TCP (obfuscation2), abridged or intermediate framing.
- `WsTransport` — obfuscated MTProto over WebSocket binary frames.
- `HttpTransport` — MTProto over HTTP/1.1.
- `FakeTlsTransport` — MTProxy fake-TLS (`ee`-secret + fronting domain).

`TcpTransport` keeps the raw-TCP intermediate behaviour. Shared obfuscation
(AES-256-CTR) and the frame codecs live in `Obfuscation` / `FrameCodec`.

### Security

**Auth key exchange hardening.** The DH handshake now validates every server-supplied
value it previously trusted blindly: the echoed nonces in `server_DH_params` and
`dh_gen_ok`, the `server_DH_inner_data` SHA1 integrity prefix, `new_nonce_hash1`, and
the DH parameters (`g`, `dh_prime`, `g_a`, and our own `g_b`) via the hardened
`TDesu.Telegram.Crypto` validators. **RSA_PAD is now the default** for `p_q_inner_data`
(sent as `p_q_inner_data_dc`); the server's advertised key order is honoured so the
RSA_PAD key is selected. The classic RSA scheme remains available in the crypto library.

**Message layer.** Constant-time `msg_key` comparison, verification that a decrypted
message's `session_id` matches the current session, and a bounded inbound `msg_id`
replay guard. Persisted session files are now created owner-only (0600 on Unix); the
blob still holds the plaintext auth key, so wrap `ISessionStore` to encrypt at rest.

### Dependencies

- `TDesu.Telegram.Crypto` → 0.3.0 (RSA_PAD + hardened DH validation).

## 0.3.1

### Protocol

**Reconnect-aware RPC dispatch.** `MtProtoClient.RpcAsync` now waits for
an in-progress auto-reconnect to complete before returning a
`ConnectionClosed` error. Previously, the caller's retry loop would
immediately re-send on the same dead transport, burning all retry
attempts in <1 s while the reconnect was still establishing a new TCP
socket. Now the RPC blocks up to 15 s on the `Reconnected` event,
so the first retry hits a live connection.

## 0.3.0

### Transport

**TCP keepalive for dead connection detection.**
`TcpTransport.ConnectAsync` now sets `SO_KEEPALIVE` on the socket with
aggressive timings: 5 s idle → 5 s probe interval → 3 retries (≈ 20–25 s
detection). Fixes half-open TCP connections that caused `CallRawAsync` to
hang indefinitely — the app-layer `ping_delay_disconnect` could not detect
the dead socket because the write side still appeared writable.

### TL Generator

**C# code generation backend.** `td-tl-gen` gains `--lang csharp` emitting
single-layer C# TL types, writers, and constructor-ID tables. Union types
use a `<Name>Base` abstract class with top-level case classes. Non-nullable
reference fields are default-initialized to silence CS8618.

## 0.2.10

**Bugfix: structural overlays now gate a field already present in the base
schema.** When the primary schema advances past a `[[structural_overlays]]`
entry's `max_old_layer` and pulls the overlay field into the base combinator
(e.g. layer-225 `dialog#fc89f7f3` already carries `unread_poll_votes_count`),
the idempotency guard skipped the overlay entirely — so the field lost its
`LayerGate` and was written unconditionally. A caller at `layer <= max_old_layer`
gets the OLD constructor CID via `[[layer_variants]]` (e.g. `dialog#d58a08c6`,
whose wire shape has no `unread_poll_votes_count`) but the NEW struct bytes,
desyncing the reader. Observed live as a Telethon (layer 216)
`TypeNotFoundError` reading CID `0x00000000` on `messages.getDialogs`.

`applyStructuralExtras` now stamps `LayerGate = Some max_old_layer` onto the
pre-existing base field instead of no-op'ing, so the writer wraps it in
`if layer > max_old_layer then …` at the correct wire position. Callers
> `max_old_layer` are byte-identical; callers ≤ it no longer get the phantom
field. Regression test: `WriterGeneratorTests.structural overlay gates a
field already present in the base schema`.

## 0.2.9

**Bugfix: union-case `Serialize` now emits the `flags:#` int32.**
For union cases whose schema declares a `flags:#` integer with flag-bound
optional or presence-bool fields (e.g. `inputBotInlineResult#88bf9319
flags:# id:string type:string title:flags.1?string …`), the previous
emitter went straight from `WriteConstructorId` to writing the first
non-flag field. `Deserialize` on the other side dutifully called
`ReadInt32()` for the flags slot, so round-trip through our own
serializer was misaligned by 4–N bytes and the next struct CID came
out as garbage (e.g. `0x6E656D75` — bytes of the *next* string instead
of a real CID).

Discovered while writing tests for the localgram inline-bot
`messages.setInlineBotResults` handler: passing typed `InputBotInlineResult`
values through `MessagesSetInlineBotResults.Serialize` produced bytes
that the handler's own typed `Deserialize` couldn't parse with
`Unknown constructor id for InputBotInlineMessage: 0x6E656D75`. Real
clients (Telethon, tdesktop) emit the flags correctly, so the
production parsing path was always fine — only own-Serialize → own-
Deserialize round-trips broke.

Fix: `mkUnionSerializeMember` in `EmitTypes.fs` now wraps each match
clause in `let mutable <flagField> = 0`, emits the `if ... then flags
<- flags ||| (1 <<< bit)` chain for flag-bound fields (optional →
`field.IsSome`; presence-bool → the field itself), then `writer.WriteInt32(flags)`
before the per-field writes. Presence-bool fields are dropped from the
per-field write list — they contribute to the flags int32 only.

`flagsComputationExprs` refactored into a `flagsComputationExprsWith`
helper parameterized on field-access (records keep `value.X`; unions
pass the locally destructured `f.Name`). 77/77 generator tests still
pass.

## 0.2.8

**Bugfix: SCC types now actually emit as `and`-chains.** 0.2.7 grouped
mutually recursive types into one `SynModuleDecl.Types([t1; t2; t3], r)`
block, but every emitted decl carried `SynTypeDefnLeadingKeyword.Type`,
so Fantomas rendered the block as three independent `type X = ...`
declarations instead of `type X = ... and Y = ... and Z = ...`. F# then
treated each as a fresh scope, and forward references between them
failed to compile (`error FS0039: The type 'X' is not defined` — the
exact symptom 0.2.7 was meant to fix).

Fix: `renderTypeSccs` (and the per-domain equivalent in
`buildPerDomainModules`) now emits the first decl in each SCC with
`SynTypeDefnLeadingKeyword.Type` and all subsequent decls with
`SynTypeDefnLeadingKeyword.And`. Singleton SCCs stay on `Type` (the
common case). Verified on layer 225 against localgram: the
`MessageMedia ↔ Poll ↔ PollAnswer` 3-cycle now collapses into one
`type … and … and …` group at the correct topological position. 77/77
generator tests still pass.

The 0.2.7 changelog claimed this worked. It didn't — the SCC detection
was correct (Tarjan is fine) but the AST construction layer dropped
the recursion-keyword marker. 0.2.8 actually closes the loop.

`buildRecordDecl` and `buildUnionDecl` keep the old `Type` default for
single-decl callers; the new `buildRecordDeclWithKeyword` /
`buildUnionDeclWithKeyword` are the SCC-aware variants used by the
multi-decl path.

## 0.2.7

**SCC-aware type emission for mutually recursive TL constructors.** Layer
225 (the upstream tdesktop `dev` branch as of May 2026) introduced two
mutual cycles in the `Base` domain that broke the previous DAG-only topo
sort:

* `MessageMedia.MessageMediaPoll` carries `attached_media: MessageMedia option`
  — a self-loop.
* `PollResults.SolutionMedia: MessageMedia option` and
  `MessageMedia.MessageMediaPoll(_, results: PollResults, _)` — a 2-cycle
  between `MessageMedia` and `PollResults`.

The old `topoSortTypes` walked the dependency graph DFS-style and emitted
in reverse-finish order, which works for DAGs but produces forward
references when nodes participate in a cycle. Compiling the result
yielded ~20 `error FS0039: The type 'MessageMedia' is not defined` errors
on `Base.g.fs`.

0.2.7 replaces the topo sort with **Tarjan's strongly-connected-components
algorithm** (`topoSortSCCs` in `EmitTypes.fs`). Each SCC is emitted as a
single `SynModuleDecl.Types([t1; t2; ...], r)` block, which F# renders
as `type X = ... and Y = ...`. Singleton SCCs (the common case) keep
emitting as `type X = ...`. SCCs are returned in topological order of
the condensation, so dependency direction is preserved between unrelated
groups.

The same change applies to the per-domain split path
(`buildPerDomainModules`): within each domain group, types are now
SCC-sorted before emission. The existing Base-promotion pass keeps
cycles from crossing domain boundaries.

**Idempotent structural overlays.** `[[structural_overlays]]` entries
that target a field name already present on the base TL type are now
silently skipped (`applyStructuralExtras` in `CodeModel.fs`). This lets
old override files survive an upstream schema bump that pulled their
extras into the base definition without producing duplicate-field
compile errors. The redundant overlay entries can then be retired at
the operator's leisure rather than emergency-removed during the bump.

**Other:**

* `extractTypeRefs` now strips ` option` and ` array` suffixes
  iteratively so compound types like `X option array` resolve to `X`
  (previously stopped after one strip). Dependency tracking is now
  complete for any combination.
* `nameOf`, `extractTypeRefs`, `depsOf` extracted as private
  module-level helpers shared by both topo paths.

77/77 generator tests still pass.

## 0.2.6

**Hotfix.** 0.2.5's `Requests.targets` header contained `--` inside the
XML comment body (`<!-- ... --target types --split-by-domain. -->`), which
violates the XML 1.0 spec and is rejected by MSBuild
(`An XML comment cannot contain "--"`). 0.2.6 replaces the comment with a
`--`-free phrasing. No other changes.

## 0.2.5

**Per-domain split for the `types` target.** `td-tl-gen --target types
--split-by-domain` now emits one `<Domain>.g.fs` file per TL domain prefix
(`Account.g.fs`, `Auth.g.fs`, … `Messages.g.fs`, plus `Base.g.fs` for
unprefixed types) under `<output>/Requests/`, instead of a single 30K-line
`GeneratedTlRequests.g.fs`. A `Requests.targets` MSBuild manifest is also
emitted so consumers can `<Import Project="…\Requests.targets" />` to pick
up the per-domain `<Compile Include>` entries in topological compile order
(`Base` first; cross-domain refs are limited to `Base` ↔ specific domain by
the splitter's cycle resolution).

* Cycle resolution: any non-Base type referenced from a Base type is
  promoted to Base transitively (today only `MessagesEmojiGameOutcome`
  qualifies on the upstream tdesktop schema).
* Default domain list: `Account, Auth, Bots, Channels, Chatlists, Contacts,
  Fragment, Help, LangPack, Messages, Payments, Phone, Photos, Premium,
  SmsJobs, Stats, Stickers, Stories, Updates, Upload, Users`. Override with
  `--split-domains "A,B,C"` if you need a custom prefix list.
* The default (no flag) is unchanged — single-file output for backwards
  compatibility.

Migration for downstream `.fsproj` (was: `<Compile
Include="Generated\GeneratedTlRequests.g.fs" />`):

```xml
<Import Project="Generated\Requests\Requests.targets" />
```

`buildPerDomainModules` is also exposed as a public API on `EmitTypes` for
callers that want to do their own writing.

## 0.2.4

**Auto-plumb `[[layer_variants]]` CIDs into `AliasCids`.** Previously
declaring a `[[layer_variants]]` entry produced a runtime alias map but
didn't set the static `AliasCids: uint32[]` member on the generated type.
Pre-0.2.4 callers that walked `AliasCids` (e.g. for coverage or client-side
dispatch) missed the per-layer CIDs. 0.2.4 unifies the two paths so both
reflect every layer-variant CID.

## 0.2.3

**Structural layer overlays** — write-side support for schema additions at a
newer layer without a hand-rolled hotfix module.

Before 0.2.3 the `[[layer_variants]]` mechanism could only dispatch different
CIDs at runtime; the record shape was fixed to the primary schema. Layer 223
of the Telegram API introduced a handful of constructors where the struct
layout *itself* changed (e.g. `dialog` gained `unread_poll_votes_count:int`
between `unread_reactions_count` and `notify_settings`). Emitting those
constructors per-layer previously required maintaining a separate
`LayerHotfix.fs` in downstream code that duplicated the writer body.

New TOML section:

```toml
[[structural_overlays]]
name = "Dialog"
max_old_layer = 216
extra_fields = [
  { after = "unread_reactions_count", name = "unread_poll_votes_count", type = "int" },
]
```

Generator behaviour:

* The extra field is spliced into the combinator's field list immediately
  after the named `after` anchor. It appears on `WriteDialogParams` as a
  regular scalar (default `0` via `defaultWriteDialog`).
* The writer function now takes `layer: int` (automatic — the generator
  treats structural-overlay combinators as layer-aware for free). Writes
  for overlay-only fields are wrapped in `if layer > max_old_layer then
  ...`, emitted at the correct wire position. Callers ≤ `max_old_layer`
  see byte-identical output to pre-0.2.3.
* Combined with `[[layer_variants]]`, the result is: layer ≤ N callers
  get the old CID + old struct bytes; layer > N callers get the new CID
  + new struct bytes with the extra field slotted in.

Supported scalar types: `int`, `long`, `string`, `bytes`, `bool`, `double`.
Flag-bit additions and record/DU-typed insertions are not handled by
`structural_overlays` — those still need a hand-rolled hotfix or a future
generator extension.

Regression test: `WriterGeneratorTests.structural overlay splices extra
scalar and layer-gates the write`.

## 0.1.16

**Wire-format fix for opaque-ref union fields outside the writers whitelist.**

When a writer-whitelisted record referenced a TL union/single that was not in
the writers whitelist, `EmitWriters.resolveFieldType` fell back to the F# type
`"byte[]"`, which routed the writer through `WriteBytes` (length-prefixed TL
`bytes` primitive). For boxed-type-ref opaque payloads the wire format requires
the bytes to be emitted *raw* (caller pre-serializes constructor id + payload),
not wrapped in a bytes envelope.

Symptom: clients reading such a field saw the 4-byte length envelope as the
next constructor id and crashed. A downstream server caught it as
`TypeNotFoundError(constructor=0x00000000)` from Telethon parsing the
`documentAttributeSticker.stickerset:InputStickerSet` field that
`messages.getAvailableReactions` writes.

Fix: the fallback now uses the existing `rawBytes` sentinel (introduced in
0.1.5 for the hardcoded `opaqueTypes` set), so the writer emits
`WriteRawBytes`. F# field type still surfaces as `byte[]` — caller-facing API
is unchanged. Callers that previously worked around the bug with hand-written
raw bytes followed by `WriteBytes` will need to drop the length-prefix
preamble.

Regression test: `WriterGeneratorTests.opaque-ref field outside writers
whitelist emits WriteRawBytes`.

Affects every writer-whitelisted record that has a field typed by a union
not in the writers whitelist.

## 0.1.11

**Type-level bundling for shared-flag-bit fields** in writer DUs and per-case records.

Multiple fields that share the same `flags.N?` bit (e.g. `codeSettings#ad253d78
token:flags.8?string app_sandbox:flags.8?Bool`) are now collapsed into a single
`(T1 * T2) option` in the generated DU case / record. Previously they were
emitted as independent options, allowing the impossible state
`{ token = Some "x"; appSandbox = None }` which silently corrupted wire format.

Generated code now reads:

```
type WriteCodeSettings = {
    ...
    tokenAndappSandbox: (string * bool) option
}
```

with the writer destructuring the tuple via `match`:

```
match p.tokenAndappSandbox with
| Some (_b0, _b1) -> w.WriteString(_b0); w.WriteBool(_b1)
| None -> ()
```

This is a **breaking change** for callers that previously constructed records or
DU cases with the now-merged fields. Migration is mechanical: replace
`{ ...; field1 = a; field2 = b; ... }` with
`{ ...; field1Andfield2 = Some (aValue, bValue); ... }`.

10 bundled fields appear across a real downstream whitelist after this change.
Affected upstream constructors (when whitelisted): `message` (views/forwards),
`channelFull` (kickedCount/bannedCount, requestsPending/recentRequesters,
migratedFromChatId/migratedFromMaxId), `pollResults` (solution/solutionEntities),
`webPage` (embedUrl/embedType, embedWidth/embedHeight),
`peerSettings` (requestChatTitle/requestChatDate, businessBotId/businessBotManageUrl),
`availableReaction` (aroundAnimation/centerIcon).

Naming convention: first field's name + `And` + second field's name (camel-cased).

## 0.1.10

Fix for `rawBytes` sentinel inside composite type spellings. `mkSynType` in
`Fantomas/TypeBuilder.fs` only collapsed bare `"rawBytes"` → `"byte[]"`; for
DU case fields whose F# type was a composite string like `"rawBytes option"`
or `"rawBytes array"`, the sentinel survived emission and produced
`type X = { f: rawBytes option }` which doesn't compile.

`mkSynType` is now recursive and parses ` option`/` array` suffixes the same
way `EmitWriters.toSynType` already did, so the sentinel collapses correctly
inside composites.

Consumers can drop the `: rawBytes option` → `: byte[] option` /
`: rawBytes array` → `: byte[] array` post-processing they may have added
as a workaround (one consumer had this as a `patchRawBytes` step in its regen script).

Snapshot tests updated accordingly: arrays in record fields now render as
`T[]` (the SynType.Array AST node) instead of being passed through as raw
text — both spellings are equivalent F#, but the previous output was a
side effect of the buggy passthrough.

## 0.1.9

Fix for `writer_record_per_case_unions` when a union has cases with no
data fields. Previously the generator emitted an empty record
(`type WriteFooBarParams = { }`) which F# rejects ("expecting record
field"). Empty cases now skip the per-case record entirely and stay as
bare `| CaseName` in the union.

Also flags a known generator-emit gotcha (not fixed here): for unions
NOT in `writer_record_per_case_unions`, fields are destructured
positionally — if any case has a field named `w` (the write-buffer
parameter name) or `h`, the destructuring shadows the outer scope. The
existing workaround is to add the union to `writer_record_per_case_unions`,
which switches dispatch to record access (`p_.w`) and avoids shadowing.

## 0.1.8

Wire-format fix: opaque-type-ref `byte[]` fields now serialize with
`WriteRawBytes` (raw blob — caller provides a complete pre-serialized TL
value) instead of `WriteBytes` (TL `bytes` primitive — length-prefixed).

The bug was: the generator's "fallback to byte[]" for unwhitelisted /
hardcoded-opaque types (`Page`, `RichText`, `MediaArea`, …) emitted a
field type of `byte[]` and a write call of `WriteBytes`. For a TL `bytes`
primitive that's correct, but for a boxed type ref it produces wrong wire
output (extra `bytes`-ctor + length + padding wrapped around the value).

Implementation: `CodeModel.mapPrimitiveType` now returns the internal
sentinel `"rawBytes"` for opaque types instead of `"byte[]"`. The
sentinel collapses back to `byte[]` at F# emit time (`mkSynType`,
`toSynType`) but the writer-call path checks for it and emits
`WriteRawBytes` instead of `WriteBytes`.

Read side stays on `ReadBytes` for opaque refs — without schema knowledge
the reader can't structurally parse a boxed type ref. Callers that need
to deserialize must whitelist the opaque type's constructors.

In practice: the original consumer always passes `None` / `[||]` for opaque-typed
fields, so this bug never fired at runtime. Future consumers that
actually populate these fields now get correct wire output.

## 0.1.7

Refines the per-case record naming added in 0.1.6: when a case name starts
with the type name, the duplicated prefix is dropped. So instead of
`WriteMessageMessageParams` and `WriteMessageMessageServiceParams`, the
generator now emits `WriteMessageParams` and `WriteMessageServiceParams`.

This keeps the single-case record name `Write{TypeName}Params` stable
across a single→multi-case migration: existing record-with construction
still type-checks, only the union-case wrap becomes necessary at the
point of consumption.

## 0.1.6

New writer-target feature: **per-case records for multi-case unions**, opt-in
via `[whitelists].writer_record_per_case_unions = ["Message", ...]` in the
override TOML.

By default, a multi-case union `WriteX` is emitted with positional case
constructors:

```fsharp
type WriteX =
    | Foo of a: int * b: string * ... 40 fields
    | Bar of x: int * y: int
```

For unions whose F# result-type name (PascalCase) appears in the
`writer_record_per_case_unions` whitelist, the generator instead emits a
record per case and references it from the union:

```fsharp
type WriteXFooParams = { a: int; b: string; ...; (40 fields) }
type WriteXBarParams = { x: int; y: int }
type WriteX =
    | Foo of value: WriteXFooParams
    | Bar of value: WriteXBarParams
```

This makes record-with construction syntax usable at callsites
(`WriteX.Foo({ defaultFoo with a = ... })`) instead of forcing positional
spelling of every field. Useful for unions like the schema's `Message`
that have many fields per case.

Backwards-compatible: unions not listed keep their existing shape.

## 0.1.5

Fix `WriteVector` emission in `EmitTypes.serializeExprFor`: the previous
form `writer.WriteVector (v) (lambda)` is parsed by F# as a curried call,
but `TlWriteBuffer.WriteVector` is a tuple-taking member
(`WriteVector(items: 'a[], writeItem: ...)`) — so the generated code didn't
type-check at the consumer. Now emits `writer.WriteVector(v, lambda)`.

This bug only surfaces when the generated `*.Serialize` member contains a
`Vector<T>` field, so it didn't trip the generator's snapshot tests.
Caught downstream when a consumer's regen produced ~100 build errors.

## 0.1.4

Two emit fixes that previously required hand-restoring `.g.fs` files after
running the generator:

- **Empty `aliases` array offside.** `EmitTemplates.generateLayerAliases`
  emitted `[|\n    |]` when the alias list was empty, which F# parses as
  offside and breaks the build. Now collapses to a single-line `[||]`.
- **`get` accessor for keyword-named fields.** `EmitTypes.mkFieldAccessors`
  pascal-cased keyword-escaped field names like `` ``type`` `` directly,
  producing invalid `` get``type`` `` member names. Now strips the
  surrounding backticks before pascal-casing, yielding `getType`.

## 0.1.3

Allow `GeoPoint` to resolve structurally when its constructors are in the
writers/types whitelist.

Until this release `GeoPoint` was hardcoded in the generator's `opaqueTypes`
list, forcing every field referencing it to emit as `byte[]` regardless of
whitelist — i.e. there was no way to get `WriteGeoPoint.GeoPoint(lon, lat, …)`
out of `td-tl-gen`. `GeoPoint` is a small, non-recursive type with only two
constructors, so the recursion-avoidance rationale that applies to `Page` /
`PageBlock` / `RichText` doesn't hold. `GeoPoint` is now structural by default.

Callers that referenced `geo: byte[]` via the old opaque path do not break on
the package upgrade alone, but the next time they regen writers (whitelist
includes `geoPoint` / `geoPointEmpty`) the field type will change; update
callsites accordingly.

Known issue (pre-existing, not fixed here): `byte[]` fields from remaining
opaque refs (`Page`, `RichText`, `MediaArea`, `MessageExtendedMedia`, …) are
serialized with `WriteBytes` (TL `bytes` primitive, length-prefixed) rather
than `WriteRawBytes`. This is a wire-format bug that only bites if you pass
non-empty data; `None` / empty vectors are safe. Planned fix in 0.2.0.

## 0.1.2

Release plumbing fix — no API changes. Adds `MinVerTagPrefix=v` to
`Directory.Build.props` so the release workflow actually computes the version
from `v*` tags. Prior `v0.1.0` and `v0.1.1` tags were packed as
`0.0.0-alpha.0.2` because MinVer ignored the `v` prefix with no configuration.
Consumers on 0.1.x should install 0.1.2 (first correctly-versioned 0.1
release on NuGet).

## 0.1.0

First proper release. The 0.0.0-alpha.0 generator was one project's private helper that
happened to be packaged as a NuGet tool — its hardcoded paths, namespaces and
embedded "default" overrides made it unusable for anyone outside the original
project. 0.1.0 reshapes it into a generic dotnet tool.

This release covers both packages in this repository:

- **TDesu.Telegram.TL 0.1.0** — TL parser and AST. No public API changes from
  0.0.0-alpha.0; just out of alpha.
- **TDesu.Telegram.TL.Generator 0.1.0** (`td-tl-gen`) — major CLI rework, see below.

### Breaking — `td-tl-gen` CLI rework

The CLI is rewritten end-to-end. The old free-floating flags
(`--cid-only`, `--serialization-types`, `--writers`, ...) wrote to hardcoded paths
and used hardcoded namespaces; they're replaced by an explicit
`--target` selector that writes to a directory you choose, with a namespace you
choose.

```sh
td-tl-gen \
  --schema cached/api.tl \
  --output ./Generated \
  --namespace MyApp.Serialization \
  --overrides path/to/overrides.toml \
  --target cid,types,writers,coverage,return-types
```

Required flags:
- `--schema <path>` — TL schema file (e.g. `cached/api.tl`)
- `--output <dir>` — directory where generated `.g.fs` files are written
- `--namespace <ns>` — F# namespace for emitted code
- `--overrides <toml>` — TOML override config (no embedded default in 0.1.0+)
- `--target <names>` — comma-separated targets

Available targets:

| Target | Output file | Notes |
|---|---|---|
| `cid` | `GeneratedCid.g.fs` | Requires `--mtproto-schema <path>` |
| `types` | `GeneratedTlRequests.g.fs` | Whitelist-filtered request types |
| `writers` | `GeneratedTlWriters.g.fs` | Standalone `write{X}` functions and `Write*` DUs |
| `coverage` | `GeneratedCoverageValidator.g.fs` | Handler coverage validator |
| `return-types` | `GeneratedReturnTypes.g.fs` | CID → return type lookup |
| `tests` | `GeneratedRoundTripTests.g.fs` | Round-trip tests for whitelisted requests |
| `layer-aliases` | `GeneratedLayerAliases.g.fs` | Requires `--layer-base-schema <path>` |
| `client-cids` | `GeneratedClientCid.g.fs` | Flat literal CID table |
| `client-parsers` | `GeneratedResponseParsers.g.fs` | Driven by `[whitelists].client_parsers` |
| `all` | (multi) | Equivalent to `cid,types,writers,coverage,return-types` |

Optional flags:
- `--mtproto-schema <path>` — required by the `cid` target
- `--layer-base-schema <path>` — required by the `layer-aliases` target
- `--tests-namespace <ns>` — override the `tests` module name (default `<ns>.Tests.GeneratedRoundTripTests`)
- `--client-namespace <ns>` — override the namespace for `client-cids`/`client-parsers` (default `<ns>.Client.Api`)

### Removed — embedded `DefaultOverrides.toml`

0.0.0-alpha.0 shipped a 484-line `DefaultOverrides.toml` as an embedded resource
inside the generator binary. It was the original project's whitelist verbatim and was
implicitly applied to every invocation, regardless of who ran the generator. There
was no way to opt out.

0.1.0 removes the embedded resource entirely:
- `Config.loadDefaults()` and `Config.loadWithOverlay(...)` are gone
- `Config.load(path)` and `Config.loadMerged(paths)` replace them
- `--overrides <toml>` is **required** on every invocation
- The previous embedded TOML is preserved verbatim under
  `samples/ServerOverrides/server-overrides.toml` as a worked example

### Removed — hardcoded `["Message"; "User"; "Chat"]` whitelist

`Pipeline.generateClientParsers` previously had a hardcoded mini-whitelist of three
constructor names baked into the code. It's now driven by `OverrideConfig.ClientParserWhitelist`,
populated from a new TOML field:

```toml
[whitelists]
client_parsers = ["Message", "User", "Chat"]
```

Empty whitelist produces an empty parsers module — no implicit defaults.

### Removed — `Pipeline.generate` (raw types dump)

The all-in-one mode that wrote `MTProtoTypes.g.fs`/`ApiTypes.g.fs`/`ApiFunctions.g.fs`
to a hardcoded sibling project is gone. Use the explicit `--target` flags.

### Internal changes

- All `EmitTemplates.generate*` and `Pipeline.generate*` functions now take an
  explicit namespace parameter (no more hardcoded `"TDesu.Serialization"` etc.)
- `EmitWriters.generateWriterModule` takes a namespace parameter
- `Pipeline.fs` no longer hardcodes `__SOURCE_DIRECTORY__`-relative output paths
- The `samples/CustomOverrides` example shows the F# library API

### Migration

If you were calling `td-tl-gen` directly:

```sh
# 0.0.0-alpha.0
td-tl-gen --writers
td-tl-gen --serialization-types
td-tl-gen --cid-only

# 0.1.0
td-tl-gen --schema cached/api.tl --output ./Generated \
          --namespace YourApp.Serialization \
          --overrides your-overrides.toml \
          --target writers,types
td-tl-gen --schema cached/api.tl --mtproto-schema cached/mtproto.tl \
          --output ./Generated --namespace YourApp.Serialization \
          --overrides your-overrides.toml --target cid
```

If you were calling the F# API directly (via project reference), every
`generate*` function now takes a namespace as its first parameter:

```fsharp
// 0.0.0-alpha.0
let code = EmitTemplates.generateCidModule config mtprotoSchema apiSchema
Pipeline.generateSerializationTypes config apiSchema outputPath

// 0.1.0
let code = EmitTemplates.generateCidModule "YourApp.Serialization" config mtprotoSchema apiSchema
Pipeline.generateSerializationTypes "YourApp.Serialization" config apiSchema outputPath
```

## 0.9.0

Breaking: `AuthKeyExchange.factorizePQ` takes a `CancellationToken` and returns
`Result<uint64 * uint64, MtProtoError>` instead of a bare tuple. Nothing outside
`performExchange` called it, but it is public, so this is a minor bump rather
than a patch.

- **fix(auth): bound `factorizePQ`.** `pq` is parsed off an unauthenticated,
  pre-handshake `resPQ`, so the server chooses how much CPU the client spends.
  Neither Pollard-rho loop polled the token or counted iterations, and nothing
  established that the input was a semiprime. Measured: the largest prime below
  2^63 runs ~24 minutes uninterruptible and then returns `(1, pq)`, which is not
  a factorisation — so the defect had a silent mode as well as a loud one. Capped
  at 32*sqrt(2147483629) iterations, shared across both attempts; a result that
  does not multiply back to `pq` with both factors above 1 is an `Error` even
  inside budget. Cancellation is now immediate and free (~1 ns polled against a
  0.48 us iteration). The realistic case is unchanged at 10.8 ms.
- **fix(tl): bare `vector<T>` is not boxed `Vector<T>`.** The parser folded case
  and the IR could not carry the distinction, so every bare vector went out with
  the boxed `0x1CB5C415` header. `future_salts` was 44 bytes where the wire
  format is 36, and a real client read that header as an element count of
  482,092,053. `int128`/`int256` travelled the same road as length-prefixed
  `bytes`. Both were invisible to a round-trip test — reader and writer were
  wrong the same way.
- **fix(cli): an unknown argument is an error.** `--mtproto-schemaa` parsed as
  nothing, emitted 37 fewer types and exited 0.
- **fix(tests): a missing snapshot now fails** instead of writing the golden file
  and passing, and the comparison no longer strips `\r\n` before diffing.
