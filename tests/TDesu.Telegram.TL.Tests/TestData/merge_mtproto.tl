// Minimal MTProto-side schema for the csharp mtproto-merge tests.
// `bind_auth_key_inner` and `pong` are unique; `message` collides with the api
// record and the `ping` function collides with the api function.

int ? = Int;

bind_auth_key_inner#75a3f765 nonce:long temp_auth_key_id:long perm_auth_key_id:long temp_session_id:long expires_at:int = BindAuthKeyInner;

message#5bb8e511 msg_id:long seqno:int bytes:int = Message;

pong#347773c5 msg_id:long ping_id:long = Pong;

---functions---

ping#7abe77ec ping_id:long = Pong;
