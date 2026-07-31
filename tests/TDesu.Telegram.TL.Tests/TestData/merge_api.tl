// Minimal API-side schema for the csharp mtproto-merge tests.
// Deliberately declares names that mtproto.tl also declares (`Message`, `ping`)
// so the api-wins collision rule has something to fire on.

boolFalse#bc799737 = Bool;
boolTrue#997275b5 = Bool;

message#5a686d7c flags:# out:flags.1?true text:string = Message;

---functions---

ping#12ab34cd ping_id:long = Bool;
