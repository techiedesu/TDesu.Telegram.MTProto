boolFalse#bc799737 = Bool;
boolTrue#997275b5 = Bool;

message#5a686d7c flags:# out:flags.1?true text:string entities:flags.0?Vector<MessageEntity> = Message;

messageEntity#e4599bbd offset:int length:int = MessageEntity;

user#215c4438 id:long access_hash:long first_name:string = User;

inputPeerEmpty#7f3b18ea = InputPeer;
inputPeerSelf#7da07ec9 = InputPeer;
inputPeerChat#35a95cb9 chat_id:long = InputPeer;

---functions---
messages.sendMessage#dff8042c peer:InputPeer message:string = Updates;
auth.signIn#8d52a951 phone_number:string phone_code_hash:string phone_code:string = auth.Authorization;
