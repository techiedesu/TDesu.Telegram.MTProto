using TDesu.Telegram.TL;

const string schema = """
                      boolFalse#bc799737 = Bool;
                      boolTrue#997275b5 = Bool;

                      inputPeerEmpty#7f3b18ea = InputPeer;
                      inputPeerSelf#7da07ec9 = InputPeer;
                      inputPeerChat#35a95cb9 chat_id:long = InputPeer;

                      message#5a686d7c flags:# out:flags.1?true text:string entities:flags.0?Vector<MessageEntity> = Message;

                      ---functions---
                      messages.sendMessage#dff8042c peer:InputPeer message:string = Updates;
                      auth.signIn#8d52a951 phone_number:string phone_code_hash:string phone_code:string = auth.Authorization;
                      """;

// Parse — throws FormatException on error
var result = TlParser.Parse(schema);

Console.WriteLine($"Parsed: {result.GetConstructors().Length} constructors, {result.GetFunctions().Length} functions");
Console.WriteLine($"Layer: {result.GetLayer()?.ToString() ?? "none"}");
Console.WriteLine();

// Iterate constructors
Console.WriteLine("=== Constructors ===");
foreach (var ctor in result.GetConstructors())
{
    var ns = ctor.Id.GetNamespace();
    var prefix = ns != null ? $"{ns}." : "";
    var cid = ctor.GetConstructorId();
    var cidStr = cid.HasValue ? $"#{cid.Value:X08}" : "";

    Console.WriteLine($"  {prefix}{ctor.Id.Name}{cidStr}");

    foreach (var param in ctor.GetParams())
        Console.WriteLine($"    {param.Name}: {param.Type}");
}

Console.WriteLine();

// Iterate functions
Console.WriteLine("=== Functions ===");
foreach (var func in result.GetFunctions())
{
    var ns = func.Id.GetNamespace();
    var prefix = ns != null ? $"{ns}." : "";
    Console.WriteLine($"  {prefix}{func.Id.Name} -> {func.ResultType}");
}

// TryParse example
Console.WriteLine();
if (TlParser.TryParse("invalid schema {{{", out _))
    Console.WriteLine("Unexpected success");
else
    Console.WriteLine("TryParse correctly returned false for invalid input");
