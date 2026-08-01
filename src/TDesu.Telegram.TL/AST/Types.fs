namespace TDesu.Telegram.TL.AST

type TlIdentifier = { Namespace: string option; Name: string }

type TlConstructorId = TlConstructorId of uint32

[<RequireQualifiedAccess>]
type TlTypeExpr =
    /// A lowercase type reference: the constructor's fields with NO
    /// constructor id in front of them.
    | Bare of TlIdentifier
    /// An uppercase type reference: a constructor id followed by its fields.
    | Boxed of TlIdentifier
    | TypeVar of string
    /// `Vector<T>` (`isBare = false`) writes `0x1CB5C415`, a count and the
    /// elements; `vector<T>` (`isBare = true`) writes the count and the
    /// elements only. TL spells the difference with one capital letter and
    /// means two different wire formats by it, so the node has to carry it —
    /// folding the two together put a vector header inside `future_salts`
    /// that a real client reads as its element count (#117).
    | Vector of isBare: bool * inner: TlTypeExpr
    | Nat
    | Conditional of fieldRef: string * bitIndex: int * innerType: TlTypeExpr

type TlParam = { Name: string; Type: TlTypeExpr }

type TlCombinator = {
    Id: TlIdentifier
    ConstructorId: TlConstructorId option
    TypeParams: string list
    Params: TlParam list
    ResultType: TlTypeExpr
}

type TlSchema = {
    Constructors: TlCombinator list
    Functions: TlCombinator list
    Layer: int option
}
