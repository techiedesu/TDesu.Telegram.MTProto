namespace TDesu.Telegram.TL.AST

type TlIdentifier = { Namespace: string option; Name: string }

type TlConstructorId = TlConstructorId of uint32

[<RequireQualifiedAccess>]
type TlTypeExpr =
    | Bare of TlIdentifier
    | Boxed of TlIdentifier
    | TypeVar of string
    | Vector of TlTypeExpr
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
