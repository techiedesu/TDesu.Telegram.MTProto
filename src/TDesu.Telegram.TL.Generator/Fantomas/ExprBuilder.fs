module TDesu.Telegram.TL.Generator.Fantomas.ExprBuilder

open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Text
open TDesu.Telegram.TL.Generator.Fantomas.Extensions

let r = Range.Zero

// --- Identifiers ---

let mkIdent (name: string) =
    SynExpr.Ident(Ident.Create name)

let mkLongIdent (parts: string list) =
    SynExpr.LongIdent(
        isOptional = false,
        longDotId = SynLongIdent.Create(parts |> List.map Ident.Create),
        altNameRefCell = None,
        range = r
    )

// --- Application ---

let mkApp (fn: SynExpr) (arg: SynExpr) =
    SynExpr.App(
        flag = ExprAtomicFlag.NonAtomic,
        isInfix = false,
        funcExpr = fn,
        argExpr = arg,
        range = r
    )

let mkInfixApp (op: string) (lhs: SynExpr) (rhs: SynExpr) =
    SynExpr.App(
        flag = ExprAtomicFlag.NonAtomic,
        isInfix = false,
        funcExpr =
            SynExpr.App(
                flag = ExprAtomicFlag.NonAtomic,
                isInfix = true,
                funcExpr =
                    SynExpr.LongIdent(
                        isOptional = false,
                        longDotId =
                            SynLongIdent.SynLongIdent(
                                id = [ Ident.Create $"op_{op}" ],
                                dotRanges = [],
                                trivia = [ Some(IdentTrivia.OriginalNotation op) ]
                            ),
                        altNameRefCell = None,
                        range = r
                    ),
                argExpr = lhs,
                range = r
            ),
        argExpr = rhs,
        range = r
    )

let mkDotGet (expr: SynExpr) (ident: string) =
    SynExpr.DotGet(
        expr = expr,
        rangeOfDot = r,
        longDotId = SynLongIdent.Create([ Ident.Create ident ]),
        range = r
    )

// --- Constants ---

let mkConst (c: SynConst) = SynExpr.Const(c, r)

let mkUnit = mkConst SynConst.Unit

let mkInt32 (v: int) = mkConst (SynConst.Int32 v)

let mkUInt32 (v: uint32) = mkConst (SynConst.UInt32 v)

let mkInt64 (v: int64) = mkConst (SynConst.Int64 v)

let mkString (v: string) = mkConst (SynConst.String(v, SynStringKind.Regular, r))

let mkBool (v: bool) =
    if v then
        mkLongIdent [ "true" ]
    else
        mkLongIdent [ "false" ]

// --- Paren ---

let mkParen (expr: SynExpr) = SynExpr.Paren(expr, r, Some r, r)

// --- Tuple ---

let mkTuple (exprs: SynExpr list) =
    SynExpr.Tuple(
        isStruct = false,
        exprs = exprs,
        commaRanges = List.replicate (exprs.Length - 1) r,
        range = r
    )

// --- Let ---

let mkLet (name: string) (rhs: SynExpr) (body: SynExpr) =
    SynExpr.LetOrUse(
        isRecursive = false,
        isUse = false,
        bindings =
            [ SynBinding.Create(
                  kind = SynBindingKind.Normal,
                  valData = SynValData.Create(),
                  headPat = SynPat.CreateNamed(SynIdent.Create(Ident.Create name)),
                  expr = rhs,
                  trivia = SynBindingTriviaHelper.create (SynLeadingKeyword.Let(r), Some r, None)
              ) ],
        body = body,
        range = r,
        trivia = { SynExprLetOrUseTrivia.InKeyword = None }
    )

let mkLetMutable (name: string) (rhs: SynExpr) (body: SynExpr) =
    SynExpr.LetOrUse(
        isRecursive = false,
        isUse = false,
        bindings =
            [ SynBinding.Create(
                  kind = SynBindingKind.Normal,
                  valData = SynValData.Create(),
                  headPat = SynPat.CreateNamed(SynIdent.Create(Ident.Create name)),
                  expr = rhs,
                  trivia = SynBindingTriviaHelper.create (SynLeadingKeyword.Let(r), Some r, None),
                  isMutable = true
              ) ],
        body = body,
        range = r,
        trivia = { SynExprLetOrUseTrivia.InKeyword = None }
    )

let mkUse (name: string) (rhs: SynExpr) (body: SynExpr) =
    SynExpr.LetOrUse(
        isRecursive = false,
        isUse = true,
        bindings =
            [ SynBinding.Create(
                  kind = SynBindingKind.Normal,
                  valData = SynValData.Create(),
                  headPat = SynPat.CreateNamed(SynIdent.Create(Ident.Create name)),
                  expr = rhs,
                  trivia = SynBindingTriviaHelper.create (SynLeadingKeyword.Use(r), Some r, None)
              ) ],
        body = body,
        range = r,
        trivia = { SynExprLetOrUseTrivia.InKeyword = None }
    )

// --- Sequential ---

let mkSeq (exprs: SynExpr list) =
    match exprs with
    | [] -> mkUnit
    | [ single ] -> single
    | first :: rest ->
        rest
        |> List.fold
            (fun acc next ->
                SynExpr.Sequential(
                    debugPoint = DebugPointAtSequential.SuppressBoth,
                    isTrueSeq = true,
                    expr1 = acc,
                    expr2 = next,
                    range = r,
                    trivia = { SynExprSequentialTrivia.SeparatorRange = None }
                ))
            first

// --- Match ---

let mkMatchClause (pat: SynPat) (body: SynExpr) =
    SynMatchClause.SynMatchClause(
        pat = pat,
        whenExpr = None,
        resultExpr = body,
        range = r,
        debugPoint = DebugPointAtTarget.No,
        trivia = { BarRange = Some r; ArrowRange = Some r }
    )

let mkMatch (expr: SynExpr) (clauses: SynMatchClause list) =
    SynExpr.Match(
        matchDebugPoint = DebugPointAtBinding.NoneAtInvisible,
        expr = expr,
        clauses = clauses,
        range = r,
        trivia = { MatchKeyword = r; WithKeyword = r }
    )

// --- If/Then/Else ---

let mkIf (cond: SynExpr) (thenE: SynExpr) (elseE: SynExpr option) =
    SynExpr.IfThenElse(
        ifExpr = cond,
        thenExpr = thenE,
        elseExpr = elseE,
        spIfToThen = DebugPointAtBinding.NoneAtInvisible,
        isFromErrorRecovery = false,
        range = r,
        trivia =
            { IfKeyword = r
              IsElif = false
              ThenKeyword = r
              ElseKeyword = elseE |> Option.map (fun _ -> r)
              IfToThenRange = r }
    )

// --- Record ---

let mkRecordExpr (fields: (string list * SynExpr) list) =
    SynExpr.Record(
        baseInfo = None,
        copyInfo = None,
        recordFields =
            [ for names, expr in fields do
                  SynExprRecordField.Create(
                      fieldName = RecordFieldNameHelper.create (names |> List.map Ident.Create),
                      equalsRange = r,
                      expr = expr,
                      blockSeparator = BlockSeparator(r, None)
                  ) ],
        range = r
    )

// --- Array ---

let mkArrayExpr (items: SynExpr list) =
    SynExpr.ArrayOrListComputed(isArray = true, expr = mkSeq items, range = r)

// --- Set ---

let mkSet (target: string) (value: SynExpr) =
    SynExpr.LongIdentSet(
        longDotId = SynLongIdent.Create([ Ident.Create target ]),
        expr = value,
        range = r
    )

let mkDotSet (expr: SynExpr) (ident: string) (value: SynExpr) =
    SynExpr.DotSet(
        targetExpr = expr,
        longDotId = SynLongIdent.Create([ Ident.Create ident ]),
        rhsExpr = value,
        range = r
    )

// --- For ---

let mkForEach (name: string) (enumExpr: SynExpr) (body: SynExpr) =
    SynExpr.ForEach(
        forDebugPoint = DebugPointAtFor.No,
        inDebugPoint = DebugPointAtInOrTo.No,
        seqExprOnly = SeqExprOnly false,
        isFromSource = true,
        pat = SynPat.CreateNamed(SynIdent.Create(Ident.Create name)),
        enumExpr = enumExpr,
        bodyExpr = body,
        range = r
    )

// --- Patterns ---

let mkPatNamed (name: string) =
    SynPat.CreateNamed(SynIdent.Create(Ident.Create name))

let mkPatLongIdent (parts: string list) (args: SynPat list) =
    SynPat.CreateLongIdent(
        longDotId = SynLongIdent.Create(parts |> List.map Ident.Create),
        argPats =
            if args.IsEmpty then
                SynArgPats.Pats []
            else
                SynArgPats.Pats [ SynPat.CreateParen(SynPat.CreateTuple args) ]
    )

let mkPatLongIdentSimple (parts: string list) =
    SynPat.CreateLongIdent(
        longDotId = SynLongIdent.Create(parts |> List.map Ident.Create),
        argPats = SynArgPats.Pats []
    )

let mkPatWild = SynPat.Wild r

let mkPatConst (c: SynConst) = SynPat.Const(c, r)

let mkPatOr (pats: SynPat list) =
    match pats with
    | [] -> failwith "mkPatOr requires at least one pattern"
    | [ single ] -> single
    | first :: rest ->
        rest
        |> List.fold
            (fun acc next -> SynPat.Or(lhsPat = acc, rhsPat = next, range = r, trivia = { BarRange = r }))
            first

// --- Type annotations ---

let mkTyped (expr: SynExpr) (typeName: string) =
    SynExpr.Typed(
        expr = expr,
        targetType = SynLongIdent.CreateSingleIdent typeName,
        range = r
    )

// --- Typed patterns ---

let mkPatTyped (pat: SynPat) (synType: SynType) =
    SynPat.Typed(pat = pat, targetType = synType, range = r)

// --- Lambda ---

let mkLambda (paramNames: string list) (body: SynExpr) =
    let pats =
        paramNames
        |> List.map (fun name ->
            SynSimplePat.Id(
                ident = Ident.Create name,
                altNameRefCell = None,
                isCompilerGenerated = false,
                isThisVal = false,
                isOptional = false,
                range = r
            ))

    let synPats =
        paramNames |> List.map (fun name -> mkPatNamed name)

    SynExpr.Lambda(
        fromMethod = false,
        inLambdaSeq = false,
        args = SynSimplePats.SimplePats(pats = pats, commaRanges = [], range = r),
        body = body,
        parsedData = Some(synPats, body),
        range = r,
        trivia = { SynExprLambdaTrivia.ArrowRange = Some r }
    )

// --- New ---

let mkNew (typeName: SynType) (args: SynExpr) =
    SynExpr.New(isProtected = false, targetType = typeName, expr = args, range = r)

// --- Interpolated string ---

let mkInterpolatedString (parts: SynInterpolatedStringPart list) =
    SynExpr.InterpolatedString(contents = parts, synStringKind = SynStringKind.Regular, range = r)
