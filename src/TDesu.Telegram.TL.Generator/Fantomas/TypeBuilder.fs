module TDesu.Telegram.TL.Generator.Fantomas.TypeBuilder

open Fantomas.Core
open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Text
open Fantomas.FCS.Xml
open TDesu.Telegram.TL.Generator.Fantomas.Extensions

let r = Range.Zero

// --- Type helpers ---

let mkSynType (name: string) = SynLongIdent.CreateSingleIdent name

let mkSynTypeApp (name: string) (args: SynType list) =
    SynType.App(
        typeName = SynType.LongIdent(SynLongIdent.Create([ Ident.Create name ])),
        typeArgs = args,
        commaRanges = List.replicate (args.Length - 1) r,
        isPostfix = false,
        lessRange = Some r,
        greaterRange = Some r,
        range = r
    )

let mkSynTypeArray (elementType: SynType) =
    SynType.Array(rank = 1, elementType = elementType, range = r)

// --- Record type ---

let mkRecordField (name: string) (fieldType: SynType) =
    SynField.Create(fieldType = fieldType, idOpt = Ident.Create name)

let mkRecordType
    (name: string)
    (fields: SynField list)
    (members: SynMemberDefn list)
    (keyword: SynTypeDefnLeadingKeyword)
    (attributes: SynAttributeList list)
    =
    SynTypeDefn.Create(
        typeInfo =
            SynComponentInfo.Create(longId = [ Ident.Create name ], attributes = attributes),
        typeRepr =
            SynTypeDefnRepr.CreateSimple(
                SynTypeDefnSimpleRepr.Record(accessibility = None, recordFields = fields, range = r)
            ),
        members = members,
        trivia =
            { SynTypeDefnTrivia.LeadingKeyword = keyword
              SynTypeDefnTrivia.EqualsRange = Some r
              SynTypeDefnTrivia.WithKeyword =
                  if members.IsEmpty then
                      None
                  else
                      Some r }
    )

// --- Union type ---

let mkUnionCase (caseName: string) (fields: SynField list) =
    SynUnionCase.SynUnionCase(
        attributes = [],
        ident = SynIdent.Create(Ident.Create caseName),
        caseType = SynUnionCaseKind.Fields fields,
        xmlDoc = PreXmlDoc.Empty,
        accessibility = None,
        range = r,
        trivia = { BarRange = Some r }
    )

let mkUnionType
    (name: string)
    (cases: SynUnionCase list)
    (members: SynMemberDefn list)
    (keyword: SynTypeDefnLeadingKeyword)
    (attributes: SynAttributeList list)
    =
    SynTypeDefn.Create(
        typeInfo =
            SynComponentInfo.Create(longId = [ Ident.Create name ], attributes = attributes),
        typeRepr =
            SynTypeDefnRepr.CreateSimple(
                SynTypeDefnSimpleRepr.Union(accessibility = None, unionCases = cases, range = r)
            ),
        members = members,
        trivia =
            { SynTypeDefnTrivia.LeadingKeyword = keyword
              SynTypeDefnTrivia.EqualsRange = Some r
              SynTypeDefnTrivia.WithKeyword =
                  if members.IsEmpty then
                      None
                  else
                      Some r }
    )

// --- Members ---

let private mkMemberFlags () =
    { SynMemberFlags.IsInstance = false
      SynMemberFlags.IsDispatchSlot = false
      SynMemberFlags.IsOverrideOrExplicitImpl = false
      SynMemberFlags.IsFinal = false
      SynMemberFlags.GetterOrSetterIsCompilerGenerated = false
      SynMemberFlags.MemberKind = SynMemberKind.Member }

let private staticMemberTrivia equalsRange inlineKeyword =
    SynBindingTriviaHelper.create (
        SynLeadingKeyword.StaticMember(staticRange = r, memberRange = r),
        equalsRange,
        inlineKeyword
    )

let mkStaticMemberWithReturnType
    (name: string)
    (pats: SynPat list)
    (retType: SynType option)
    (body: SynExpr)
    =
    let headPat =
        SynPat.CreateLongIdent(
            longDotId = SynLongIdent.Create([ Ident.Create name ]),
            argPats = SynArgPats.Pats(pats |> List.map SynPat.CreateParen)
        )

    let returnInfo =
        retType
        |> Option.map (fun t ->
            SynBindingReturnInfo.SynBindingReturnInfo(
                typeName = t,
                range = r,
                attributes = [],
                trivia = { ColonRange = Some r }
            ))

    SynMemberDefn.CreateMember(
        SynBinding.SynBinding(
            kind = SynBindingKind.Normal,
            valData = SynValData.Create(memberFlags = mkMemberFlags ()),
            headPat = headPat,
            expr = body,
            trivia = staticMemberTrivia (Some r) None,
            accessibility = None,
            isInline = false,
            isMutable = false,
            attributes = [],
            xmlDoc = PreXmlDoc.Empty,
            returnInfo = returnInfo,
            range = r,
            debugPoint = DebugPointAtBinding.NoneAtInvisible
        )
    )

let mkStaticMember (name: string) (pats: SynPat list) (body: SynExpr) =
    let headPat =
        SynPat.CreateLongIdent(
            longDotId = SynLongIdent.Create([ Ident.Create name ]),
            argPats = SynArgPats.Pats(pats |> List.map SynPat.CreateParen)
        )

    SynMemberDefn.CreateMember(
        SynBinding.Create(
            kind = SynBindingKind.Normal,
            valData = SynValData.Create(memberFlags = mkMemberFlags ()),
            headPat = headPat,
            expr = body,
            trivia = staticMemberTrivia (Some r) None
        )
    )

let mkStaticProperty (name: string) (retType: SynType option) (body: SynExpr) =
    let headPat =
        SynPat.CreateLongIdent(
            longDotId = SynLongIdent.Create([ Ident.Create name ]),
            argPats = SynArgPats.Pats []
        )

    let returnInfo =
        retType
        |> Option.map (fun t ->
            SynBindingReturnInfo.SynBindingReturnInfo(
                typeName = t,
                range = r,
                attributes = [],
                trivia = { ColonRange = Some r }
            ))

    SynMemberDefn.CreateMember(
        SynBinding.SynBinding(
            kind = SynBindingKind.Normal,
            valData = SynValData.Create(memberFlags = mkMemberFlags ()),
            headPat = headPat,
            expr = body,
            trivia = staticMemberTrivia (Some r) None,
            accessibility = None,
            isInline = false,
            isMutable = false,
            attributes = [],
            xmlDoc = PreXmlDoc.Empty,
            returnInfo = returnInfo,
            range = r,
            debugPoint = DebugPointAtBinding.NoneAtInvisible
        )
    )

let mkInlineStaticMember (name: string) (pats: SynPat list) (body: SynExpr) =
    let headPat =
        SynPat.CreateLongIdent(
            longDotId = SynLongIdent.Create([ Ident.Create name ]),
            argPats = SynArgPats.Pats(pats |> List.map SynPat.CreateParen)
        )

    SynMemberDefn.CreateMember(
        SynBinding.Create(
            kind = SynBindingKind.Normal,
            valData = SynValData.Create(memberFlags = mkMemberFlags ()),
            headPat = headPat,
            expr = body,
            trivia = staticMemberTrivia (Some r) (Some r),
            isInline = true
        )
    )

// --- Module-level bindings ---

let mkLetBinding
    (name: string)
    (pats: SynPat list)
    (body: SynExpr)
    (keyword: SynLeadingKeyword)
    =
    SynBinding.Create(
        kind = SynBindingKind.Normal,
        valData = SynValData.Create(),
        headPat =
            SynPat.CreateLongIdent(
                longDotId = SynLongIdent.Create([ Ident.Create name ]),
                argPats = SynArgPats.Pats(pats |> List.map SynPat.CreateParen)
            ),
        expr = body,
        trivia = SynBindingTriviaHelper.create (keyword, Some r, None)
    )

let mkLiteralBinding (name: string) (body: SynExpr) =
    let attr =
        { SynAttributeList.Attributes =
              [ { SynAttribute.TypeName = SynLongIdent.Create([ Ident.Create "Literal" ])
                  SynAttribute.ArgExpr = SynExpr.Const(SynConst.Unit, r)
                  SynAttribute.Target = None
                  SynAttribute.AppliesToGetterAndSetter = false
                  SynAttribute.Range = r } ]
          SynAttributeList.Range = r }

    SynBinding.Create(
        kind = SynBindingKind.Normal,
        valData = SynValData.Create(),
        headPat = SynPat.CreateNamed(SynIdent.Create(Ident.Create name)),
        expr = body,
        trivia = SynBindingTriviaHelper.create (SynLeadingKeyword.Let(r), Some r, None),
        attributes = [ attr ]
    )

// --- Attributes ---

let mkAttribute (name: string) =
    { SynAttributeList.Attributes =
          [ { SynAttribute.TypeName = SynLongIdent.Create([ Ident.Create name ])
              SynAttribute.ArgExpr = SynExpr.Const(SynConst.Unit, r)
              SynAttribute.Target = None
              SynAttribute.AppliesToGetterAndSetter = false
              SynAttribute.Range = r } ]
      SynAttributeList.Range = r }

let mkRequireQualifiedAccessAttr () = mkAttribute "RequireQualifiedAccess"

// --- Namespace / Module / ParsedInput ---

let mkOpenDecl (ns: string) =
    let idents = ns.Split('.') |> Array.toList |> List.map Ident.Create

    SynModuleDecl.Open(
        target = SynOpenDeclTarget.ModuleOrNamespace(longId = SynLongIdent.Create(idents), range = r),
        range = r
    )

let mkNamespace (ns: string) (decls: SynModuleDecl list) =
    let idents = ns.Split('.') |> Array.toList |> List.map Ident.Create

    SynModuleOrNamespace.SynModuleOrNamespace(
        longId = idents,
        isRecursive = false,
        kind = SynModuleOrNamespaceKind.DeclaredNamespace,
        decls = decls,
        xmlDoc = PreXmlDoc.Empty,
        attribs = [],
        accessibility = None,
        range = r,
        trivia =
            SynModuleOrNamespaceTriviaHelper.create (SynModuleOrNamespaceLeadingKeyword.Namespace r)
    )

let mkModule (name: string) (decls: SynModuleDecl list) (attributes: SynAttributeList list) =
    SynModuleOrNamespace.SynModuleOrNamespace(
        longId = [ Ident.Create name ],
        isRecursive = false,
        kind = SynModuleOrNamespaceKind.NamedModule,
        decls = decls,
        xmlDoc = PreXmlDoc.Empty,
        attribs = attributes,
        accessibility = None,
        range = r,
        trivia =
            SynModuleOrNamespaceTriviaHelper.create (SynModuleOrNamespaceLeadingKeyword.Module r)
    )

let mkParsedInput (contents: SynModuleOrNamespace list) =
    ParsedInput.ImplFile(
        ParsedImplFileInput.ParsedImplFileInput(
            fileName = "generated.fs",
            isScript = false,
            qualifiedNameOfFile =
                QualifiedNameOfFile.QualifiedNameOfFile(Ident.Create "Generated"),
            scopedPragmas = [],
            hashDirectives = [],
            contents = contents,
            flags = (false, false),
            trivia =
                { ConditionalDirectives = []
                  CodeComments = [] },
            identifiers = set []
        )
    )

let formatAst (parsedInput: ParsedInput) =
    let config =
        { FormatConfig.Default with
            ExperimentalElmish = true
            MultilineBracketStyle = MultilineBracketStyle.Stroustrup
            SpaceBeforeParameter = true
            MaxLineLength = 120 }

    CodeFormatter.FormatASTAsync(parsedInput, config)
