module TDesu.Telegram.TL.Generator.Fantomas.Extensions

open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Text
open Fantomas.FCS.Xml

module internal Defaults =
    let inline def v = Option.defaultValue v
    let inline defWith f = Option.defaultWith f
    let inline defList v = Option.defaultWith (fun () -> []) v
    let defBool v = Option.defaultValue false v
    let defRange v = Option.defaultValue Range.Zero v

open Defaults

type SynPat with

    static member CreateNamed(ident, ?isThisVal, ?accessibility, ?range) =
        SynPat.Named(
            ident = ident,
            isThisVal = defBool isThisVal,
            accessibility = accessibility,
            range = defRange range
        )

    static member CreateLongIdent(longDotId, argPats, ?extraId, ?typarDecls, ?accessibility, ?range) =
        SynPat.LongIdent(
            longDotId = longDotId,
            argPats = argPats,
            extraId = extraId,
            typarDecls = typarDecls,
            accessibility = accessibility,
            range = defRange range
        )

    static member CreateParen(pat, ?range) =
        SynPat.Paren(pat = pat, range = defRange range)

    static member CreateTuple(elementPats, ?isStruct, ?commaRanges, ?range) =
        SynPat.Tuple(
            elementPats = elementPats,
            isStruct = defBool isStruct,
            commaRanges =
                defWith (fun () -> List.replicate (elementPats.Length - 1) Range.Zero) commaRanges,
            range = defRange range
        )

type SynField with

    static member Create
        (
            fieldType,
            ?attributes,
            ?isStatic,
            ?idOpt,
            ?isMutable,
            ?xmlDoc,
            ?accessibility,
            ?range,
            ?trivia
        ) =
        SynField.SynField(
            fieldType = fieldType,
            attributes = defList attributes,
            isStatic = defBool isStatic,
            idOpt = idOpt,
            isMutable = defBool isMutable,
            xmlDoc = def PreXmlDoc.Empty xmlDoc,
            accessibility = accessibility,
            range = defRange range,
            trivia =
                def
                    { SynFieldTrivia.LeadingKeyword = None
                      SynFieldTrivia.MutableKeyword = None }
                    trivia
        )

type Ident with

    static member Create(text, ?range) =
        Ident(text, range = defRange range)

type SynIdent with

    static member Create(ident, ?trivia) =
        SynIdent.SynIdent(ident = ident, trivia = trivia)

type SynLongIdent with

    static member Create(id, ?dotRanges, ?trivia) =
        SynLongIdent.SynLongIdent(
            id = id,
            dotRanges =
                defWith (fun () -> List.replicate (List.length id - 1) Range.Zero) dotRanges,
            trivia = defList trivia
        )

    static member CreateSingleIdent(typeName, ?range) =
        SynType.LongIdent(
            SynLongIdent.Create([ Ident.Create(text = typeName, range = defRange range) ])
        )

type SynArgInfo with

    static member Create(?attributes, ?optional, ?ident) =
        SynArgInfo.SynArgInfo(
            attributes = defList attributes,
            optional = defBool optional,
            ident = ident
        )

type SynComponentInfo with

    static member Create
        (
            ?attributes,
            ?typeParams,
            ?constraints,
            ?longId,
            ?xmlDoc,
            ?preferPostfix,
            ?accessibility,
            ?range
        ) =
        SynComponentInfo.SynComponentInfo(
            attributes = defList attributes,
            typeParams = typeParams,
            constraints = defList constraints,
            longId = defList longId,
            xmlDoc = def PreXmlDoc.Empty xmlDoc,
            preferPostfix = defBool preferPostfix,
            accessibility = accessibility,
            range = defRange range
        )

type SynExprRecordField with

    static member Create(fieldName, ?equalsRange, ?expr, ?blockSeparator) =
        SynExprRecordField.SynExprRecordField(fieldName, equalsRange, expr, blockSeparator)

type SynMemberDefn with

    static member CreateMember(memberDefn, ?range) =
        SynMemberDefn.Member(memberDefn = memberDefn, range = defRange range)

type SynValInfo with

    static member Create(?curriedArgInfos, ?returnInfo) =
        SynValInfo.SynValInfo(
            curriedArgInfos = defList curriedArgInfos,
            returnInfo = defWith SynArgInfo.Create returnInfo
        )

type SynTypeDefn with

    static member Create(typeInfo, typeRepr, members, trivia, ?implicitConstructor, ?range) =
        SynTypeDefn.SynTypeDefn(
            typeInfo = typeInfo,
            typeRepr = typeRepr,
            members = members,
            trivia = trivia,
            implicitConstructor = implicitConstructor,
            range = defRange range
        )

type SynTypeDefnRepr with

    static member CreateSimple(simpleRepr, ?range) =
        SynTypeDefnRepr.Simple(simpleRepr = simpleRepr, range = defRange range)

type SynValData with

    static member Create(?valInfo, ?memberFlags, ?thisIdOpt) =
        SynValData.SynValData(
            valInfo = defWith SynValInfo.Create valInfo,
            memberFlags = memberFlags,
            thisIdOpt = thisIdOpt
        )

type SynBinding with

    static member Create
        (
            kind,
            valData,
            headPat,
            expr,
            trivia,
            ?accessibility,
            ?isInline,
            ?isMutable,
            ?attributes,
            ?xmlDoc,
            ?returnInfo,
            ?range,
            ?debugPoint
        ) =
        SynBinding.SynBinding(
            kind = kind,
            valData = valData,
            headPat = headPat,
            expr = expr,
            trivia = trivia,
            accessibility = accessibility,
            isInline = defBool isInline,
            isMutable = defBool isMutable,
            attributes = defList attributes,
            xmlDoc = def PreXmlDoc.Empty xmlDoc,
            returnInfo = returnInfo,
            range = defRange range,
            debugPoint = def DebugPointAtBinding.NoneAtInvisible debugPoint
        )

// --- Helper functions for records and type aliases ---

module SynBindingTriviaHelper =

    let create
        (leadingKeyword: SynLeadingKeyword, equalsRange: range option, inlineKeyword: range option)
        =
        { SynBindingTrivia.LeadingKeyword = leadingKeyword
          SynBindingTrivia.EqualsRange = equalsRange
          SynBindingTrivia.InlineKeyword = inlineKeyword }

module SynModuleOrNamespaceTriviaHelper =

    let create (leadingKeyword: SynModuleOrNamespaceLeadingKeyword) =
        { SynModuleOrNamespaceTrivia.LeadingKeyword = leadingKeyword }

module RecordFieldNameHelper =

    let create (id: Ident list) =
        RecordFieldName(SynLongIdent.Create(id = id), false)
