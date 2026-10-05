namespace TDesu.Telegram.TL.Generator

open TDesu.Telegram.TL.Generator.Overrides

/// Plans semantic object mappings; EmitCSharp reuses its normal field codecs
/// for each archived layout. There is no second binary reader or writer.
module CSharpLayers =
    type Declaration = {
        Name: string
        ResultType: string
        Cid: uint32
        Fields: GeneratedField list
        IsFunction: bool
    }

    type Binding = {
        Source: GeneratedField
        Target: GeneratedField
        Nested: bool
    }

    type Variant = {
        Layer: int
        Source: Declaration
        Target: Declaration
        Nested: (GeneratedField * Declaration) option
        Bindings: Binding list
        ReadFields: GeneratedField list
        Write: bool
    }

    let private declarations (types: GeneratedType list) (functions: GeneratedFunction list) =
        [ for t in types do
              match t with
              | Record(name, fields, cid) ->
                  yield { Name = name; ResultType = name; Cid = cid; Fields = fields; IsFunction = false }
              | Union(name, cases) ->
                  for c in cases do
                      yield { Name = c.Name; ResultType = name; Cid = c.ConstructorId; Fields = c.Fields; IsFunction = false }
          for f in functions ->
              { Name = f.Name; ResultType = f.ReturnType; Cid = f.ConstructorId; Fields = f.Params; IsFunction = true } ]

    let private key (f: GeneratedField) = f.RecordName.Replace("`", "")

    // The IR exposes a flags:# register with no dependent fields as an int.
    let private emptyRegister (f: GeneratedField) =
        f.FSharpType = "int32" && (f.Name = "flags" || f.Name = "flags2")

    let private optionalOrMarker f = f.FlagField.IsSome || emptyRegister f

    let plan layer (mappings: CSharpLayerMapping list) oldTypes oldFunctions types functions =
        if layer <= 0 then invalidArg "layer" "A historical schema must declare a positive layer"
        let old = declarations oldTypes oldFunctions |> List.map (fun d -> d.Name, d) |> Map.ofList
        let current = declarations types functions |> List.map (fun d -> d.Name, d) |> Map.ofList
        let get kind name map =
            match Map.tryFind name map with
            | Some d -> d
            | None -> failwithf "C# layer mapping: %s declaration '%s' does not exist" kind name
        let duplicates = mappings |> List.countBy (fun m -> m.Source) |> List.filter (fun (_, n) -> n <> 1)
        if not duplicates.IsEmpty then failwithf "Duplicate C# layer mappings: %A" duplicates
        let explicitSources = mappings |> List.map (fun m -> m.Source) |> Set.ofList
        let renamedTypes =
            mappings
            |> List.map (fun m -> (get "source" m.Source old).ResultType, (get "target" m.Target current).ResultType)
            |> Set.ofList

        let rec adaptType source target =
            let s, t = IrType.unoption source, IrType.unoption target
            let result =
                if s = t then s
                elif IrType.isVector s && IrType.isVector t && IrType.isBareVector s = IrType.isBareVector t then
                    IrType.vectorOf (IrType.isBareVector s) (adaptType (IrType.element s) (IrType.element t))
                elif renamedTypes.Contains(s, t) then t
                else failwithf "C# layer mapping changes wire type %s to %s without a declaration mapping" s t
            if IrType.isOption source then result + IrType.OptionSuffix else result

        let make (source: Declaration) (target: Declaration) nested write =
            let rootFields = FieldHelpers.dataFields target.Fields
            let nestedFields = nested |> Option.map (snd >> fun d -> FieldHelpers.dataFields d.Fields) |> Option.defaultValue []
            let bindings =
                FieldHelpers.dataFields source.Fields
                |> List.map (fun f ->
                    let matches =
                        [ for t in rootFields do
                              if key t = key f then yield t, false
                          for t in nestedFields do
                              if key t = key f then yield t, true ]
                    match matches with
                    | [ t, isNested ] -> { Source = f; Target = t; Nested = isNested }
                    | _ -> failwithf "C# layer mapping %s -> %s: field %s must map to exactly one target field" source.Name target.Name f.RecordName)
            let checkRequired isNested fields =
                for f in fields do
                    let isContainer = not isNested && (nested |> Option.exists (fun (n, _) -> key n = key f))
                    let mapped = bindings |> List.exists (fun b -> b.Nested = isNested && key b.Target = key f)
                    if not mapped && not isContainer && not (optionalOrMarker f) then
                        failwithf "C# layer mapping %s -> %s cannot supply required field %s" source.Name target.Name f.RecordName
            checkRequired false rootFields
            checkRequired true nestedFields
            let readFields =
                source.Fields |> List.map (fun f ->
                    match bindings |> List.tryFind (fun b -> b.Source = f) with
                    | None -> f
                    | Some b -> { f with FSharpType = adaptType f.FSharpType b.Target.FSharpType })
            { Layer = layer; Source = source; Target = target; Nested = nested
              Bindings = bindings; ReadFields = readFields; Write = write }

        let explicit =
            [ for mapping in mappings do
                  let source = get "source" mapping.Source old
                  let target = get "target" mapping.Target current
                  let nested =
                      match mapping.NestedField, mapping.NestedConstructor with
                      | None, None -> None
                      | Some field, Some name ->
                          let memberField =
                              target.Fields |> List.tryFind (fun f -> key f = field)
                              |> Option.defaultWith (fun () -> failwithf "%s has no field %s" target.Name field)
                          let constructor = get "nested" name current
                          if IrType.unoption memberField.FSharpType <> constructor.ResultType then
                              failwithf "%s.%s does not accept %s" target.Name field name
                          Some(memberField, constructor)
                      | _ -> failwith "Nested field and constructor must be supplied together"
                  yield make source target nested true ]
        let automatic =
            [ for KeyValue(name, source) in old do
                  if not (explicitSources.Contains name) then
                      match Map.tryFind name current with
                      | Some target when target.Cid <> source.Cid ->
                          yield make source target None false
                      | _ -> () ]
        explicit @ automatic |> List.sortBy (fun v -> v.Target.Name, v.Source.Cid)
