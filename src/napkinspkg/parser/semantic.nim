import ./types

type
  SemanticPass* = object
    nodes*: seq[AstNode]
    registeredTypes*: seq[RegisteredType]

proc initSemanticPass*(nodes: seq[AstNode]): SemanticPass = SemanticPass(nodes: nodes, registeredTypes: @NapkinTypes)

proc processEnum(sp: var SemanticPass, n: AstNode): AstNode =
  let
    name = n.eName.strVal
    typ = n.eType.strVal

  if typ notin ["VInt32", "VInt64", "UInt8", "UInt16", "SInt8", "SInt16", "SInt32", "SInt64"]:
    quit "Invalid enum type: " & typ

  sp.registeredTypes.add registerType(name)

  return n

proc validateFieldType(sp: var SemanticPass, n: AstNode): bool =
  case n.kind
    of Identifier:
      if n.strVal notin sp.registeredTypes:
        quit "Unknown type: " & n.strVal

      if sp.registeredTypes[n.strVal].isGeneric:
        quit "Type is generic: " & n.strVal

    of Index:
      let typName = n.itarget.strVal
      if typName notin sp.registeredTypes:
        quit "Unknown type: " & typName

      let typ = sp.registeredTypes[typName]

      if not typ.isGeneric:
        quit "Type is not generic: " & typName

      if n.iargs.len != typ.params.len:
        quit "Invalid number of arguments for type: " & typName

      for x in 0..<n.iargs.len:
        if n.iargs[x].kind notin typ.params[x]:
          quit "Invalid type for index: " & $n.iargs[x]

    else:
      quit "Invalid AST!"

  return true

proc processStruct(sp: var SemanticPass, n: AstNode): AstNode =
  result = n
  let name = result.sName.strVal
  var
    types = newSeq[set[AstKind]]()
    constructorNames = newSeq[string]()
    fieldNames = newSeq[string]()

  for i in result.sArgs:
    types.add {Identifier}



    if i[0].strVal in constructorNames:
      quit "Duplicate constructor name: " & i[0].strVal

    constructorNames.add i[0].strVal

  sp.registeredTypes.add registerType(name, params=types)

proc process*(sp: var SemanticPass): seq[AstNode] =
  for n in sp.nodes:
    result.add case n.kind
      of EnumTypeDef:
        sp.processEnum(n)

      of StructDef:
        sp.processStruct(n)

      of PacketDef:
        echo "Unimplemented!"
        quit()

      else:
        echo "Invalid node in this context"
        quit()

    break