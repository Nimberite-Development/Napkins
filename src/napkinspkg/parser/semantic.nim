import std/sugar

import ./types

# TODO: Error reporting function

type
  SemanticPass* = object
    nodes*: seq[AstNode]
    registeredTypes*: seq[RegisteredType]

proc initSemanticPass*(nodes: seq[AstNode]): SemanticPass = SemanticPass(nodes: nodes, registeredTypes: @NapkinTypes)

proc processEnum(sp: var SemanticPass, n: AstNode): AstNode =
  let
    name = n.eName.strVal
    typ = n.eType.strVal

  if typ notin OrdinalNapkinTypes:
    quit "Invalid enum type: " & typ

  sp.registeredTypes.add registerType(name)

  return n

proc validateType(sp: var SemanticPass, n: AstNode, identsInScope: seq[string] = @[]): bool =
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
  type
    ProtoNameTypeTriple = tuple[proto: AstNode, name: AstNode, typ: AstNode]
    ProtoNameTypAstTyp = tuple[protoNameTyp: ProtoNameTypeTriple, typ: RegisteredType]

  template proto(i: ProtoNameTypAstTyp): AstNode = i.protoNameTyp.proto
  template name(i: ProtoNameTypAstTyp): AstNode = i.protoNameTyp.name
  template typAst(i: ProtoNameTypAstTyp): AstNode = i.protoNameTyp.typ

  template typName(i: ProtoNameTypeTriple): string =
    (if i.typ.kind == Identifier: i.typ.strVal elif i.typ.kind == Index: i.typ.itarget.strVal else: quit("Invalid AST!"))

  result = n
  let structName = result.sName.strVal
  var
    types = newSeq[set[AstKind]]()
    constructors = newSeq[tuple[protoNameTyp: ProtoNameTypeTriple, typ: RegisteredType]]()
    fieldNames = newSeq[tuple[protoNameTyp: ProtoNameTypeTriple, typ: RegisteredType]]()

  for i in result.sArgs:
    types.add {Identifier}

    var dup: typeof(constructors)

    for it in constructors:
      if it.name.strVal == i.name.strVal:
        dup.add it

    echo i
    if not sp.validateType(i.typ):
      quit "Invalid type: " & $i.typName

    constructors.add (i, sp.registeredTypes[i.typName])

  #for 

  sp.registeredTypes.add registerType(structName, params=types)

proc process*(sp: var SemanticPass): seq[AstNode] =
  for n in sp.nodes:
    result.add case n.kind
      of EnumTypeDef:
        sp.processEnum(n)

      of StructDef:
        sp.processStruct(n)

      of PacketDef:
        echo "Unimplemented!"
        break

      else:
        echo "Invalid node in this context"
        quit()