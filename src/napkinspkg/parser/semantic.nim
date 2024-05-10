import std/[strformat, sequtils, tables]

import ./types

# TODO: Error reporting function

type
  InitialSemanticPass* = object
    nodes*: seq[AstNode]
    registeredTypes*: seq[RegisteredType]

proc initSemanticPass*(nodes: seq[AstNode]): InitialSemanticPass = InitialSemanticPass(nodes: nodes, registeredTypes: @NapkinTypes)

proc processEnum(sp: var InitialSemanticPass, n: AstNode): AstNode =
  let
    name = n.eName.strVal
    typ = n.eType.strVal

  if typ notin OrdinalNapkinTypes:
    quit "Invalid enum type: " & typ

  sp.registeredTypes.add registerType(name)

  return n


proc validateType(sp: var InitialSemanticPass, n: AstNode, typesToValidate: var seq[AstNode],
    identsInScope = newSeq[string]()): bool =
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

      for i in n.iargs:
        if i.kind == Identifier:
          if i.strVal in identsInScope:
            discard

          elif i.strVal in NapkinTypes:
            typesToValidate.add i

        elif i.kind == NumLiteral:
          discard

        elif i.kind == Index:
          typesToValidate.add i.itarget

        else:
          quit "Invalid AST!"

    else:
      quit "Invalid AST!"

  return true


proc processStruct(sp: var InitialSemanticPass, n: AstNode): AstNode =
  type
    ProtoNameTypeTriple = tuple[proto: AstNode, name: AstNode, typ: AstNode]
    ProtoNameTypAstTyp = tuple[protoNameTyp: ProtoNameTypeTriple, typ: RegisteredType]

  template proto(i: ProtoNameTypAstTyp): AstNode = i.protoNameTyp.proto
  template name(i: ProtoNameTypAstTyp): AstNode = i.protoNameTyp.name
  #template typAst(i: ProtoNameTypAstTyp): AstNode = i.protoNameTyp.typ

  template typName(i: AstNode): string =
    (if i.sfType.kind == Identifier: i.sfType.strVal elif i.sfType.kind == Index: i.sfType.itarget.strVal else: quit("Invalid AST!"))
  template typName(i: ProtoNameTypeTriple): string =
    (if i.typ.kind == Identifier: i.typ.strVal elif i.typ.kind == Index: i.typ.itarget.strVal else: quit("Invalid AST!"))

  result = n
  let structName = result.sName.strVal
  var
    types: seq[set[AstKind]]
    constructors: seq[tuple[protoNameTyp: ProtoNameTypeTriple, typ: RegisteredType]]
    fields: seq[tuple[protoNameTyp: ProtoNameTypeTriple, typ: RegisteredType]]
    typesToValidate: seq[AstNode]

  for i in result.sArgs:
    types.add {Identifier}

    var dup: typeof(constructors)

    for it in constructors:
      if it.name.strVal == i.name.strVal:
        dup.add it

    for it in dup:
      if it.proto.numVal < i.proto.numVal:
        quit &"{i.name.strVal} supports protocol version {i.proto.numVal} and above, while the given definition is " &
          &"lower than previously defined! To resolve this, place {it.name.strVal} of protocol version {it.proto.numVal}" &
          &" before the definition for protocol version {i.proto.numVal}!"

      elif it.proto.numVal == i.proto.numVal:
        quit &"{i.name.strVal} already exists for protocol version {i.proto.numVal} and above with the same type!"


    typesToValidate.add i.typ
    while typesToValidate.len > 0:
      let typ = typesToValidate.pop()
      if not sp.validateType(typ, typesToValidate, constructors.mapIt(it.name.strVal)):
        quit "Invalid type: " & $typ

    constructors.add (i, sp.registeredTypes[i.typName])

  sp.registeredTypes.add registerType(structName, params=types)

  for field in result.sFieldDefs:
    typesToValidate.add field.sfType
    while typesToValidate.len > 0:
      let typ = typesToValidate.pop()
      if not sp.validateType(typ, typesToValidate, fields.mapIt(it.name.strVal) & constructors.mapIt(it.name.strVal)):
        quit "Invalid type: " & $typ

    fields.add ((field.sfProto, field.sfName, field.sfType), sp.registeredTypes[field.typName])


proc processPacket(sp: var InitialSemanticPass, n: AstNode): AstNode =
  result = n

  var protoIds: seq[ProtoIDPair]

  for i in result.protoIdPairs:
    for x in protoIds:
      if x.proto >= i.proto:
        quit "Protocol-Packet ID pairs must be in ascending order sorted by protocol!"

    protoIds.add i

  var
    typesToValidate: seq[AstNode]
    fields: seq[AstNode]

  for field in result.pFieldDefs:
    typesToValidate.add field.pfType

    while typesToValidate.len > 0:
      let typ = typesToValidate.pop()
      if not sp.validateType(typ, typesToValidate, fields.mapIt(it.pfName.strVal)):
        quit "Invalid type: " & $typ

    fields.add field

proc process*(sp: var InitialSemanticPass): seq[AstNode] =
  for n in sp.nodes:
    result.add case n.kind
      of EnumTypeDef:
        sp.processEnum(n)

      of StructDef:
        sp.processStruct(n)

      of PacketDef:
        sp.processPacket(n)

      else:
        echo "Invalid node in this context"
        quit()