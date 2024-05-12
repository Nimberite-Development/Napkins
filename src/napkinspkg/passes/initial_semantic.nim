import std/[strformat, sequtils, tables]

import ../parser/types

import ./base

# TODO: Consider dropping this pass? `FinalSemanticPass` could be enough if enum checks are moved there,
# TODO: the rest of the logic is shared between these passes.

type
  InitialSemanticPass* = object of BasePass
    registeredTypes*: seq[RegisteredType]

proc initInitalSemanticPass*(fileName: string, nodes: seq[AstNode], throwOnError = false): InitialSemanticPass =
  InitialSemanticPass(fileName: fileName, nodes: nodes, throwOnError: throwOnError, registeredTypes: @NapkinTypes)

proc processEnum*(p: var InitialSemanticPass, n: AstNode): AstNode =
  result = n

  let
    name = result.eName.strVal
    typ = result.eType.strVal

  if typ notin OrdinalNapkinTypes:
    p.report "Invalid enum type: " & typ, result.eType

  var fieldTable: Table[string, seq[int]]

  for i in result.eFieldDefs:
    if not fieldTable.hasKey(i.efName.strVal):
      fieldTable[i.efName.strVal] = @[i.efProto.numVal]
      continue

    if i.efProto.numVal in fieldTable[i.efName.strVal]:
      p.report &"Duplicate enum definition for field \"{i.efName.strVal}\" and protocol `{i.efProto.numVal}!`", i.efName

  p.registeredTypes.add registerType(name)


proc validateType(p: var InitialSemanticPass, n: AstNode, typesToValidate: var seq[AstNode],
    identsInScope = newSeq[string]()): bool =
  case n.kind
    of Identifier:
      if n.strVal notin p.registeredTypes:
        p.report "Unknown type: " & n.strVal, n

      if p.registeredTypes[n.strVal].isGeneric:
        p.report "Type is generic: " & n.strVal, n

    of Index:
      let typName = n.itarget.strVal
      if typName notin p.registeredTypes:
        p.report "Unknown type: " & typName, n

      let typ = p.registeredTypes[typName]

      if not typ.isGeneric:
        p.report "Type is not generic: " & typName, n

      if n.iargs.len != typ.params.len:
        p.report "Invalid number of arguments for type: " & typName, n

      for x in 0..<n.iargs.len:
        if n.iargs[x].kind notin typ.params[x]:
          p.report "Invalid type for index: " & $n.iargs[x], n.iargs[x]

      for i in n.iargs:
        if i.kind == Identifier:
          if i.strVal in identsInScope:
            discard

          elif i.strVal in NapkinTypes:
            typesToValidate.add i

        elif i.kind == NumLiteral:
          discard

        elif i.kind == Index:
          typesToValidate.add i

        else:
          p.report "Invalid AST!", i

    else:
      p.report "Invalid AST!", n

  return true


template typName(i: AstNode): string {.dirty.} =
  var res: string

  if i.sfType.kind == Identifier:
    res = i.sfType.strVal
  elif i.sfType.kind == Index:
    res = i.sfType.itarget.strVal
  else:
    let kind = i.sfType.kind
    p.report "Expected an identifier or an indexable type, but got `" & $kind & "`!", i.sfType

  res


proc processStruct*(p: var InitialSemanticPass, n: AstNode): AstNode =
  type
    ProtoNameTypeTriple = tuple[proto: AstNode, name: AstNode, typ: AstNode]
    ProtoNameTypAstTyp = tuple[protoNameTyp: ProtoNameTypeTriple, typ: RegisteredType]

  template proto(i: ProtoNameTypAstTyp): AstNode = i.protoNameTyp.proto
  template name(i: ProtoNameTypAstTyp): AstNode = i.protoNameTyp.name
  #template typAst(i: ProtoNameTypAstTyp): AstNode = i.protoNameTyp.typ

  template typName(i: ProtoNameTypeTriple): string =
    var res: string

    if i.typ.kind == Identifier:
      res = i.typ.strVal
    elif i.typ.kind == Index:
      res = i.typ.itarget.strVal
    else:
      let kind = i.typ.kind
      p.report "Expected an identifier or an indexable type, but got `" & $kind & "`!", i.typ

    res

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
        p.report &"{i.name.strVal} supports protocol version {i.proto.numVal} and above, while the given definition is " &
          &"lower than previously defined! To resolve this, place {it.name.strVal} of protocol version {it.proto.numVal}" &
          &" before the definition for protocol version {i.proto.numVal}!", i.proto

      elif it.proto.numVal == i.proto.numVal:
        p.report &"{i.name.strVal} already exists for protocol version {i.proto.numVal} and above with the same type!", i.proto


    typesToValidate.add i.typ
    while typesToValidate.len > 0:
      let typ = typesToValidate.pop()
      if not p.validateType(typ, typesToValidate, constructors.mapIt(it.name.strVal)):
        p.report &"The type \"{typName(typ)}\" does not exist!", typ

    constructors.add (i, p.registeredTypes[i.typName])

  p.registeredTypes.add registerType(structName, params=types)

  for field in result.sFieldDefs:
    typesToValidate.add field.sfType
    while typesToValidate.len > 0:
      let typ = typesToValidate.pop()
      if not p.validateType(typ, typesToValidate, fields.mapIt(it.name.strVal) & constructors.mapIt(it.name.strVal)):
        p.report &"The type \"{typName(typ)}\" does not exist!", typ

    fields.add ((field.sfProto, field.sfName, field.sfType), p.registeredTypes[field.typName])


proc processPacket*(p: var InitialSemanticPass, n: AstNode): AstNode =
  result = n

  var protoIds: seq[ProtoIDPair]

  for i in result.protoIdPairs:
    for x in protoIds:
      if x.proto >= i.proto:
        p.report "Protocol-Packet ID pairs must be in ascending order sorted by protocol!", i.proto

    protoIds.add i

  var
    typesToValidate: seq[AstNode]
    fields: seq[AstNode]

  for field in result.pFieldDefs:
    typesToValidate.add field.pfType

    while typesToValidate.len > 0:
      let typ = typesToValidate.pop()
      if not p.validateType(typ, typesToValidate, fields.mapIt(it.pfName.strVal)):
        p.report &"The type \"{typName(typ)}\" does not exist!", typ

    fields.add field