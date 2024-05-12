import std/[strformat, sequtils, tables]

import ./base
import ../parser/types

type
  FinalSemanticPass* = object of BasePass
    registeredTypes*: seq[RegisteredType]
    registeredEnums*: Table[string, seq[string]]

proc initFinalSemanticPass*(fileName: string, nodes: seq[AstNode], throwOnError = false): FinalSemanticPass =
  FinalSemanticPass(fileName: fileName, nodes: nodes, throwOnError: throwOnError, registeredTypes: @NapkinTypes)


proc processEnum*(p: var FinalSemanticPass, n: AstNode): AstNode =
  result = n

  p.registeredEnums[result.eName.strVal] = @[]

  var highVal, lowVal: int

  case result.eType.strVal
    of "VInt32":
      highVal = high(int32)
      lowVal = low(int32)

    of "VInt64":
      highVal = high(int64)
      lowVal = low(int64)

    of "UInt8":
      highVal = high(uint8).int
      lowVal = low(uint8).int

    of "UInt16":
      highVal = high(uint16).int
      lowVal = low(uint16).int

    of "SInt8":
      highVal = high(int8)
      lowVal = low(int8)

    of "SInt16":
      highVal = high(int16)
      lowVal = low(int16)

    of "SInt32":
      highVal = high(int32)
      lowVal = low(int32)

    of "SInt64":
      highVal = high(int64)
      lowVal = low(int64)

    else:
      p.report "Invalid enum type: " & result.eType.strVal, result.eType

  var value = 0

  if result.eFieldDefs.len == 0:
    p.report "Enums must have at least one field!", result.eType

  let v = result.eFieldDefs[0].efValue.numVal
  if (v < lowVal) or (v > highVal):
    p.report &"Invalid enum value `{v}` for type \"{result.eType.strVal}\"!", result.eFieldDefs[0].efValue

  value = v
  inc value

  if result.eFieldDefs.len < 2:
    return

  var values = @[v]

  for i in 1..<(result.eFieldDefs.len):
    template field: var AstNode = result.eFieldDefs[i]
    p.registeredEnums[result.eName.strVal].add field.efName.strVal

    if field.efValue.numVal != 0:
      value = field.efValue.numVal
    else:
      field.efValue.numVal = value

    if (value < lowVal) or (value > highVal):
      p.report &"Invalid enum value `{value}` for type \"{result.eType.strVal}\", out of range {lowVal}..{highVal}!",
        field.efValue

    for x in result.eFieldDefs:
      if x.efValue.numVal == value:
        p.report &"Duplicate enum value `{value}` for field \"{x.efName.strVal}\" found in \"{field.efName.strVal}\"!", field.efValue

    inc value

  result.eResolved = true


proc validateType(p: var FinalSemanticPass, n: AstNode, typesToValidate: var seq[AstNode],
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


proc validateCondition*(p: var FinalSemanticPass, c: Condition, identsInScope = newSeq[string](),
    identsUsed: var seq[string]): bool =
  case c.kind
    of EnumComparison:
      case c.targetField.kind
        of Identifier:
          if c.targetField.strVal notin identsInScope:
            p.report &"Can't find an identifier with the name `{c.targetField}` in scope!", c.targetField

        of Index:
          if c.targetField.itarget.strVal notin identsInScope:
            p.report &"Can't find an identifier with the name `{c.targetField.itarget}` in scope!", c.targetField.itarget

          # TODO: Struct field access

        else:
          p.report &"Malformed AST! Expected an identifier or an indexable type, but got `{c.targetField.kind}`!", c.targetField

      if c.enumValue.kind != DotExpr:
        p.report "Enum fields must be accessed with `.` for clarity!", c.enumValue

      if c.enumValue.dleft.kind != Identifier:
        p.report &"Malformed AST! Expected an identifier, but got `{c.enumValue.dleft.kind}`!", c.enumValue.dleft

      if c.enumValue.dleft.strVal notin p.registeredEnums:
        p.report &"Enum with name `{c.enumValue.dleft.strVal}` can't be found in scope!", c.enumValue

      if c.enumValue.dright.kind != Identifier:
        p.report &"Malformed AST! Expected an identifier, but got `{c.enumValue.dright.kind}`!", c.enumValue.dright

      if c.enumValue.dright.strVal notin p.registeredEnums[c.enumValue.dleft.strVal]:
        p.report &"Enum `{c.enumValue.dleft.strVal}` doesn't contain `{c.enumValue.dright.strVal}`!", c.enumValue

    of Expr:
      discard


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


proc processStruct*(p: var FinalSemanticPass, n: AstNode): AstNode =
  # Copy and pasted from `initial_semantic.nim`, could separate into a `semantic_common.nim` file?
  result = n

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

  # TODO: Validate conditions in fields


  result.sResolved = true


proc processPacket*(p: var FinalSemanticPass, n: AstNode): AstNode =
  # Copy and pasted from `initial_semantic.nim`
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

  result.pResolved = true