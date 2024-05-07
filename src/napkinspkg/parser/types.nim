import std/strutils

import ../lexer

type
  FieldParseResult* = enum
    HandledConditionalField, HandledIndex, AddNewField, FoundDedent

  ParserFailureReason* = enum
    UnexpectedToken, GenericWithMissingParameters, ExpectedEnumDefinition, MalformedEnumDefinition,
    IndexOnNonIndexableValue, InvalidPacketDirection, PlaceholderTokenFound

  NapkinsParserError* = object of CatchableError
    fileName*: string
    reason*: ParserFailureReason
    token*: Token

  PacketDirection* = enum
    Client, Server # Client == *To* Client, Server == *To* Server

  # proto: NumLiteral
  # packet: Identifier
  ProtoIDPair* = tuple[proto: AstNode, packet: AstNode]

  AstKind* = enum
    NullLiteral,
    NumLiteral,
    Identifier,
    Index,
    DotExpr,
    StructDef,
    PacketDef,
    EnumTypeDef,
    EnumFieldDef,
    PacketFieldDef,
    StructFieldDef,
    FunctionCall # Only used for conditional fields!

  ConditionKind* = enum
    EnumComparison, Expr

  Condition* = object
    # Used for packet decode and encode 
    case kind*: ConditionKind
      of EnumComparison:
        targetField*: AstNode
        enumValue*: AstNode

      of Expr:
        exp*: AstNode # FunctionCall

  AstNode* {.acyclic.} = ref object
    # TODO: Add line info data - Use `std/macros`' `LineInfo` or our own type?
    case kind*: AstKind
      of NullLiteral:
        discard

      of NumLiteral:
        numVal*: int
      
      of Identifier:
        strVal*: string

      of Index:
        itarget*: AstNode               # Identifier
        iargs*: seq[AstNode]            # seq[Identifier | NumLiteral | Index]

      of DotExpr:
        dleft*: AstNode                 # Identifier
        dright*: AstNode                # Identifier

      of PacketFieldDef:
        pfName*: AstNode                # Identifier
        pfProto*: AstNode               # NumLiteral
        pfType*: AstNode                # Identifier
        pfConditions*: seq[Condition]   # seq[Condition] - Used for conditional fields
        pfOrder*: int                   # ? Order is defined on the parser level

      of EnumFieldDef:
        efName*: AstNode                # Identifier
        efProto*: AstNode               # NumLiteral
        efValue*: AstNode               # NumLiteral
        efOrder*: int                   # ? Order is defined on the parser level

      of StructFieldDef:
        sfName*: AstNode                # Identifier
        sfProto*: AstNode               # NumLiteral
        sfType*: AstNode                # Identifier
        sfConditions*: seq[Condition]   # seq[Condition] - Used for conditional fields
        sfOrder*: int                   # ? Order is defined on the parser level

      of StructDef:
        sName*: AstNode                 # Identifier
        sArgs*: seq[(AstNode, AstNode)] # seq[Identifier | NumLiteral | Index] x2 - Name-Type pair
        sFieldDefs*: seq[AstNode]       # seq[StructFieldDef]
        sResolved*: bool                # ? Struct value resolution is done during a later parsing stage

      of EnumTypeDef:
        eName*: AstNode                 # Identifier
        eType*: AstNode                 # Identifier
        eFieldDefs*: seq[AstNode]       # seq[EnumFieldDef]
        eResolved*: bool                # ? Enum value resolution is done during a later parsing stage

      of PacketDef:
        pName*: AstNode # Identifier
        protoIdPairs*: seq[ProtoIDPair]
        pDirection*: PacketDirection
        pFieldDefs*: seq[AstNode]       # seq[PacketFieldDef]
        pResolved*: bool                # ? Packet value resolution is done during a later parsing stage

      of FunctionCall:
        fcName*: AstNode                # Identifier - Function name, `!`, `<<`, `>>`, `&` and `|`
        fcArgs*: seq[AstNode]           # seq[Identifier | NumLiteral] - Function arguments

    when defined(napkinNodeIds):
      id*: int

  State* = object
    throwOnError*: bool ## Throws an error instead of quiting
    fileName*: string
    tkPos*, ndPos*: int
    tokens*: seq[Token]
    nodes*: seq[AstNode]

    when defined(napkinNodeIds):
      nodeId*: int

  RegisteredType* = object
    name*: string
    params*: seq[set[AstKind]]

proc `$`*(n: AstNode, depth: int = 0): string

proc `$`*(c: Condition, depth: int = 0): string =
  case c.kind
    of EnumComparison:
      result = repeat(" ", depth * 2) & "EnumComparison ->\n"
      result.add repeat(" ", (depth + 1) * 2) & "TargetField: " & `$`(c.targetField, depth + 2) & '\n'
      result.add repeat(" ", (depth + 1) * 2) & "EnumValue: " & `$`(c.enumValue, depth + 2)

    of Expr:
      result = repeat(" ", depth * 2) & "Expr ->\n"
      result.add repeat(" ", (depth + 1) * 2) & `$`(c.exp, depth + 2)

proc `$`*(n: AstNode, depth: int = 0): string =
  case n.kind
    of NullLiteral:
      result = "Null"

    of NumLiteral:
      result = "NumLiteral `"
      result.addQuoted n.numVal
      result.add "`"

    of Identifier:
      result = "Identifier "
      result.addQuoted n.strVal

    of Index:
      result = "Index ->\n"
      result.add repeat(" ", (depth + 1) * 2) & "Target: " & `$`(n.itarget, (depth + 1) + 1) & '\n'
      result.add repeat(" ", (depth + 1) * 2) & "Args ->\n"
      for i in n.iargs: result.add repeat(" ", (depth + 2) * 2) & `$`(i, depth + 2) & '\n'
      result.setLen(result.len - 1)

    of DotExpr:
      result = "DotExpr ->\n"
      result.add repeat(" ", (depth) * 2) & "Left: " & `$`(n.dleft, (depth + 1) + 1) & '\n'
      result.add repeat(" ", (depth) * 2) & "Right: " & `$`(n.dright, (depth + 1) + 1)

    of PacketFieldDef:
      result = repeat(" ", depth * 2) & "PacketFieldDef ->\n"
      result.add repeat(" ", (depth + 1) * 2) & "Name: " & `$`(n.pfName, depth + 1) & '\n'
      result.add repeat(" ", (depth + 1) * 2) & "Proto: " & `$`(n.pfProto, depth + 1) & '\n'
      result.add repeat(" ", (depth + 1) * 2) & "Type: " & `$`(n.pfType, depth + 1) & '\n'
      result.add repeat(" ", (depth + 1) * 2) & "Order: "
      result.addQuoted n.pfOrder
      result.add '\n'
      result.add repeat(" ", (depth + 1) * 2) & "Conditions ->\n"
      for i in n.pfConditions: result.add `$`(i, depth + 2) & "\n\n"
      if n.pfConditions.len > 0:
        result.setLen(result.len - 2)
      else:
        result.setLen(result.len - 1)
        result.add " N/A"

    of EnumFieldDef:
      result = repeat(" ", depth * 2) & "EnumFieldDef ->\n"
      result.add repeat(" ", (depth + 1) * 2) & "Name: " & `$`(n.efName, depth + 1) & '\n'
      result.add repeat(" ", (depth + 1) * 2) & "Proto: " & `$`(n.efProto, depth + 1) & '\n'
      result.add repeat(" ", (depth + 1) * 2) & "Value: " & `$`(n.efValue, depth + 1) & '\n'
      result.add repeat(" ", (depth + 1) * 2) & "Order: "
      result.addQuoted n.efOrder

    of StructFieldDef:
      result = repeat(" ", depth * 2) & "StructFieldDef ->\n"
      result.add repeat(" ", (depth + 1) * 2) & "Name: " & `$`(n.sfName, depth + 1) & '\n'
      result.add repeat(" ", (depth + 1) * 2) & "Proto: " & `$`(n.sfProto, depth + 1) & '\n'
      result.add repeat(" ", (depth + 1) * 2) & "Type: " & `$`(n.sfType, depth + 1) & '\n'
      result.add repeat(" ", (depth + 1) * 2) & "Order: "
      result.addQuoted n.sfOrder
      result.add '\n'
      result.add repeat(" ", (depth + 1) * 2) & "Conditions:\n"
      for i in n.sfConditions: result.add `$`(i, depth + 2) & "\n\n"
      if n.sfConditions.len > 0:
        result.setLen(result.len - 2)
      else:
        result.setLen(result.len - 1)
        result.add " N/A"

    of StructDef:
      result = repeat(" ", depth * 2) & "StructDef ->\n"
      result.add repeat(" ", (depth + 1) * 2) & "Name: " & `$`(n.sName, depth + 1) & '\n'
      result.add repeat(" ", (depth + 1) * 2) & "Struct Arg-Type Pairs:\n"
      for i in n.sArgs:
        result.add repeat(" ", (depth + 2) * 2) & `$`(i[0], depth + 2) & " -> " & `$`(i[1], depth + 2) & '\n'
      if n.sArgs.len == 0:
        result.setLen result.len - 1
        result.add " N/A\n"
      result.add repeat(" ", (depth + 1) * 2) & "Resolved: " & ($n.sResolved).capitalizeAscii & '\n'
      result.add repeat(" ", (depth + 1) * 2) & "Struct Fields:\n"
      for i in n.sFieldDefs: result.add `$`(i, depth + 2) & "\n\n"
      result.setLen result.len - 2

    of EnumTypeDef:
      result = repeat(" ", depth * 2) & "EnumTypeDef ->\n"
      result.add repeat(" ", (depth + 1) * 2) & "Name: " & `$`(n.eName, depth + 1) & '\n'
      result.add repeat(" ", (depth + 1) * 2) & "Type: " & `$`(n.eType, depth + 1) & '\n'
      result.add repeat(" ", (depth + 1) * 2) & "Resolved: " & ($n.eResolved).capitalizeAscii & '\n'
      result.add repeat(" ", (depth + 1) * 2) & "Enum Fields:\n"
      for i in n.eFieldDefs: result.add `$`(i, depth + 2) & "\n\n"
      result.setLen result.len - 2

    of PacketDef:
      result = repeat(" ", depth * 2) & "PacketDef ->\n"
      result.add repeat(" ", (depth + 1) * 2) & "Name: " & `$`(n.pName, depth + 1)
      result.add '\n'
      for i in n.protoIdPairs:
        result.add repeat(" ", (depth + 1) * 2) & "Proto ID Pair: " & `$`(i.proto, depth + 1) & " -> " &
          `$`(i.packet, depth + 1) & ", "
      result.setLen result.len - 2
      result.add '\n'
      result.add repeat(" ", (depth + 1) * 2) & "Direction: " & $n.pDirection & '\n'
      result.add repeat(" ", (depth + 1) * 2) & "Resolved: " & ($n.pResolved).capitalizeAscii & '\n'
      result.add repeat(" ", (depth + 1) * 2) & "Packet Fields:\n"
      for i in n.pFieldDefs: result.add `$`(i, depth + 2) & "\n\n"

      result.setLen result.len - 2

    of FunctionCall:
      result = "FunctionCall ->\n"
      result.add repeat(" ", depth * 2) & "Name: " & `$`(n.fcName, depth + 1) & '\n'
      result.add repeat(" ", depth * 2) & "Arguments ->\n"
      for i in n.fcArgs: result.add repeat(" ", (depth + 1) * 2) & `$`(i, depth + 2) & "\n"
      result.setLen result.len - 1

proc `$`*(n: seq[AstNode]): string =
  for i in n:
    result &= $i & "\n\n"

  result.setLen result.len - 2

func registerType*(name: string, params: varargs[set[AstKind]] = newSeq[set[AstKind]]()): RegisteredType =
  RegisteredType(name: name, params: @params)

template isGeneric*(t: RegisteredType): bool = bool(t.params.len)

# TODO: Text Component, JSON Text Component, Entity Metadata, Slot
const NapkinTypes* = [
  # Array[Size, T] - Size refers to anything that defines the length, T refers to the type
  registerType("Array", {Identifier, NumLiteral}, {Identifier, Index}),
  registerType("VInt32"),     # VarInt
  registerType("VInt64"),     # VarLong
  registerType("UInt8"),      # Unsigned Byte
  registerType("UInt16"),     # Unsigned Short
  registerType("SInt8"),      # Signed Byte
  registerType("SInt16"),     # Signed Short
  registerType("SInt32"),     # Signed Int
  registerType("SInt64"),     # Signed Long
  registerType("Float32"),    # Float
  registerType("Float64"),    # Double
  registerType("Bool"),       # Boolean
  # String[MSize] - MSize meaning the maximum length as a NumLiteral
  registerType("String", {NumLiteral}),
  registerType("Identifier"), # Identifier
  registerType("UUID"),       # UUID
  # Optional[Present, T] - Present refers to whether or not a field is present, T refers to the type
  registerType("Optional", {Identifier}, {Identifier}),
  registerType("Position"),   # Position
  # Enum[T] - T refers to the type
  registerType("Enum", {Identifier}),
  # NBT[Size] - Size refers to the length using another field for definition
  registerType("NBT", {Identifier}),
  # Constrain[T, U, V] - T refers to the type, U refers to the maximum value, V refers to the minimum value
  registerType("ConstrainUV", {Identifier}, {NumLiteral}, {NumLiteral}),
  # Constrain[T, U] - T refers to the type, U refers to the maximum value
  registerType("ConstrainU", {Identifier}, {NumLiteral})
]

proc contains*(nt: openArray[RegisteredType], name: string): bool =
  for t in nt: (if t.name == name: return true)

proc `[]`*(nt: openArray[RegisteredType], name: string): RegisteredType =
  for t in nt: (if t.name == name: return t)

proc nodes*(state: State): seq[AstNode] = state.nodes