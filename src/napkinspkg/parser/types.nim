import std/strutils

import ../lexer

type
  ParserFailureReason* = enum
    UnexpectedToken, GenericWithMissingParameters, ExpectedEnumDefinition, MalformedEnumDefinition,
    InvalidPacketDirection, PlaceholderTokenFound

  NapkinsParserError* = object of CatchableError
    fileName*: string
    reason*: ParserFailureReason
    token*: Token

  # proto: NumLiteral
  # packet: Identifier
  ProtoIDPair* = tuple[proto: AstNode, packet: AstNode]

  AstKind* = enum
    NullLiteral,
    NumLiteral,
    Identifier,
    Index,
    PacketDef,
    EnumTypeDef,
    EnumFieldDef,
    PacketFieldDef

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
        indexee*: AstNode               # Identifier
        indexer*: AstNode               # Identifier

      of PacketFieldDef:
        pfName*: AstNode                # Identifier
        pfProto*: AstNode               # NumLiteral
        pfType*: AstNode                # Identifier
        pfOrder*: int                   # ? Order is defined on the parser level

      of EnumFieldDef:
        efName*: AstNode                # Identifier
        efProto*: AstNode               # NumLiteral
        efValue*: AstNode               # NumLiteral
        efOrder*: int                   # ? Order is defined on the parser level

      of EnumTypeDef:
        eName*: AstNode                 # Identifier
        eType*: AstNode                 # Identifier
        eFieldDefs*: seq[AstNode]       # seq[EnumFieldDef]
        eResolved*: bool                # ? Enum value resolution is done during a later parsing stage

      of PacketDef:
        pName*: AstNode # Identifier
        protoIdPairs*: seq[ProtoIDPair]
        direction*: AstNode             # Identifier, to 'Client' or 'Server'
        pFieldDefs*: seq[AstNode]       # seq[PacketFieldDef]
        pResolved*: bool                # ? Packet value resolution is done during a later parsing stage

    when defined(napkinNodeIds):
      id: int

  State* = object
    throwOnError*: bool ## Throws an error instead of quiting
    fileName*: string
    tkPos*, ndPos*: int
    tokens*: seq[Token]
    nodes*: seq[AstNode]

  BuiltinType* = object
    name*: string
    params*: seq[set[AstKind]]

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
      result = repeat(" ", depth * 2) & "Index ->\n"
      result.add repeat(" ", depth * 2) & "Indexee: " & `$`(n.indexee, depth + 1) & '\n'
      result.add repeat(" ", depth * 2) & "Indexer: " & `$`(n.indexer, depth + 1)

    of PacketFieldDef:
      result = repeat(" ", depth * 2) & "PacketFieldDef ->\n"
      result.add repeat(" ", (depth + 1) * 2) & "Name: " & `$`(n.pfName, depth + 1) & '\n'
      result.add repeat(" ", (depth + 1) * 2) & "Proto: " & `$`(n.pfProto, depth + 1) & '\n'
      result.add repeat(" ", (depth + 1) * 2) & "Type: " & `$`(n.pfType, depth + 1) & '\n'
      result.add repeat(" ", (depth + 1) * 2) & "Order: "
      result.addQuoted n.pfOrder

    of EnumFieldDef:
      result = repeat(" ", depth * 2) & "EnumFieldDef ->\n"
      result.add repeat(" ", (depth + 1) * 2) & "Name: " & `$`(n.efName, depth + 1) & '\n'
      result.add repeat(" ", (depth + 1) * 2) & "Proto: " & `$`(n.efProto, depth + 1) & '\n'
      result.add repeat(" ", (depth + 1) * 2) & "Value: " & `$`(n.efValue, depth + 1) & '\n'
      result.add repeat(" ", (depth + 1) * 2) & "Order: "
      result.addQuoted n.efOrder

    of EnumTypeDef:
      result = repeat(" ", depth * 2) & "EnumTypeDef ->\n"
      result.add repeat(" ", (depth + 1) * 2) & "Name: " & `$`(n.eName, depth + 1) & '\n'
      result.add repeat(" ", (depth + 1) * 2) & "Type: " & `$`(n.eType, depth + 1) & '\n'
      result.add repeat(" ", (depth + 1) * 2) & "Enum Fields:\n"
      result.add repeat(" ", (depth + 1) * 2) & "Resolved: " & ($n.eResolved).capitalizeAscii & '\n'
      for i in n.eFieldDefs: result.add `$`(i, depth + 2) & "\n\n"
      result.setLen result.len - 2

    of PacketDef:
      result = repeat(" ", depth * 2) & "PacketDef ->\n"
      result.add repeat(" ", (depth + 1) * 2) & "Name: " & `$`(n.pName, depth + 1)
      for i in n.protoIdPairs:
        result.add repeat(" ", (depth + 1) * 2) & "Proto ID Pair: " & `$`(i.proto, depth + 1) & " -> " &
          `$`(i.packet, depth + 1) & ", "

      result.setLen result.len - 2
      result.add '\n'

      result.add repeat(" ", (depth + 1) * 2) & "Direction: " & `$`(n.direction, depth + 1) & '\n'
      result.add repeat(" ", (depth + 1) * 2) & "Resolved: " & ($n.pResolved).capitalizeAscii & '\n'
      result.add repeat(" ", (depth + 1) * 2) & "Packet Fields:\n"
      for i in n.pFieldDefs: result.add `$`(i, depth + 2) & "\n\n"

      result.setLen result.len - 2

func builtinType*(name: string, params: varargs[set[AstKind]] = newSeq[set[AstKind]]()): BuiltinType =
  BuiltinType(name: name, params: @params)

template isGeneric*(t: BuiltinType): bool = bool(t.params.len)

# TODO: Text Component, JSON Text Component, Entity Metadata, Slot
const NapkinTypes* = [
  # Array[Size, T] - Size refers to anything that defines the length, T refers to the type
  builtinType("Array", {Identifier, NumLiteral}, {Identifier}),
  builtinType("VInt32"),     # VarInt
  builtinType("VInt64"),     # VarLong
  builtinType("UInt8"),      # Unsigned Byte
  builtinType("UInt16"),     # Unsigned Short
  builtinType("SInt8"),      # Signed Byte
  builtinType("SInt16"),     # Signed Short
  builtinType("SInt32"),     # Signed Int
  builtinType("SInt64"),     # Signed Long
  builtinType("Float32"),    # Float
  builtinType("Float64"),    # Double
  builtinType("Bool"),       # Boolean
  # String[MSize] - MSize meaning the maximum length as a NumLiteral
  builtinType("String", {NumLiteral}),
  builtinType("Identifier"), # Identifier
  builtinType("UUID"),       # UUID
  # Optional[Present, T] - Present refers to whether or not a field is present, T refers to the type
  builtinType("Optional", {Identifier, Identifier}),
  builtinType("Position"),   # Position
  # Enum[T] - T refers to the type
  builtinType("Enum", {Identifier}),
  # NBT[Size] - Size refers to the length using another field for definition
  builtinType("NBT", {Identifier})
]

proc contains*(nt: openArray[BuiltinType], name: string): bool =
  for t in nt: (if t.name == name: return true)

proc nodes*(state: State): seq[AstNode] = state.nodes