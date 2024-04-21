import std/[
  strformat
]

import ./lexer

type
  ParserFailureReason* = enum
    UnexpectedToken, ExpectedEnumDefinition, PlaceholderTokenFound

  NapkinsParserError* = object of CatchableError
    fileName*: string
    reason*: ParserFailureReason
    token*: Token

  # proto: NumLiteral
  # packet: Identifier
  ProtoIDPair* = tuple[proto: AstNode, packet: AstNode]

  # proto: NumLiteral
  # typ: Identifier | Index
  # order: int #? Order is defined on the parser level
  ProtoTypTriad* = tuple[proto: AstNode, typ: AstNode, order: int]

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
        indexer*: AstNode # Identifier
        indexee*: AstNode # Identifier

      of PacketFieldDef:
        pfName*: AstNode # Identifier
        protoTypTriads*: seq[ProtoTypTriad]

      of EnumFieldDef:
        efName: AstNode # Identifier
        efValue: int # NumLiteral


      of EnumTypeDef:
        eName*: AstNode # Identifier
        values*: seq[AstNode] # seq[EnumFieldDef]

      of PacketDef:
        pName*: AstNode # Identifier
        protoIdPairs*: seq[ProtoIDPair]
        direction*: AstNode # Identifier, 'Client' or 'Server'
        fieldDefs*: seq[AstNode] # seq[PacketFieldDef]

    when defined(napkinNodeIds):
      id: int

  State* = object
    throwOnError*: bool ## Throws an error instead of quiting
    fileName: string
    tkPos, ndPos: int
    tokens: seq[Token]
    nodes: seq[AstNode]

  BuiltinType* = object
    name*: string
    params*: seq[set[AstKind]]

proc builtinType*(name: string, params: varargs[set[AstKind]] = newSeq[set[AstKind]]()): BuiltinType =
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
  for t in nt:
    if t.name == name: return true

  return false

proc nodes*(state: State): seq[AstNode] = state.nodes

proc report(s: State, msg: string, reason: ParserFailureReason, token: Token) =
  template errstr: string = &"{s.fileName}:{token.startLine}:{token.startColumn}"

  if s.throwOnError:
    let err = newException(NapkinsParserError,
      &"[{errstr}] {msg}")
    (err.fileName, err.token, err.reason) = (s.fileName, token, reason)
    raise err

  else:
    quit &"[{errstr}] {msg}"

template atTkEnd(s: var State): bool = s.tkPos >= s.tokens.len
proc eat(s: var State): Token =
  result = s.tokens[s.tkPos]
  inc s.tkPos

  if result.typ == Placeholder:
    s.report "Placeholder tokens are only to be used during the lexing stage for syntax validation!",
      PlaceholderTokenFound, result

proc parseEnum*(s: var State, ident: Token): AstNode =
  result = AstNode(kind: EnumTypeDef, eName: AstNode(kind: Identifier, strVal: ident.val))

  let tkOne = s.eat()

  if tkOne.typ != Ident:
    s.report "Expected an identifier!", UnexpectedToken, tkOne

  if tkOne.val != "Enum":
    s.report "Expected 'Enum'!", ExpectedEnumDefinition, tkOne

proc parse*(tokens: seq[Token], fileName: string = "<string>",
    throwOnError: bool = true): State =
  result = State(tokens: tokens, fileName: fileName, throwOnError: throwOnError)
  # Use lexer object instead? Would provide more info with less redundancy

  while not result.atTkEnd:
    let tkOne = result.eat()

    if tkOne.typ != Ident:
      result.report "Expected an identifier!", UnexpectedToken, tkOne

    let tkTwo = result.eat()

    case tkTwo.typ
      of Arrow:
        result.parseEnum(tkOne)

      of OpenParen:
        discard

      else:
        result.report "Expected an arrow or an open parenthesis!", UnexpectedToken, tkTwo


    # Unimplemented codepath
    result.report "Unimplemented behaviour from this point on.", UnexpectedToken, result.eat()