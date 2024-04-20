import std/[
  strformat
]

import ./lexer

type
  ParserFailureReason* = enum
    UnexpectedToken, PlaceholderTokenFound

  NapkinsParserError* = object of CatchableError
    fileName*: string
    reason*: ParserFailureReason
    token*: Token

  # proto: NumLiteral
  # packet: Identifier
  ProtoIDPair* = tuple[proto, packet: AstPointer]

  # proto: NumLiteral
  # typ: Identifier | Index
  # order: int #? Order is defined on the parser level
  ProtoTypTriad* = tuple[proto: AstPointer, typ: AstPointer, order: int]

  # val: NumLiteral
  # name: Identifier
  # Used for defining enums
  ValueNamePair* = tuple[val: AstPointer, name: AstPointer]

  AstKind* = enum
    NullLiteral,
    NumLiteral,
    Identifier,
    Index,
    EnumDef,
    FieldDef,
    PacketDef,

  AstPointer* = distinct int

  AstNode* = object
    # TODO: Add line info data - Use `std/macros`' `LineInfo` or our own type?
    case kind*: AstKind
      of NullLiteral:
        discard

      of NumLiteral:
        numVal*: int
      
      of Identifier:
        strVal*: string

      of Index:
        indexer*: AstPointer # Identifier
        indexee*: AstPointer # Identifier

      of FieldDef:
        fName*: AstPointer # Identifier
        protoTypTriads*: seq[ProtoTypTriad]

      of EnumDef:
        eName*: AstPointer # Identifier
        values*: seq[AstPointer] # seq[]

      of PacketDef:
        pName*: AstPointer # Identifier
        protoIdPairs*: seq[ProtoIDPair]
        direction*: AstPointer # Identifier, 'Client' or 'Server'
        fieldDefs*: seq[AstPointer] # seq[FieldDef]

    when defined(napkinNodeIds):
      id: int

  State* = object
    throwOnError*: bool ## Throws an error instead of quiting
    fileName: string
    tkPos, ndPos: int
    tokens: seq[Token]
    nodes: seq[AstNode]

proc `[]`*(state: State, idx: AstPointer): AstNode =
  result = state.nodes[idx.int]

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

proc parse*(tokens: seq[Token], fileName: string = "<string>",
    throwOnError: bool = true): State =
  result = State(tokens: tokens, throwOnError: throwOnError)

  while not result.atTkEnd:
    let tk = result.eat()

    if tk.typ != Ident:
      result.report "Expected an identifier!", UnexpectedToken, tk

    # TODO: Rest of the parser