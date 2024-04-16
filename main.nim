import std/[strformat, strutils]

type
  TkType* = enum
    Skip, Ident, Num, Null, OpenParen, CloseParen, OpenBrack,
    CloseBrack, Comma, Arrow, Colon, Indent, Dedent

  Token* = object
    typ*: TkType
    val*: string
    startLine*, startColumn*: int
    when defined(napkinTokenIds):
      id: int

  ProtoIDPair* = tuple[proto: int, packet: int]
  ProtoTypTriad* = tuple[proto: int, typ: AstNode, order: int] # typ: Identifier | Index

  AstKind* = enum
    NullLiteral,
    Identifier,
    Index,
    FieldDef,
    PacketDef,
    PacketDefs

  AstNode* = distinct int

  AstData* = object
    case kind*: AstKind
      of NullLiteral:
        discard
      
      of Identifier:
        strVal*: string

      of Index:
        indexer*: AstNode # Identifier
        indexee*: AstNode # Identifier

      of FieldDef:
        fName*: AstNode # Identifier
        protoTypTriads*: seq[ProtoTypTriad]

      of PacketDef:
        pName*: AstNode # Identifier
        protoIdPairs*: seq[ProtoIDPair]
        direction*: AstNode # Identifier, 'Client' or 'Server'
        fieldDefs*: seq[AstNode] # seq[FieldDef]

      of PacketDefs:
        packets*: seq[AstNode] # seq[PacketDef]

  Lexer* = object
    # TODO: Tab support
    code: string # TODO: Streams?
    pos, indentDepth: int
    column, line: int = 1
    when defined(napkinTokenIds):
      tokenId: int

  State* = object
    nodes: seq[AstData]

proc `[]`*(state: State, idx: AstNode): AstData =
  result = state.nodes[idx.int]

proc nodes*(state: State): seq[AstData] = state.nodes

proc `$`*(token: Token, depth: int = 1): string =
  let indent = repeat("  ", depth)
  result &= &"Token(\n{indent}typ: {token.typ},\n"
  result &= &"{indent}val: "

  result.addQuoted(token.val)
  when defined(napkinTokenIds):
    result &= &"\n{indent}id: {token.id}"

  result &= "\n)"

proc `$`*(tokens: seq[Token]): string =
  result = "@[\n"
  for token in tokens:
    result &= "  " & `$`(token, 2)
    result.setLen(result.len - 2)
    result &= "\n  ),\n"

  result.setLen(result.len - 2)

  result &= "\n]"
  result &= ")"

var lexer = Lexer(code: readFile("test.nmp"))

template atEnd(l: Lexer): bool = l.pos >= l.code.len
template cchar(l: Lexer): char = l.code[l.pos]

when defined(napkinTokenIds):
  template setTokenId(t: var Token) =
    t.id = lexer.tokenId
    inc l.tokenId
else:
  template setTokenId(t: var Token) = discard

proc next(l: var Lexer) =
  if l.cchar == '\n':
    inc l.line
    l.column = 1

  else:
    inc l.column

  inc l.pos

proc lexIdentifier(l: var Lexer): Token =
  var lexeme: string

  while l.cchar.isAlphaNumeric:
    lexeme &= l.cchar
    l.next()

  result = Token(typ: (if lexeme != "Null": Ident else: Null), val: lexeme)
  result.setTokenId()

proc lexNumOrArr(l: var Lexer): Token =
  var lexeme = $l.cchar
  l.next()

  if lexeme[0] == '-':
    if l.cchar == '>':
      lexeme &= l.cchar
      l.next()
      result = Token(typ: Arrow, val: lexeme)
      result.setTokenId()
      return
      
    elif not l.cchar.isDigit:
      lexeme &= l.cchar
      l.next()
      quit &"{lexeme} isn't a valid number!"

  var isHex = false

  if l.cchar == 'x':
    isHex = true
    lexeme &= 'x'
    l.next()

  template validChars: string =
    if isHex:
      "0123456789ABCDEFabcdef"
    else:
      "0123456789"

  while l.cchar in validChars:
    lexeme &= l.cchar
    l.next()

  if isHex:
    lexeme = $parseHexInt(lexeme)

  result = Token(typ: Num, val: lexeme)
  result.setTokenId()

proc lexIndent(l: var Lexer): Token =
  var lexeme: string

  while l.cchar == ' ':
    lexeme &= l.cchar
    l.next()

  if l.indentDepth == 0:
    l.indentDepth = lexeme.len
    result = Token(typ: Indent, val: lexeme)
    result.setTokenId()
    return

  else:
    if l.indentDepth != lexeme.len:
      quit &"The spacing isn't consistent, expected {l.indentDepth} spaces! {l.line} '{lexeme}'"

proc lex(l: var Lexer): seq[Token] =
  # TODO: Error reporting function
  while not l.atEnd:
    if l.cchar.isAlphaAscii:
      result &= l.lexIdentifier()
      continue

    elif l.cchar.isDigit or l.cchar == '-':
      result &= l.lexNumOrArr()
      continue

    else:
      var typ = case l.cchar
        of '(': OpenParen
        of ')': CloseParen
        of '[': OpenBrack
        of ']': CloseBrack
        of ':': Colon
        of ',': Comma
        of ' ':
          l.next()
          continue

        else:
          if l.cchar != '\n':
            quit &"{l.cchar} isn't an implemented token!"

          else:
            while l.cchar == '\n':
              l.next()

              if l.atEnd:
                l.indentDepth = 0
                result &= Token(typ: Dedent)
                result[^1].setTokenId()
                return

              elif l.cchar != ' ':
                l.indentDepth = 0
                result &= Token(typ: Dedent)
                result[^1].setTokenId()

              else:
                let tkn = l.lexIndent()
                if tkn.typ != Skip:
                  result &= tkn
          continue

      result &= Token(typ: typ, val: $l.cchar)
      result[^1].setTokenId()
      l.next()


let tokens = lexer.lex()
echo tokens

#[
pos = 0
template ctoken: Token = tokens[pos]

proc parseProtoIDPair: ProtoIDPair =
  if ctoken.typ != Num:
    quit &"Got {ctoken.typ} but expected a Num!"
  let protocol = parseInt(ctoken.val)
  inc pos

  if ctoken.typ != Comma:
    quit &"Got {ctoken.typ} but expected a Comma!"
  inc pos

  if ctoken.typ != Num:
    quit &"Got {ctoken.typ} but expected a Num!"
  let packet = parseInt(ctoken.val)
  inc pos

  return (protocol.nl, packet.nl)


proc parsePacket =
  if ctoken.typ != Ident:
    quit &"Got {ctoken.typ} but expected an Ident!"

  var packet = PacketDef(name: Identifier(val: ctoken.val))
  ast.packets.add packet
  inc pos

  if ctoken.typ != OpenParen:
    quit &"Got {ctoken.typ} but expected an OpenParen!"
  inc pos

  var protoIdPairs: seq[ProtoIDPair]
  protoIdPairs.add parseProtoIDPair()

  while ctoken.typ == Arrow:
    inc pos
    protoIdPairs.add parseProtoIDPair()

  packet.protoIdPairs = protoIdPairs

  if ctoken.typ != CloseParen:
    quit &"Got {ctoken.typ} but expected a CloseParen!"
  inc pos

  if ctoken.typ != Arrow:
    quit &"Got {ctoken.typ} but expected an Arrow!"
  inc pos

  if ctoken.typ != Ident:
    quit &"Got {ctoken.typ} but expected an Ident!"
  packet.direction = Identifier(val: ctoken.val)
  inc pos

  if ctoken.typ != Colon:
    quit &"Got {ctoken.typ} but expected a Colon!"
  inc pos

  var hasIndent = false
  if ctoken.typ == Indent:
    hasIndent = true
    inc pos

  template handleDedent =
    if ctoken.typ != Dedent:
      quit &"Got {ctoken.typ} but expected a Dedent!"
    return

  if ctoken.typ == Null:
    inc pos
    if hasIndent:
      handleDedent()
    return

  var order = 0
  while ctoken.typ != Dedent:
    var field = FieldDef()
    packet.fieldDefs.add field
    var protoTypTriad: ProtoTypTriad = (0.nl, NullLiteral(), order)

    if ctoken.typ == OpenParen:
      inc pos

      if ctoken.typ != Num:
        quit &"Got {ctoken.typ} but expected a Num!"
      protoTypTriad.proto = parseInt(ctoken.val).nl
      inc pos

      if ctoken.typ != CloseParen:
        quit &"Got {ctoken.typ} but expected a CloseParen!"
      inc pos

    if ctoken.typ != Ident:
      quit &"Got {ctoken.typ} but expected an Ident!"
    field.name = Identifier(val: ctoken.val)
    inc pos

    if ctoken.typ != Colon:
      quit &"Got {ctoken.typ} but expected a Colon!"
    inc pos

    case ctoken.typ
      of Ident:
        protoTypTriad.typ = Identifier(val: ctoken.val)
        inc pos
        if ctoken.typ == OpenBrack:
          inc pos

          if ctoken.typ != Ident:
            quit &"Got {ctoken.typ} but expected an Ident!"
          protoTypTriad.typ = Index(indexer: protoTypTriad.typ.Identifier,
            indexee: Identifier(val: ctoken.val))
          inc pos
          if ctoken.typ != CloseBrack:
            quit &"Got {ctoken.typ} but expected a CloseBrack!"
          inc pos

      of Null:
        protoTypTriad.typ = NullLiteral() # Marks a field as 'removed'
        inc pos

      else:
        quit &"Got {ctoken.typ} but expected an Ident or Null!"

    field.protoTypTriads.add protoTypTriad
    inc order
  handleDedent()


proc parse =
  while pos <= tokens.len:
    if ctoken.typ != Ident:
      quit &"Got {ctoken.typ} but expected an Ident!"

    parsePacket()
    break


parse()
echo ast.repr
]#