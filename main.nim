import std/[strformat, strutils]

type
  TkType = enum
    Ident, Num, Null, OpenParen, CloseParen, OpenBrack,
    CloseBrack, Comma, Arrow, Colon, Indent, Dedent

  Token = object
    typ: TkType
    val: string

  ProtoIDPair = tuple[proto: NumLiteral, packet: NumLiteral]
  ProtoTypTriad = tuple[proto: NumLiteral, typ: Expression, order: int]

  AstNode* = ref object of RootObj

  Expression* = ref object of AstNode

  Identifier* = ref object of Expression
    val*: string

  Index* = ref object of Expression
    indexer*, indexee*: Identifier

  NumLiteral* = ref object of Expression
    val*: int

  NullLiteral* = ref object of Expression

  FieldDef* = ref object of AstNode
    name*: Identifier
    protoTypTriads: seq[ProtoTypTriad]

  PacketDefs* = ref object of AstNode
    packets*: seq[PacketDef]

  PacketDef* = ref object of AstNode
    name*: Identifier
    protoIdPairs*: seq[ProtoIDPair]
    direction*: Identifier # Must be 'Client' or 'Server'
    fieldDefs*: seq[FieldDef]

template nl(i: int): NumLiteral = NumLiteral(val: i)

proc `$`*(token: Token, depth: int = 1): string =
  let indent = repeat("  ", depth)
  result &= &"Token(\n{indent}typ: {token.typ},\n"
  result &= &"{indent}val: "

  result.addQuoted(token.val)
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

var code = readFile("test.nmp")
var tokens = newSeq[Token]()
var ast = PacketDefs()
var pos: int
var indentDepth = 0

template cchar: char = code[pos]

proc lexIndent =
  var lexeme: string

  while cchar == ' ':
    lexeme &= cchar
    inc pos

  if indentDepth == 0:
    indentDepth = lexeme.len
    tokens.add Token(typ: Indent, val: lexeme)

  else:
    if indentDepth != lexeme.len:
      echo "The spacing isn't consistent, expected ", $indentDepth, "spaces!"


proc lex =
  while pos < code.len:
    if cchar.isAlphaAscii:
      var lexeme: string
      while cchar.isAlphaNumeric:
        lexeme &= cchar
        inc pos

      tokens.add Token(typ:
        (if lexeme != "Null": Ident else: Null), val: lexeme)
      continue

    elif cchar.isDigit or cchar == '-':
      var lexeme = $cchar
      inc pos

      if lexeme[0] == '-':
        if cchar == '>':
          tokens.add Token(typ: Arrow, val: lexeme & cchar)
          inc pos
          continue
        if not cchar.isDigit:
          echo lexeme & cchar, " isn't a valid number!"
          continue

      var isHex = false

      if cchar == 'x':
        isHex = true
        lexeme &= 'x'
        inc pos

      template validChars: string =
        if isHex:
          "0123456789ABCDEFabcdef"
        else:
          "0123456789"

      while cchar in validChars:
        lexeme &= cchar
        inc pos

      if isHex:
        lexeme = $parseHexInt(lexeme)

      tokens.add Token(typ: Num, val: lexeme)
      continue

    else:
      var typ = case cchar
        of '(': OpenParen
        of ')': CloseParen
        of '[': OpenBrack
        of ']': CloseBrack
        of ':': Colon
        of ',': Comma
        of ' ':
          inc pos
          continue

        else:
          if cchar != '\n':
            echo cchar, " isn't an implemented token!"
            inc pos

          else:
            while cchar == '\n':
              inc pos
              if pos >= code.len:
                indentDepth = 0
                tokens.add Token(typ: Dedent)
                break

              elif cchar != ' ':
                indentDepth = 0
                tokens.add Token(typ: Dedent)

              else: lexIndent()

          continue

      tokens.add Token(typ: typ, val: $cchar)
      inc pos


lex()

echo tokens

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
