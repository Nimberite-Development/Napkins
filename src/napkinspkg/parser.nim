import std/[
  strformat,
  strutils
]

import ./lexer
import ./parser/types

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

proc parseEnumField(s: var State, order: int): AstNode =
  result = AstNode(kind: EnumFieldDef)

  # Parse the version that the enum field is present in
  var currToken = s.eat()
  if currToken.typ == OpenParen:
    currToken = s.eat()
    if currToken.typ != Num:
      s.report "Expected a number!", UnexpectedToken, currToken

    result.efProto = AstNode(kind: NumLiteral, numVal: currToken.val.parseInt)

    currToken = s.eat()
    if currToken.typ != CloseParen:
      s.report "Expected a close parenthesis!", UnexpectedToken, currToken

    currToken = s.eat()

  else:
    result.efProto = AstNode(kind: NumLiteral, numVal: 0)

  # Parse the value that the enum field is set to
  if currToken.typ == OpenBrace:
    currToken = s.eat()
    if currToken.typ != Num:
      s.report "Expected a number!", UnexpectedToken, currToken

    result.efValue = AstNode(kind: NumLiteral, numVal: currToken.val.parseInt)

    currToken = s.eat()
    if currToken.typ != CloseBrace:
      s.report "Expected a close brace!", UnexpectedToken, currToken

    currToken = s.eat()

  else:
    result.efValue = AstNode(kind: NumLiteral, numVal: 0)

  # Parse the enum identifier
  result.efName = AstNode(kind: Identifier, strVal: currToken.val)
  # Set the order definition for the enum field
  result.efOrder = order

proc parseEnum(s: var State, ident: Token, fieldParsingMode: bool = false): AstNode =
  result = AstNode(kind: EnumTypeDef, eName: AstNode(kind: Identifier, strVal: ident.val))

  let tkOne = s.eat()

  # Parse the enum name
  if tkOne.typ != Ident:
    s.report "Expected an identifier!", UnexpectedToken, tkOne

  # Ensure it's an enum
  if tkOne.val != "Enum":
    s.report "Expected 'Enum'!", ExpectedEnumDefinition, tkOne

  # Parse what type the enum is of (numeric types only, currently)
  var currToken = s.eat()
  if currToken.typ != OpenBrack:
    s.report "Expected an open bracket!", UnexpectedToken, currToken

  currToken = s.eat()
  if currToken.typ != Ident:
    s.report "Expected an identifier!", GenericWithMissingParameters, currToken
  # TODO: Validate this in a later stage or now?
  result.eType = AstNode(kind: Identifier, strVal: currToken.val)

  currToken = s.eat()
  if currToken.typ != CloseBrack:
    s.report "Expected a close bracket!", UnexpectedToken, currToken

  # Different ways of parsing enums depending on location
  currToken = s.eat()
  if not fieldParsingMode:
    if currToken.typ != Colon:
      s.report "Expected a colon!", UnexpectedToken, currToken

  else:
    if currToken.typ != Arrow:
      s.report "Expected an arrow!", UnexpectedToken, currToken

  currToken = s.eat()
  if currToken.typ != Indent:
    s.report "Expected an indented block!", UnexpectedToken, currToken

  var order = 0

  # Parse the enum fields
  while not s.atTkEnd:
    result.eFieldDefs.add s.parseEnumField(order)

    currToken = s.eat()
    case currToken.typ
      of Comma:
        inc order
        continue

      of Dedent:
        break

      else:
        s.report "Expected a comma or a dedent!", UnexpectedToken, currToken

proc parseProtoIDPair(s: var State): ProtoIDPair =
  var currToken = s.eat()
  if currToken.typ != Num:
    s.report "Expected a number!", UnexpectedToken, currToken
  result.proto = AstNode(kind: NumLiteral, numVal: currToken.val.parseInt)

  currToken = s.eat()
  if currToken.typ != Comma:
    s.report "Expected a comma!", UnexpectedToken, currToken

  currToken = s.eat()
  if currToken.typ != Num:
    s.report "Expected a number!", UnexpectedToken, currToken
  result.packet = AstNode(kind: NumLiteral, numVal: currToken.val.parseInt)

proc parsePacket(s: var State, ident: Token): AstNode =
  result = AstNode(kind: PacketDef, pName: AstNode(kind: Identifier, strVal: ident.val))

  var currToken: Token

  # Parse the protocol the packet was introduced in and the corrosponding ID
  while not s.atTkEnd:
    result.protoIdPairs.add s.parseProtoIDPair()

    currToken = s.eat()

    case currToken.typ
      of CloseParen:
        break

      of Arrow:
        continue

      else:
        s.report "Expected a closed parenthesis or an arrow!", UnexpectedToken, currToken

  # Parse the packet body
  currToken = s.eat()
  if currToken.typ != Arrow:
    s.report "Expected an arrow!", UnexpectedToken, currToken

  currToken = s.eat()
  if currToken.typ != Ident:
    s.report "Expected an identifier!", UnexpectedToken, currToken

  if currToken.val.toLower().capitalizeAscii notin ["Client", "Server"]:
    s.report "Expected 'Client' or 'Server'!", InvalidPacketDirection, currToken

  result.direction = AstNode(kind: Identifier, strVal: currToken.val.toLower().capitalizeAscii)

  currToken = s.eat()
  if currToken.typ != Colon:
    s.report "Expected a colon!", UnexpectedToken, currToken

  # TODO: Rest of body parsing

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
        result.nodes.add result.parseEnum(tkOne)

      of OpenParen:
        result.nodes.add result.parsePacket(tkOne)

      else:
        result.report "Expected an arrow or an open parenthesis!", UnexpectedToken, tkTwo


    # Unimplemented codepath
    return
    #result.report "Unimplemented behaviour from this point on.", UnexpectedToken, result.eat()


export types