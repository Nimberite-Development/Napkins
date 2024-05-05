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

proc parseEnum(s: var State, ident: Token): AstNode =
  result = AstNode(kind: EnumTypeDef, eName: AstNode(kind: Identifier, strVal: ident.val))

  var currToken = s.eat()

  # Parse the enum name
  if currToken.typ != Ident:
    s.report "Expected an identifier!", UnexpectedToken, currToken

  # Ensure it's an enum
  if currToken.val != "Enum":
    s.report "Expected 'Enum'!", ExpectedEnumDefinition, currToken

  # Parse what type the enum is of (numeric types only, currently)
  currToken = s.eat()
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
  if currToken.typ != Colon:
    s.report "Expected a colon!", UnexpectedToken, currToken

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

proc parsePacketField(s: var State, packet, field: var AstNode, conditions: seq[Condition],
    order: var int): FieldParseResult =
  # Begin parsing the field
  var currToken = s.eat()

  if field != nil:
    if currToken.typ == Arrow:
      currToken = s.eat()
      if currToken.typ != Indent:
        s.report "Expected an indented block!", UnexpectedToken, currToken

      var newField: AstNode = nil

      while not s.atTkEnd:
        var newConditions = conditions
        currToken = s.eat()
        case currToken.typ
          of Ident:
            newConditions.add Condition(kind: EnumComparison, targetField: field,
              enumValue: AstNode(kind: Identifier, strVal: currToken.val))

          of Dedent:
            break

          else:
            s.report "Expected an identifier or a dedent!", UnexpectedToken, currToken

        currToken = s.eat()
        if currToken.typ != Colon:
          s.report "Expected a colon!", UnexpectedToken, currToken

        currToken = s.eat()
        case currToken.typ
          of Indent:
            discard
          of Null:
            continue
          else:
            s.report "Expected an indented block or a null literal!", UnexpectedToken, currToken

        while not s.atTkEnd:
          let res = s.parsePacketField(packet, newField, newConditions, order)

          case res
            of AddNewField:
              packet.pFieldDefs.add newField
              inc order
            of FoundDedent: break
            of HandledConditionalField: discard

      return HandledConditionalField

    else:
      field = AstNode(kind: PacketFieldDef, pfConditions: conditions)

  else:
    field = AstNode(kind: PacketFieldDef, pfConditions: conditions)

  # Handle dedent
  if currToken.typ == Dedent:
    return FoundDedent

  # Parse protocol version field is present in
  if currToken.typ == OpenParen:
    currToken = s.eat()
    if currToken.typ != Num:
      s.report "Expected a number!", UnexpectedToken, currToken

    field.pfProto = AstNode(kind: NumLiteral, numVal: currToken.val.parseInt)

    currToken = s.eat()
    if currToken.typ != CloseParen:
      s.report "Expected a close parenthesis!", UnexpectedToken, currToken
    
    currToken = s.eat()

  else:
    field.pfProto = AstNode(kind: NumLiteral, numVal: 0)

  # Parse field name
  var enumNameTk: Token
  if currToken.typ != Ident:
    s.report "Expected an identifier!", UnexpectedToken, currToken
  field.pfName = AstNode(kind: Identifier, strVal: currToken.val)
  enumNameTk = currToken

  currToken = s.eat()
  if currToken.typ != Colon:
    s.report "Expected a colon!", UnexpectedToken, currToken

  # Parse field type
  currToken = s.eat()
  case currToken.typ
    of Ident:
      let fieldName = currToken.val
      field.pfType = AstNode(kind: Identifier, strVal: fieldName)

    of Null:
      field.pfType = AstNode(kind: NullLiteral)

    else:
      s.report "Expected an identifier or a null literal!", UnexpectedToken, currToken

  return AddNewField

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

  if currToken.val notin ["Client", "Server"]:
    s.report &"Expected 'Client' or 'Server', got '{currToken.val}'!", InvalidPacketDirection, currToken

  result.direction = AstNode(kind: Identifier, strVal: currToken.val.toLower().capitalizeAscii)

  currToken = s.eat()
  if currToken.typ != Colon:
    s.report "Expected a colon!", UnexpectedToken, currToken

  # Parse block
  currToken = s.eat()
  case currToken.typ
    of Null:
      return result

    of Indent:
      discard

    else:
      s.report "Expected an indented block or a null literal!", UnexpectedToken, currToken

  var
    field: AstNode = nil
    order = 0
  let conditions = newSeq[Condition]()
  # Begin field parsing
  while not s.atTkEnd:
    let res = s.parsePacketField(result, field, conditions, order)

    case res
      of AddNewField:
        result.pFieldDefs.add field
        inc order
      of FoundDedent: return
      of HandledConditionalField: discard

proc parse*(tokens: seq[Token], fileName: string = "<string>",
    throwOnError: bool = true): State =
  result = State(tokens: tokens, fileName: fileName, throwOnError: throwOnError)
  # Use lexer object instead? Would provide more info with less redundancy

  while not result.atTkEnd:
    let tkOne = result.eat()

    case tkOne.typ
      of Ident:
        discard
      of EndOfFile:
        break
      else:
        result.report "Expected an identifier!", UnexpectedToken, tkOne

    let tkTwo = result.eat()

    case tkTwo.typ
      of Arrow:
        result.nodes.add result.parseEnum(tkOne)

      of OpenParen:
        result.nodes.add result.parsePacket(tkOne)

      else:
        result.report "Expected an arrow or an open parenthesis!", UnexpectedToken, tkTwo

    #result.report "Unimplemented behaviour from this point on.", UnexpectedToken, result.eat()


export types