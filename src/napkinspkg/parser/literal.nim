import std/[
  strformat,
  strutils,
  tables
]

import ../lexer
import ./types

const PrecedenceTable = {
  BitNot: 1,
  ShiftLeft: 2,
  ShiftRight: 2,
  BitAnd: 3,
  BitOr: 4,
}.toTable
template isLeftPrecedence(op: TkType): bool = op != BitNot

proc initAstNode(s: var State, kind: AstKind): AstNode =
  result = AstNode(kind: kind)

  when defined(napkinNodeIds):
    result.id = s.nodeId
    inc s.nodeId

proc ident(s: var State, val: string): AstNode =
  result = s.initAstNode(Identifier)
  result.strVal = val

proc num(s: var State, val: string): AstNode =
  result = s.initAstNode(NumLiteral)
  result.numVal = val.parseInt

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


proc parseIndex(s: var State): AstNode =
  result = s.initAstNode(Index)

  # Parse until CloseBrack
  block outer:
    while not s.atTkEnd:
      var currToken = s.eat()
      case currToken.typ
        of CloseBrack:
          break

        of Ident:
          result.iargs.add s.ident(currToken.val)

        of Num:
          result.iargs.add s.num(currToken.val)

        else:
          s.report "Expected an identifier or a number!", UnexpectedToken, currToken

      while not s.atTkEnd:
        currToken = s.eat()
        case currToken.typ
          of Comma:
            break
          of CloseBrack:
            break outer
          of OpenBrack:
            var res = s.parseIndex()
            if result.iargs[^1].kind != Identifier:
              s.report "Expected an identifier!", IndexOnNonIndexableValue, currToken
            res.itarget = result.iargs[^1]
            result.iargs[^1] = res
          else:
            s.report "Expected a comma, an index or a close bracket!", UnexpectedToken, currToken


proc parseCondition(s: var State): Condition =
  # Parses conditions such as `@ mask & Value`
  var currToken: Token

  var
    opStack = newSeq[Token]()
    outputStack = newSeq[Token]()

  while not s.atTkEnd:
    currToken = s.eat()
    case currToken.typ
      of Ident:
        outputStack.add currToken

      of Num:
        outputStack.add currToken

      of Dot:
        outputStack.add currToken

      of BitNot:
        opStack.add currToken

      of {BitAnd, BitOr, ShiftLeft, ShiftRight}:
        if opStack.len == 0 or opStack[^1].typ != OpenParen or not isLeftPrecedence(opStack[^1].typ):
          opStack.add currToken
        else:
          while opStack.len > 0 and isLeftPrecedence(opStack[^1].typ) and opStack[^1].typ != OpenParen and (
            PrecedenceTable[currToken.typ] >= PrecedenceTable[opStack[^1].typ]
          ):
            outputStack.add opStack.pop()
          opStack.add currToken

      of OpenParen:
        opStack.add currToken

      of CloseParen:
        while opStack.len > 0 and opStack[^1].typ != OpenParen:
          outputStack.add opStack.pop()
        if opStack.len > 0 and opStack[^1].typ == OpenParen:
          discard opStack.pop()

      of Colon:
        break

      else:
        s.report "Unexpected token!", UnexpectedToken, currToken

  for op in opStack:
    if op.typ == OpenParen:
      s.report "Unmatched open parenthesis!", UnexpectedToken, op
    else:
      outputStack.add op

  for i in 0..<outputStack.len:
    if outputStack[i].typ == BitNot:
      swap(outputStack[i], outputStack[i - 1])

  result = Condition(kind: Expr)

  var stack = newSeq[AstNode]()

  var toggle = 0

  template handleDotExpr =
    if toggle == 2:
      var item = s.initAstNode(DotExpr)
      item.dright = stack.pop
      item.dleft = stack.pop

      stack.add item

  for i in outputStack:
    case i.typ
      of Ident:
        stack.add s.ident(i.val)
        handleDotExpr()

      of Num:
        stack.add s.num(i.val)

        if toggle == 2:
          s.report "Expected an identifier to complete the dot expression!", UnexpectedToken, i

      of Dot:
        if stack.len < 2:
          s.report "Expected an identifier or a number!", UnexpectedToken, i
        handleDotExpr()
        toggle = 1

      of BitNot:
        var item = s.initAstNode(FunctionCall)
        item.fcName = s.ident(i.val)
        item.fcArgs.add stack.pop

        stack.add item

        if toggle == 2:
          s.report "Expected an identifier to complete the dot expression!", UnexpectedToken, i

      of {BitAnd, BitOr, ShiftLeft, ShiftRight}:
        var item = s.initAstNode(FunctionCall)
        item.fcName = s.ident(i.val)
        item.fcArgs.add stack.pop
        item.fcArgs.add stack.pop
        stack.add item

        if toggle == 2:
          s.report "Expected an identifier to complete the dot expression!", UnexpectedToken, i

      else:
        s.report "Unexpected token!", UnexpectedToken, i

    if toggle == 2:
      toggle = 0

    elif toggle == 1:
      toggle = 2

    if stack.len > 1:
      swap stack[^2], stack[^1]

  result.exp = stack.pop

proc parsePacketOrStructField(s: var State, packet, field: var AstNode, conditions: seq[Condition],
    order: var int, isPacket: static bool): FieldParseResult =
  # Begin parsing the field
  var currToken = s.eat()

  template name(i: AstNode): AstNode = (when isPacket: i.pfName else: i.sfName)
  template fieldDefs(i: var AstNode): var seq[AstNode] = (when isPacket: i.pFieldDefs else: i.sFieldDefs)
  template typ(i: AstNode): AstNode = (when isPacket: i.pfType else: i.sfType)
  template `typ=`(a: var AstNode, b: AstNode) = (when isPacket: a.pfType = b else: a.sfType = b)
  template fieldDef(s: var State, conditions: seq[Condition], order: var int): AstNode =
    var res: AstNode
    when isPacket:
      res = s.initAstNode(PacketFieldDef)
      res.pfConditions = conditions
      res.pfOrder = order

    else:
      res = s.initAstNode(StructFieldDef)
      res.sfConditions = conditions
      res.sfOrder = order

    res
  template `proto=`(a: var AstNode, b: AstNode) = (when isPacket: a.pfProto = b else: a.sfProto = b)
  template `name=`(a: var AstNode, b: AstNode) = (when isPacket: a.pfName = b else: a.sfName = b)

  if currToken.typ == At:
    # Handle if statements here (this is what `@` represents)
    var
      newConditions = conditions & s.parseCondition()
      newField: AstNode = nil

    currToken = s.eat()
    if currToken.typ != Indent:
      s.report "Expected an indented block!", UnexpectedToken, currToken

    while not s.atTkEnd:
      let res = s.parsePacketOrStructField(packet, newField, newConditions, order, isPacket)

      case res
        of AddNewField:
          packet.fieldDefs.add newField
          inc order
        of FoundDedent: break
        of {HandledConditionalField, HandledIndex}: discard

    return HandledConditionalField

  elif field != nil:
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
            newConditions.add Condition(kind: EnumComparison, targetField: field.name,
              enumValue: s.ident(currToken.val))

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
          let res = s.parsePacketOrStructField(packet, newField, newConditions, order, isPacket)

          case res
            of AddNewField:
              packet.fieldDefs.add newField
              inc order
            of FoundDedent: break
            of {HandledConditionalField, HandledIndex}: discard

      return HandledConditionalField

    elif currToken.typ == OpenBrack:
      var res = s.parseIndex()

      res.itarget = field.typ
      field.typ = res

      return HandledIndex

    else:
      field = s.fieldDef(conditions, order)

  else:
    field = s.fieldDef(conditions, order)

  # Handle dedent
  if currToken.typ == Dedent:
    return FoundDedent

  # Parse protocol version field is present in
  if currToken.typ == OpenParen:
    currToken = s.eat()
    if currToken.typ != Num:
      s.report "Expected a number!", UnexpectedToken, currToken

    field.proto = s.num(currToken.val)

    currToken = s.eat()
    if currToken.typ != CloseParen:
      s.report "Expected a close parenthesis!", UnexpectedToken, currToken
    
    currToken = s.eat()

  else:
    field.proto = s.initAstNode(NumLiteral)

  # Parse field name
  if currToken.typ != Ident:
    s.report "Expected an identifier!", UnexpectedToken, currToken
  field.name = s.ident(currToken.val)

  currToken = s.eat()
  if currToken.typ != Colon:
    s.report "Expected a colon!", UnexpectedToken, currToken

  # Parse field type
  currToken = s.eat()
  case currToken.typ
    of Ident:
      field.typ = s.ident(currToken.val)

    of Null:
      field.typ = s.initAstNode(NullLiteral)

    else:
      s.report "Expected an identifier or a null literal!", UnexpectedToken, currToken

  return AddNewField


proc parseEnumField(s: var State, order: int): AstNode =
  result = AstNode(kind: EnumFieldDef)

  # Parse the version that the enum field is present in
  var currToken = s.eat()
  if currToken.typ == OpenParen:
    currToken = s.eat()
    if currToken.typ != Num:
      s.report "Expected a number!", UnexpectedToken, currToken

    result.efProto = s.num(currToken.val)

    currToken = s.eat()
    if currToken.typ != CloseParen:
      s.report "Expected a close parenthesis!", UnexpectedToken, currToken

    currToken = s.eat()

  else:
    result.efProto = s.initAstNode(NumLiteral)

  # Parse the value that the enum field is set to
  if currToken.typ == OpenBrace:
    currToken = s.eat()
    if currToken.typ != Num:
      s.report "Expected a number!", UnexpectedToken, currToken

    result.efValue = s.num(currToken.val)

    currToken = s.eat()
    if currToken.typ != CloseBrace:
      s.report "Expected a close brace!", UnexpectedToken, currToken

    currToken = s.eat()

  else:
    result.efValue = s.initAstNode(NumLiteral)

  # Parse the enum identifier
  result.efName = s.ident(currToken.val)
  # Set the order definition for the enum field
  result.efOrder = order


proc parseEnum(s: var State, ident: Token): AstNode =
  result = s.initAstNode(EnumTypeDef)
  result.eName = s.ident(ident.val)

  var currToken = s.eat()
  if currToken.typ != OpenBrack:
    s.report "Expected an open bracket!", UnexpectedToken, currToken

  # Parse what type the enum is of (numeric types only, currently)
  currToken = s.eat()
  if currToken.typ != Ident:
    s.report "Expected an identifier!", GenericWithMissingParameters, currToken

  result.eType = s.ident(currToken.val)

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


proc parseStruct(s: var State, ident: Token): AstNode =
  # Struct parsing is very similar to packet parsing so logic can be reused
  result = s.initAstNode(StructDef)
  result.sName = s.ident(ident.val)

  var currToken = s.eat()

  case currToken.typ
    of OpenBrack:
      while not s.atTkEnd:
        currToken = s.eat()
        case currToken.typ
          of CloseBrack:
            currToken = s.eat()
            break

          of Comma:
            continue

          of Ident:
            var name = s.ident(currToken.val)

            currToken = s.eat()
            if currToken.typ != Colon:
              s.report "Expected a colon!", UnexpectedToken, currToken

            currToken = s.eat()
            if currToken.typ != Ident:
              s.report "Expected an identifier!", GenericWithMissingParameters, currToken

            result.sArgs.add (s.ident(currToken.val), name)

          else:
            s.report "Expected an identifier or a close bracket!", UnexpectedToken, currToken

    of Colon:
      discard

    else:
      s.report "Expected an open bracket or a colon!", UnexpectedToken, currToken

  currToken = s.eat()
  if currToken.typ != Indent:
    s.report "Expected an indented block!", UnexpectedToken, currToken

  var
    field: AstNode = nil
    order = 0

  let conditions = newSeq[Condition]()
  # Begin field parsing
  while not s.atTkEnd:
    let res = s.parsePacketOrStructField(result, field, conditions, order, false)

    case res
      of AddNewField:
        result.sFieldDefs.add field
        inc order
      of FoundDedent: break
      of {HandledConditionalField, HandledIndex}: discard


proc parseEnumOrStruct(s: var State, ident: Token): AstNode =
  var currToken = s.eat()

  # Parse the enum name
  if currToken.typ != Ident:
    s.report "Expected an identifier!", UnexpectedToken, currToken

  # Ensure it's an enum
  if currToken.val == "Enum":
    result = s.parseEnum(ident)

  elif currToken.val == "Struct":
    result = s.parseStruct(ident)

  else:
    s.report "Expected an enum or struct!", UnexpectedToken, currToken


proc parseProtoIDPair(s: var State): ProtoIDPair =
  var currToken = s.eat()
  if currToken.typ != Num:
    s.report "Expected a number!", UnexpectedToken, currToken
  result.proto = s.num(currToken.val)

  currToken = s.eat()
  if currToken.typ != Comma:
    s.report "Expected a comma!", UnexpectedToken, currToken

  currToken = s.eat()
  if currToken.typ != Num:
    s.report "Expected a number!", UnexpectedToken, currToken
  result.packet = s.num(currToken.val)


proc parsePacket(s: var State, ident: Token): AstNode =
  result = s.initAstNode(PacketDef)
  result.pName = s.ident(ident.val)

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

  result.pDirection = if currToken.val == "Client": Client else: Server

  currToken = s.eat()
  if currToken.typ != Colon:
    s.report "Expected a colon!", UnexpectedToken, currToken

  # Parse block
  currToken = s.eat()
  case currToken.typ
    of Null:
      return

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
    let res = s.parsePacketOrStructField(result, field, conditions, order, true)

    case res
      of AddNewField:
        result.pFieldDefs.add field
        inc order
      of FoundDedent: break
      of {HandledConditionalField, HandledIndex}: discard

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
        result.nodes.add result.parseEnumOrStruct(tkOne)

      of OpenParen:
        result.nodes.add result.parsePacket(tkOne)

      else:
        result.report "Expected an arrow or an open parenthesis!", UnexpectedToken, tkTwo

export types