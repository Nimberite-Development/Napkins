import std/[strformat, tables]

import ./napkinspkg/combinator

const
  CodeA = "+2 * 1"
  OperatorChars = "-+*/^%"
  PrecedenceTable: Table[string, tuple[unary, infix: int]] = {
    "+": (1, 3), "-": (1, 3),
    "*": (0, 2), "/": (0, 2),
    "^": (0, 3), "%": (0, 3)
  }.toTable

let
  skipWhitespace = many(textParser(" ")).label("skipWhitespace")
  literalParser = (numberParser or identifierParser).label("literalParser")

parser(operatorParser, state, input, position):
  ## Parses an operator character.
  if position >= input.len: return ParseResult(state: state, isOk: false, error: ParseErrorTree(isBranch: false, reason: "End of Input!"))

  if input[position] notin OperatorChars:
    return ParseResult(state: state, isOk: false, error: ParseErrorTree(isBranch: false, reason: &"Expected an operator but found `{input[position]}`!"))

  var offset = position

  while offset < input.len and input[offset] in OperatorChars:
    inc offset

  result = ParseResult(state: state, isOk: true, position: offset)
  let idx = result.state.add AstNode(kind: anOperator, lineInfo: result.state.lineInfoAt(position), identVal: input[position..<offset].toString)
  result.nodes.add idx


var expPrecParserCache: Table[int, Parser]

proc expParserWithPrec*(prec: int): Parser

proc expressionParserWithPrec(state: sink State, input: openArray[char], position: Natural, prevPrec: int): ParseResult =
  ## Parses an expression and uses the given precedence to decide whether to continue parsing or exit early.
  ## Parses an expression.
  let checkpoint = state.checkpoint()
  result = ParseResult(state: state, isOk: true, position: position)

  let exprParser = (literalParser or operatorParser or between(textParser("("), textParser(")"), expParserWithPrec(0))
    ) & skipWhitespace
  result = exprParser(result.state, input, position)

  template getNode(): tuple[node: AstNode, idx: AstNodeIndex] =
    if result.state.len < 1:
      result.state.restore(checkpoint)
      return ParseResult(state: result.state, isOk: false, error: ParseErrorTree(isBranch: false, reason: "Expected an expression!"))

    if result.isErr:
      result.state.restore(checkpoint)
      return ParseResult(state: result.state, isOk: false, error: ParseErrorTree(isBranch: true, source: parserNameTable[exprParser],
        children: @[ParseErrorTree(isBranch: false, reason: "Expected an expression!"), result.error]))

    if result.nodes.len != 1:
      result.state.restore(checkpoint)
      return ParseResult(state: result.state, isOk: false, error: ParseErrorTree(isBranch: false, reason: "Expected a single node in expr parsing!"))

    (result.state[result.nodes[^1]], result.nodes[^1])

  var node = getNode()

  var res: seq[AstNode]

  if node.node.kind == anOperator:
    if node.node.identVal notin PrecedenceTable: return ParseResult(state: result.state, isOk: false,
      error: ParseErrorTree(isBranch: false, reason: &"Unexpected operator `{node.node.identVal}`!"))

    let nextPrec = PrecedenceTable[node.node.identVal].unary

    if nextPrec == 0: return ParseResult(state: result.state, isOk: false, error: ParseErrorTree(isBranch: false,
      reason: &"Unexpected unary operator `{node.node.identVal}`!"))

    res &= AstNode(kind: anPrefix, lineInfo: result.state.lineInfoAt(result.position), callee: ~(res.len + 1))

    if nextPrec > prevPrec:
      result = expParserWithPrec(nextPrec)(result.state, input, result.position)
      node = getNode()
      res[0].args.add node.idx
    else:
      res &= node.node

    if result.isErr: return result

    let idx = result.state.add res[0]
    result.nodes = @[idx]

  else:
    # Parse infixes
    res.add node.node

    result = exprParser(result.state, input, result.position)
    node = getNode()

    if node.node.kind != anOperator: return ParseResult(state: result.state, isOk: false, error: ParseErrorTree(isBranch: false,
      reason: &"Expected an operator but found `{node.node.kind}`!"))

    if node.node.identVal notin PrecedenceTable: return ParseResult(state: result.state, isOk: false, error: ParseErrorTree(isBranch: false,
      reason: &"Unexpected operator `{node.node.identVal}`!"))

    let nextPrec = PrecedenceTable[node.node.identVal].infix

    if nextPrec == 0: return ParseResult(state: result.state, isOk: false, error: ParseErrorTree(isBranch: false,
      reason: &"Unexpected infix operator `{node.node.identVal}`!"))

    res &= AstNode(kind: anInfix, lineInfo: result.state.lineInfoAt(result.position), callee: ~(res.len + 1))
    res[1].args.add ~0

    var c = false

    var
      opNode: tuple[node: AstNode, idx: AstNodeIndex]
      pos: Natural

    while true:
      result = exprParser(result.state, input, result.position)

      if result.isErr:
        if c: break
        else: return ParseResult(state: result.state, isOk: false, error: ParseErrorTree(isBranch: true,
          source: parserNameTable[exprParser], children: @[result.error]))

      opNode = getNode()
      if opNode.node.kind != anOperator:
        return ParseResult(state: result.state, isOk: false, error: ParseErrorTree(isBranch: false,
          reason: &"Expected an operator but found `{opNode.node.kind}`!"))

      result = expParserWithPrec(nextPrec)(result.state, input, result.position)

      if result.isErr:
        return ParseResult(state: result.state, isOk: false, error: ParseErrorTree(isBranch: true,
          source: parserNameTable[expParserWithPrec(nextPrec)], children: @[result.error]))

      if not c: c = true

    result = ParseResult(state: result.state, isOk: true, position: result.position, nodes: @[opNode.idx])

    let idx = result.state.add res
    result.nodes = @[idx[1]]

proc expParserWithPrec(prec: int): Parser =
  ## Generates a parser that parses an expression and uses the given precedence to decide whether to
  ## continue parsing or exit early.
  if prec in expPrecParserCache: return expPrecParserCache[prec]
  proc expParserWithPrecInternal(state: sink State, input: openArray[char], position: Natural): ParseResult =
    ## Parses an expression and uses the given precedence to decide whether to continue parsing or exit early.
    result = expressionParserWithPrec(state, input, position, prec)

  parserNameTable[expParserWithPrecInternal] = "expParserWithPrec(" & $prec & ")"
  expPrecParserCache[prec] = expParserWithPrecInternal
  expParserWithPrecInternal

let expressionParser = expParserWithPrec(0)


for code in [CodeA]:
  let res = label(newlineLocator & expressionParser, ":root")(State(), code, 0)

  if res.isOk:
    echo res
  else:
    echo res.error.dumpTree