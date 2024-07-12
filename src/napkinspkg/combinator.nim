import std/[strformat, sequtils, strutils, tables]

type
  NapkinsParserDefect* = object of Defect
  ParserNilDefect* = object of NapkinsParserDefect
  ParserUnnamedDefect* = object of NapkinsParserDefect
  ParserCombinationDefect* = object of NapkinsParserDefect

  AstNodeKind* = enum
    anInt, anIdentifier, anPrefix, anInfix, anPostfix, anFunctionCall

  AstNodeIndex* = distinct int

  LineInfo* = object
    line*: Natural
    column*: Natural

  AstNode* = object
    lineInfo*: LineInfo
    case kind*: AstNodeKind
    of anInt:
      intVal*: int
    of anIdentifier:
      identVal*: string
    of {anPrefix, anInfix, anPostfix, anFunctionCall}:
      callee*: AstNodeIndex
      args*: seq[AstNodeIndex]

  State* = object
    lineNumberTable: Table[Natural, Natural]
    ast*: seq[AstNode]

  ParseErrorTree* {.acyclic.} = object
    case isBranch*: bool
    of true:
      source*: string
      children*: seq[ParseErrorTree]
    of false:
      reason*: string
      lineInfo*: LineInfo

  ParseResult* = object
    state*: State
    case isOk*: bool
    of true:
      position*: Natural
      nodes*: seq[AstNodeIndex]
    of false:
      error*: ParseErrorTree

  Parser* = proc(state: sink State, input: openArray[char], position: Natural): ParseResult

template `~`*(idx: int): AstNodeIndex = AstNodeIndex(idx)
template `$`*(idx: AstNodeIndex): string = '~' & $idx.int

template isErr*(result: ParseResult): bool = not result.isOk

proc `=copy`*(x: var State, y: State) {.error.}


proc dumpTree*(tree: ParseErrorTree): string =
  var res: seq[string]
  var stack: seq[tuple[node: ParseErrorTree, depth: int, isLast: bool]]

  stack.add (tree, 0, true)

  while stack.len > 0:
    let (node, depth, isLast) = stack.pop()
    var prefix = ""

    if depth > 0:
      for i in 0..<(depth - 1):
        prefix &= (if stack.anyIt(it.depth == i and not it.isLast): "│   " else: "    ")
      prefix &= (if isLast: "└── " else: "├── ")

    if node.isBranch:
      res.add(prefix & "Source: " & node.source)
      for i in 0..<node.children.len:
        let child = node.children[node.children.len - 1 - i]
        stack.add (child, depth + 1, i == 0)
    else:
      res.add(prefix & "Error: " & node.reason)

  return res.join("\n")

proc `init`*(T: typedesc[LineInfo], line, column: Natural): T = T(line: line, column: column)

proc lineInfoAt*(state: State, position: Natural): LineInfo =
  ## Returns the line and column of the given position in the given state.
  if state.lineNumberTable.len < 1: return LineInfo.init(Natural(1), Natural(position + 1))

  var lastPos: Natural

  for pos in state.lineNumberTable.keys:
    if position - pos <= 0: break
    lastPos = pos

  result.line = state.lineNumberTable[lastPos]
  result.column = position - lastPos + 1


var parserNameTable: Table[Parser, string]
var textParserCache: Table[string, Parser]
var sequencedParserCache: Table[seq[Parser], Parser]
var choiceParserCache: Table[seq[Parser], Parser]
var manyParserCache: Table[Parser, Parser]
var many1ParserCache: Table[Parser, Parser]

proc toString(chars: openArray[char]): string =
  ## Converts an `openArray[char]` into a string, copies the memory.
  result.setLen chars.len
  for i in 0..<chars.len: result[i] = chars[i]


template label*(parser: Parser, name: string): Parser =
  ## Renames the given parser.
  parserNameTable[parser] = name
  parser


template parser*(ident, state, input, position: untyped{nkIdent}, body: untyped) =
  ## Creates a parser and adds it to the name table.
  proc ident*(state: sink State, input: openArray[char], position: Natural): ParseResult {.inject.} = body

  parserNameTable[ident] = ident.astToStr


parser(newlineLocator, state, input, position):
  ## Finds all newlines in the given input and adds them to the state as line numbers. This is required for
  ## nice error reporting.
  result = ParseResult(state: state, isOk: true, position: position)

  var line = 1

  result.state.lineNumberTable[position] = line

  for i in position..<input.len:
    inc line
    if input[i] != '\n': continue
    result.state.lineNumberTable[i] = line


proc `&`*(x, y: Parser): Parser =
  ## Concatenates two parsers.
  if x == nil or y == nil:
    raise newException(ParserNilDefect, "Parsers cannot be nil!")

  if x notin parserNameTable or y notin parserNameTable:
    raise newException(ParserUnnamedDefect, "Parsers must be named before concatenation!")

  let cachedParser = sequencedParserCache.getOrDefault(@[x, y], nil)
  if cachedParser != nil: return cachedParser

  proc combinationConcat(state: sink State, input: openArray[char], position: Natural): ParseResult =
    ## Concatenation of two parsers.
    result = x(state, input, position)
    if result.isErr:
      result.error = ParseErrorTree(isBranch: true, source: parserNameTable[combinationConcat] & ":Concat:Left",
        children: @[result.error])
      return result

    let nodes = result.nodes

    result = y(result.state, input, result.position)
    if result.isErr:
      result.error = ParseErrorTree(isBranch: true, source: parserNameTable[combinationConcat] & ":Concat:Right",
        children: @[result.error])
      return

    result.nodes = nodes & result.nodes

  sequencedParserCache[@[x, y]] = combinationConcat

  if x == y:
    parserNameTable[combinationConcat] = "combinationConcat[(" & parserNameTable[x] & " * 2)]"
  else:
    parserNameTable[combinationConcat] = "combinationConcat[" & parserNameTable[x] & ", " & parserNameTable[y] & "]"
  return combinationConcat


proc sequenceOf*(parsers: varargs[Parser]): Parser =
  ## Runs the parsers one after the other, if  the previous is successful.
  for p in parsers:
    if p == nil:
      raise newException(ParserNilDefect, "Parser cannot be nil!")
    elif not parserNameTable.hasKey(p):
      raise newException(ParserUnnamedDefect, "Parser must be named before concatenation!")

  let parsers = @parsers

  let cachedParser = sequencedParserCache.getOrDefault(parsers, nil)
  if cachedParser != nil: return cachedParser

  let length = parsers.len

  if length < 1:
    raise newException(ParserCombinationDefect, "Choice of requires at least one parser!")

  proc combinationSequence(state: sink State, input: openArray[char], position: Natural): ParseResult =
    ## Runs the parsers one after the other, if  the previous is successful.
    result = parsers[0](state, input, position)

    if result.isErr:
      result.error = ParseErrorTree(isBranch: true, source: parserNameTable[combinationSequence] & ":Seq:0",
        children: @[result.error])
      return

    if length < 2: return

    var nodes = result.nodes

    for i in 1..<length:
      result = parsers[i](result.state, input, result.position)
      if result.isErr:
        result.error = ParseErrorTree(isBranch: true, source: parserNameTable[combinationSequence] & &":Seq:{i}",
          children: @[result.error])
        return
      else:
        nodes &= result.nodes

    result.nodes = nodes

  sequencedParserCache[parsers] = combinationSequence

  parserNameTable[combinationSequence] = "combinationSequence["
  for p in parsers:
    parserNameTable[combinationSequence] &= parserNameTable[p]
    parserNameTable[combinationSequence].add ", "
  parserNameTable[combinationSequence].setLen parserNameTable[combinationSequence].len - 2
  parserNameTable[combinationSequence].add "]"
  return combinationSequence


proc `or`*(x, y: Parser): Parser =
  ## Generates a parser that runs `x`, if it fails, runs `y`.
  if x == nil or y == nil:
    raise newException(ParserNilDefect, "Parsers cannot be nil!")

  if x notin parserNameTable or y notin parserNameTable:
    raise newException(ParserUnnamedDefect, "Parsers must be named before concatenation!")

  let cachedParser = choiceParserCache.getOrDefault(@[x, y], nil)
  if cachedParser != nil: return cachedParser

  proc combinationOr(state: sink State, input: openArray[char], position: Natural): ParseResult =
    ## Runs `x`, if it fails, runs `y`.
    result = x(state, input, position)
    if result.isOk: return
    var errMsg = ParseErrorTree(isBranch: true, source: parserNameTable[combinationOr], children: @[result.error])

    result = y(result.state, input, position)
    if result.isErr:
      errMsg.children.add result.error
      result.error = errMsg

  choiceParserCache[@[x, y]] = combinationOr

  parserNameTable[combinationOr] = "combinationOr[" & parserNameTable[x] & ", " & parserNameTable[y] & "]"
  return combinationOr


proc choicesOf*(parsers: varargs[Parser]): Parser =
  ## Generates a parser that runs the parsers one after the other, if the previous parser fails,
  ## otherwise it returns the successful result.
  for p in parsers:
    if p == nil:
      raise newException(ParserNilDefect, "Parser cannot be nil!")
    elif not parserNameTable.hasKey(p):
      raise newException(ParserUnnamedDefect, "Parser must be named before concatenation!")

  let parsers = @parsers

  let cachedParser = choiceParserCache.getOrDefault(parsers, nil)
  if cachedParser != nil: return cachedParser

  let length = parsers.len

  if length < 1:
    raise newException(ParserCombinationDefect, "Choice of requires at least one parser!")

  proc combinationChoice(state: sink State, input: openArray[char], position: Natural): ParseResult =
    ## Runs the parsers one after the other, if the previous parser fails, otherwise it returns the successful result.
    result = parsers[0](state, input, position)
    if result.isOk: return
    var errMsg = ParseErrorTree(isBranch: true, source: parserNameTable[combinationChoice], children: @[result.error])

    if length < 2:
      result.error = errMsg
      return

    for i in 1..<length:
      result = parsers[i](result.state, input, position)
      if result.isOk: return
      errMsg.children.add result.error

    result.error = errMsg

  choiceParserCache[parsers] = combinationChoice

  parserNameTable[combinationChoice] = "combinationChoice["
  for p in parsers:
    parserNameTable[combinationChoice] &= parserNameTable[p]
    parserNameTable[combinationChoice].add ", "
  parserNameTable[combinationChoice].setLen parserNameTable[combinationChoice].len - 2
  parserNameTable[combinationChoice].add "]"
  return combinationChoice


proc many*(parser: Parser): Parser =
  ## Generates a parser that runs the given parser zero or more times.
  if parser == nil:
    raise newException(ParserNilDefect, "Parser cannot be nil!")
  elif not parserNameTable.hasKey(parser):
    raise newException(ParserUnnamedDefect, "Parser must be named before concatenation!")

  let cachedParser = manyParserCache.getOrDefault(parser, nil)
  if cachedParser != nil: return cachedParser

  proc combinationMany(state: sink State, input: openArray[char], position: Natural): ParseResult =
    ## Runs the given parser zero or more times.
    result = ParseResult(isOk: true, state: state, position: position)
    var
      lastOkPosition = position
      nodes: seq[AstNodeIndex]

    while result.isOk:
      lastOkPosition = result.position
      nodes &= result.nodes
      result = parser(result.state, input, lastOkPosition)

    return ParseResult(isOk: true, state: result.state, position: lastOkPosition, nodes: nodes)

  manyParserCache[parser] = combinationMany
  parserNameTable[combinationMany] = "combinationMany[" & parserNameTable[parser] & "]"
  return combinationMany


proc many1*(parser: Parser): Parser =
  ## Generates a parser that runs the given parser one or more times.
  if parser == nil:
    raise newException(ParserNilDefect, "Parser cannot be nil!")
  elif not parserNameTable.hasKey(parser):
    raise newException(ParserUnnamedDefect, "Parser must be named before concatenation!")

  let cachedParser = many1ParserCache.getOrDefault(parser, nil)
  if cachedParser != nil: return cachedParser

  let combinationMany1 = parser & many(parser)

  for (k, v) in sequencedParserCache.pairs:
    if v == combinationMany1:
      sequencedParserCache.del k
      break

  parserNameTable[combinationMany1] = "combinationMany1[" & parserNameTable[parser] & "]"
  many1ParserCache[parser] = combinationMany1

  return combinationMany1


template chain*(parser: Parser, state, input, position: untyped{nkIdent}, body: untyped): Parser =
  proc combinationChain(state: sink State, input: openArray[char], position: Natural): ParseResult =
    result = parser(state, input, position)
    if result.isOk:
      body

  parserNameTable[combinationChain] = "combinationChain[" & parserNameTable[parser] & "]"

  combinationChain


template chain*(parser: Parser, body: untyped): Parser = chain(parser, state, input, position, body)


proc textParser*(text: string): Parser =
  ## Returns a parser that matches the given input with the `text`.
  if textParserCache.hasKey(text): return textParserCache[text]

  proc parseText(state: sink State, input: openArray[char], position: Natural): ParseResult =
    ## Matches the given input with the `text`.
    if position + text.len > input.len:
      ParseResult(state: state, isOk: false, error: ParseErrorTree(isBranch: false, reason: "End of Input!"))
    elif input[position..<(position + text.len)] != text:
      let parsedText = input[position..<(position + text.len)].toString
      let lineInfo = state.lineInfoAt(position)
      ParseResult(state: state, isOk: false, error: ParseErrorTree(isBranch: false,
        reason: &"Expected `{text}` but got `{parsedText}` at line {lineInfo.line}, column {lineInfo.column}!"))
    else:
      ParseResult(state: state, isOk: true, position: position + text.len)

  textParserCache[text] = parseText

  parserNameTable[parseText] = "parseText("
  parserNameTable[parseText].addQuoted text
  parserNameTable[parseText].add ")"
  return parseText


parser(identifierParser, state, input, position):
  ## Parses an identifier from the input.
  if position >= input.len: return ParseResult(state: state, isOk: false, error: ParseErrorTree(isBranch: false, reason: "End of Input!"))

  var offset = position

  if input[offset] in {'a'..'z', 'A'..'Z', '_'}:
    inc offset

    while offset < input.len and input[offset] in {'a'..'z', 'A'..'Z', '0'..'9', '_'}:
      inc offset

  if offset == position: return ParseResult(state: state, isOk: false, error: ParseErrorTree(isBranch: false, reason: &"Expected an identifier but found {input[offset]}!"))

  result = ParseResult(state: state, isOk: true, position: offset)

  result.state.ast.add AstNode(kind: anIdentifier, lineInfo: result.state.lineInfoAt(position), identVal: input[position..<offset].toString)
  result.nodes.add ~result.state.ast.high

export tables.`$`