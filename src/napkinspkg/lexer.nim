import std/[strutils, strformat]

{.define: napkinTokenIds.}

type
  LexerFailureReason* = enum
    DedentationMismatch, InvalidNumber, InvalidToken

  NapkinsLexerException* = object of CatchableError
    fileName*: string
    reason*: LexerFailureReason
    line*, column*, pos*: int

  TkType* = enum
    # Define the possible token types,including 'Error' which is only present to
    # ensure that all tokens have a type explicitly set.
    Error, Ident, Num, Null, OpenParen, CloseParen, OpenBrack, CloseBrack,
    OpenBrace, CloseBrace, Comma, Arrow, Colon, Indent, Dedent, Placeholder,
    Dot, At, ShiftLeft, ShiftRight, BitAnd, BitOr, BitNot, EndOfFile

  Token* = object
    # Define the token object
    typ*: TkType
    val*: string
    startLine*, startColumn*: int
    when defined(napkinTokenIds):
      id: int

  Lexer* = object
    throwOnError: bool ## Toggle for quiting or throwing errors
    fileName, code: string # TODO: Streams?
    tokens: seq[Token]
    pos, column, line, indentCounter: int
    indentStack: seq[int]

    when defined(napkinTokenIds):
      tokenId: int

proc report(l: Lexer, msg: string, reason: LexerFailureReason, line, column, pos: int) =
  template errstr: string = &"{l.fileName}:{line}:{column}"

  if l.throwOnError:
    let err = newException(NapkinsLexerException,
      &"[{errstr}] {msg}")
    (err.fileName, err.line, err.column, err.pos, err.reason) = (l.fileName, l.line, l.column, l.pos, reason)
    raise err

  else:
    quit &"[{errstr}] {msg}"

template report(l: Lexer, msg: string, reason: LexerFailureReason) = l.report(msg, reason,
  l.line, l.column, l.pos)

proc initLexer*(code: string, fileName: string = "<string>",
  throwOnError: bool = true): Lexer = Lexer(
    throwOnError: throwOnError, fileName: fileName, code: code, column: 1, line: 1
  )

proc `$`*(token: Token, depth: int = 0): string =
  template indent(m: int): string = repeat("  ", depth + m)

  result &= &"{indent(0)}Token(\n"
  
  for name, field in token.fieldPairs:
    result &= indent(1) & name & ": "
    result.addQuoted(field)
    result &= ",\n"

  result.setLen(result.len - 2)
  
  result &= &"\n{indent(0)})"

proc `$`*(tokens: seq[Token]): string =
  if tokens.len == 0:
    return "@[]"
  result = "@[\n"
  
  for token in tokens:
    result &= &"{`$`(token, 1)},\n"

  result.setLen(result.len - 2)

  result &= "\n]"

proc initToken*(l: var Lexer, typ: TkType, val: string, startLine, startColumn: int): Token =
  result = Token(
    startLine: startLine,
    startColumn: startColumn,
    typ: typ,
    val: val
  )

  when defined(napkinTokenIds):
    result.id = l.tokenId
    inc l.tokenId

template atEnd(l: Lexer): bool = l.pos >= l.code.len
template curChar(l: Lexer): char = l.code[l.pos]

proc next(l: var Lexer) =
  if l.curChar == '\n':
    inc l.line
    l.column = 1

  else:
    inc l.column

  inc l.pos

proc lexIndent(l: var Lexer) =
  var lexeme: string
  let (startLine, startColumn, startPos) = (l.line, l.column, l.pos)

  while not l.atEnd and l.curChar == ' ':
    lexeme &= l.curChar
    l.next()

  if l.atEnd or l.curChar == '\n': return

  if l.indentStack.len == 0:
    if lexeme.len != 0:
      l.indentStack.add lexeme.len
      l.tokens.add l.initToken(Indent, lexeme, startLine, startColumn)

  elif l.indentStack[^1] < lexeme.len:
    l.indentStack.add lexeme.len
    l.tokens.add l.initToken(Indent, lexeme, startLine, startColumn)

  elif l.indentStack[^1] > lexeme.len:
    if lexeme.len != 0:
      let indexOfMatchingIndent = l.indentStack.find(lexeme.len)

      if indexOfMatchingIndent == -1:
        l.report &"Dedentation level doesn't match existing dedentation in scope!",
          DedentationMismatch, startLine, startColumn, startPos

      let toPop = l.indentStack[indexOfMatchingIndent..<l.indentStack.len].len - 1

      for i in 0..<toPop:
        discard l.indentStack.pop()
        l.tokens.add l.initToken(Dedent, lexeme, startLine, startColumn)

    else:
      for i in 0..<l.indentStack.len: l.tokens.add l.initToken(Dedent, lexeme, startLine, startColumn)

      l.indentStack.setLen(0)

proc lexIdentifier(l: var Lexer) =
  var lexeme: string
  let (startLine, startColumn) = (l.line, l.column)

  while not l.atEnd and l.curChar.isAlphaNumeric:
    lexeme &= l.curChar
    l.next()

  l.tokens.add l.initToken((if lexeme != "Null": Ident else: Null), lexeme, startLine, startColumn)

proc lexNumOrArr(l: var Lexer) =
  var lexeme = $l.curChar
  let (startLine, startColumn, startPos) = (l.line, l.column, l.pos)
  l.next()

  if lexeme[0] == '-':
    if l.atEnd:
      l.report &"{lexeme} isn't a valid number!", InvalidNumber, startLine, startColumn, startPos

    elif l.curChar == '>':
      lexeme &= l.curChar
      l.next()
      l.tokens.add l.initToken(Arrow, lexeme, startLine, startColumn)
      return

    elif not l.curChar.isDigit:
      lexeme &= l.curChar
      l.report &"{lexeme} isn't a valid number!", InvalidNumber, startLine, startColumn, startPos

  var isHex = false

  if l.curChar == 'x':
    isHex = true
    lexeme &= 'x'
    l.next()

  template validChars: string = (if isHex: "0123456789ABCDEFabcdef" else: "0123456789")

  while not l.atEnd and l.curChar in validChars:
    lexeme &= l.curChar
    l.next()

  if isHex: lexeme = $parseHexInt(lexeme)

  l.tokens.add l.initToken(Num, lexeme, startLine, startColumn)
  return

proc lex*(l: var Lexer): seq[Token] =
  # TODO: Error reporting function
  while not l.atEnd:
    if l.curChar == '\n':
      l.next()
      l.lexIndent()
      continue

    elif l.curChar == '#':
      while not l.atEnd and l.curChar != '\n': l.next()
      continue

    elif l.curChar == '.':
      var counter = 0
      while l.curChar == '.':
        inc counter
        l.next()

      case counter
        of 1:
          l.tokens.add l.initToken(Dot, ".", l.line, l.column)
        of 3:
          l.tokens.add l.initToken(Placeholder, "...", l.line, l.column)
        else:
          l.report &"Unexpected character `{l.curChar}`!", InvalidToken
      continue

    elif l.curChar.isAlphaAscii:
      l.lexIdentifier()
      continue

    elif l.curChar.isDigit or l.curChar == '-':
      l.lexNumOrArr()
      continue

    else:
      var typ = case l.curChar
        of '(': OpenParen
        of ')': CloseParen
        of '[': OpenBrack
        of ']': CloseBrack
        of '{': OpenBrace
        of '}': CloseBrace
        of ':': Colon
        of ',': Comma
        of '@': At
        of '!': BitNot
        of '&': BitAnd
        of '|': BitOr
        of ' ':
          l.next()
          continue
        else:
          let (line, column) = (l.line, l.column)
          if l.curChar == '<':
            l.next()
            if l.curChar == '<':
              l.tokens.add l.initToken(ShiftLeft, "<<", line, column)
              continue
          
          elif l.curChar == '>':
            l.next()
            if l.curChar == '>':
              l.tokens.add l.initToken(ShiftRight, ">>", line, column)
              continue

          l.report &"Unexpected character `{l.curChar}`!", InvalidToken
          return

      l.tokens.add l.initToken(typ, $l.curChar, l.line, l.column)
      l.next()
      continue

  for i in 0..<l.indentStack.len: l.tokens.add l.initToken(Dedent, "", l.line, l.column)

  l.tokens.add l.initToken(EndOfFile, "", l.line, l.column)

  return l.tokens