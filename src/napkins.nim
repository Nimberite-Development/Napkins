import std/strformat

import ./napkinspkg/combinator

const
  CodeA = "string:wah"
  CodeB = "integer:7"
  CodeC = "dice:2d8"
  CodeD = "nil:nil"

let typeParser = (identifierParser & textParser(":")).chain(state, input, position):
  let typ = result.state.ast[result.nodes[0].int].identVal
  let lineInfo = result.state.lineInfoAt(position)

  case typ
  of "string":
    discard
  of "integer":
    discard
  of "dice":
    discard
  else:
    return ParseResult(state: result.state, isOk: false, error: ParseErrorTree(isBranch: false,
      reason: &"Found the unknown type `{typ}` while parsing at line {lineInfo.line}, column {lineInfo.column}!"))

for code in [CodeA, CodeB, CodeC, CodeD]:
  let res = (newlineLocator & typeParser)(State(), code, 0)

  if res.isOk:
    echo res
  else:
    echo res.error.dumpTree