import ./napkinspkg/[
  lexer,
  parser
]

var lxr = initLexer(readFile("test.nmp"), "test.nmp", false)
let tokens = lxr.lex()

echo tokens

var ast = parse(tokens, "test.nmp", true).nodes
echo ast.repr