import ./napkinspkg/[
  lexer,
  parser
]

var lxr = initLexer(readFile("test.nmp"), "test.nmp", false)
let tokens = lxr.lex()

echo tokens

var ast = parse(tokens).nodes
echo ast.repr