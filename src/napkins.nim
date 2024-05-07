import ./napkinspkg/lexer

import ./napkinspkg/parser/[semantic, literal, types]

var lxr = initLexer(readFile("test.nmp"), "test.nmp", true)
let tokens = lxr.lex()

#echo tokens

let ast = parse(tokens, "test.nmp", true).nodes
var sp = initSemanticPass(ast)

echo sp.process()