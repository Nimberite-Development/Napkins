import ./napkinspkg/lexer

import ./napkinspkg/parser/[literal, types]

import ./napkinspkg/passes/[semantic, extraction, base]

echo '\n'

var lxr = initLexer(readFile("test.nmp"), "test.nmp", true)
let tokens = lxr.lex()

let ast = parse(tokens, "test.nmp", true).nodes

var ep = initExtractionPass("test.nmp", ast, 850, true)
let nodes = ep.process()

var sp = initSemanticPass("test.nmp", nodes, true)
let nodes2 = sp.process()

echo nodes2