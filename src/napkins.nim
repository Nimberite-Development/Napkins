import ./napkinspkg/lexer

import ./napkinspkg/parser/[literal, types]

import ./napkinspkg/passes/[final_semantic, extraction, initial_semantic, base]

echo '\n'

var lxr = initLexer(readFile("test.nmp"), "test.nmp", true)
let tokens = lxr.lex()

let ast = parse(tokens, "test.nmp", true).nodes

var sp = initInitalSemanticPass("test.nmp", ast, true)
let nodes = sp.process()

var ep = initExtractionPass("test.nmp", nodes, 850, true)
let nodes2 = ep.process()

var fp = initFinalSemanticPass("test.nmp", nodes2, true)
let nodes3 = fp.process()

echo nodes3