import std/tables

import ./base
import ../parser/types

type
  SemanticPass* = object of BasePass
    registeredTypes: seq[RegisteredType]

proc initSemanticPass*(fileName: string, nodes: seq[AstNode], throwOnError = false): SemanticPass =
  SemanticPass(fileName: fileName, nodes: nodes, throwOnError: throwOnError, registeredTypes: @NapkinTypes)


proc processEnum*(p: var SemanticPass, n: AstNode): AstNode =
  result = n
  discard