import std/strformat

import ../parser/types

type
  NapkinsPassError* = object of CatchableError
    pass*: string
    node*: AstNode

  BasePass* = ref object of RootObj
    fileName*: string
    nodes*: seq[AstNode]
    throwOnError*: bool

  PassConcept* = concept p
    p of BasePass

proc report*[T: PassConcept](p: T, msg: string, node: AstNode) =
  template errstr: string = &"[{node.lineInfo.fileName}:{node.lineInfo.line}:{node.lineInfo.column}] Error: {msg}"
  if p.throwOnError:
    var err = newException(NapkinsPassError, errstr())
    err.pass = $T
    err.node = node
    raise err

  else:
    quit errstr()

proc processEnum*[T: PassConcept](p: var T, n: AstNode): AstNode =
  mixin processEnum
  when compiles(n.processEnum(p)):
    n.processEnum(p)

  else:
    {.error: &"`{$T}` doesn't implement `processEnum`.".}

proc processStruct*[T: PassConcept](p: var T, n: AstNode): AstNode =
  mixin processStruct
  when compiles(n.processStruct(p)):
    n.processStruct(p)

  else:
    {.error: &"`{$T}` doesn't implement `processStruct`.".}

proc processPacket*[T: PassConcept](p: var T, n: AstNode): AstNode =
  mixin processPacket
  when compiles(n.processPacket(p)):
    n.processPacket(p)

  else:
    {.error: &"`{$T}` doesn't implement `processPacket`.".}

proc process*[T: PassConcept](p: var T): seq[AstNode] =
  mixin processEnum
  mixin processStruct
  mixin processPacket

  for n in p.nodes:
    result.add case n.kind
      of EnumTypeDef:
        p.processEnum(n)

      of StructDef:
        p.processStruct(n)

      of PacketDef:
        p.processPacket(n)

      else:
        echo "Invalid node in this context"
        quit()