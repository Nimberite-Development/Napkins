import std/[strformat]
import ./types

type
  BasePass* = ref object of RootObj
    throwOnError*: bool

proc loc(node: AstNode): string = &"{node.lineInfo.fileName}:{node.lineInfo.line}:{node.lineInfo.column}"

proc report*[T: BasePass](p: T, msg: string, node: AstNode) =
  if p.throwOnError:
    quit &"[{node.loc}] Error: {msg}"

proc processEnum*[T: BasePass](p: var T, n: AstNode): AstNode =
  mixin processEnum
  when compiles(n.processEnum(p)):
    n.processEnum(p)

  else:
    {.error: &"`{$T}` doesn't implement `processEnum`.".}

proc processStruct*[T: BasePass](p: var T, n: AstNode): AstNode =
  mixin processStruct
  when compiles(n.processStruct(p)):
    n.processStruct(p)

  else:
    {.error: &"`{$T}` doesn't implement `processStruct`.".}

proc processPacket*[T: BasePass](p: var T, n: AstNode): AstNode =
  mixin processPacket
  when compiles(n.processPacket(p)):
    n.processPacket(p)

  else:
    {.error: &"`{$T}` doesn't implement `processPacket`.".}

proc process*[T: BasePass](p: var T, nodes: openArray[AstNode]): seq[AstNode] =
  mixin processEnum
  mixin processStruct
  mixin processPacket

  for n in nodes:
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