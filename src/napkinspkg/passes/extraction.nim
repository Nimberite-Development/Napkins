import std/[
  algorithm,
  strformat,
  sequtils,
  tables
]

import ./base
import ../parser/types

type
  ExtractionPass* = object of BasePass
    pvn*: int

proc initExtractionPass*(fileName: string, nodes: seq[AstNode], pvn: int, throwOnError = false): ExtractionPass =
  ExtractionPass(fileName: fileName, nodes: nodes, pvn: pvn, throwOnError: throwOnError)


proc processEnum*(p: var ExtractionPass, n: AstNode): AstNode =
  result = n

  var fieldTable: Table[string, AstNode]

  for i in n.eFieldDefs:
    if not fieldTable.hasKey(i.efName.strVal):
      if p.pvn >= i.efProto.numVal:
        fieldTable[i.efName.strVal] = i
      continue

    if p.pvn >= i.efProto.numVal and i.efProto.numVal > fieldTable[i.efName.strVal].efProto.numVal:
      fieldTable[i.efName.strVal] = i

  var fields = toSeq(fieldTable.values)

  fields.sort do (a, b: AstNode) -> int: cmp(a.efOrder, b.efOrder)

  result.eFieldDefs = @[]

  for i in fields: result.eFieldDefs.add i


proc processStruct*(p: var ExtractionPass, n: AstNode): AstNode =
  result = n

  var argsTable: Table[string, tuple[proto, name, typ: AstNode, order: int]]
  var order = -1

  for i in n.sArgs:
    inc order
    if not argsTable.hasKey(i.name.strVal):
      if p.pvn >= i.proto.numVal:
        argsTable[i.name.strVal] = (i.proto, i.name, i.typ, order)
      continue
  
  var args = toSeq(argsTable.values)

  args.sort do (a, b: tuple[proto, name, typ: AstNode, order: int]) -> int: cmp(a.order, b.order)

  result.sArgs = @[]

  for i in args: result.sArgs.add (i.proto, i.name, i.typ)

  var fieldTable: Table[string, AstNode]
  
  for i in n.sFieldDefs:
    if not fieldTable.hasKey(i.sfName.strVal):
      if p.pvn >= i.sfProto.numVal:
        fieldTable[i.sfName.strVal] = i
      continue

    if p.pvn >= i.sfProto.numVal and i.sfProto.numVal > fieldTable[i.sfName.strVal].sfProto.numVal:
      fieldTable[i.sfName.strVal] = i

  var fields = toSeq(fieldTable.values)

  fields.sort do (a, b: AstNode) -> int: cmp(a.sfOrder, b.sfOrder)

  result.sFieldDefs = @[]

  for i in fields: result.sFieldDefs.add i


proc processPacket*(p: var ExtractionPass, n: AstNode): AstNode =
  result = n

  var fieldTable: Table[string, AstNode]

  for i in n.pFieldDefs:
    if not fieldTable.hasKey(i.pfName.strVal):
      if p.pvn >= i.pfProto.numVal:
        fieldTable[i.pfName.strVal] = i
      continue

    if p.pvn >= i.pfProto.numVal and i.pfProto.numVal > fieldTable[i.pfName.strVal].pfProto.numVal:
      fieldTable[i.pfName.strVal] = i

  var fields = toSeq(fieldTable.values)

  fields.sort do (a, b: AstNode) -> int: cmp(a.pfOrder, b.pfOrder)

  result.pFieldDefs = @[]

  for i in fields: result.pFieldDefs.add i