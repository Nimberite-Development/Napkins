import std/tables

import ./base
import ../parser/types

type
  SemanticPass* = ref object of BasePass
    registeredTypes: seq[RegisteredType]
    registeredEnums: Table[RegisteredType, seq[tuple[name: string, value: int]]]

proc initSemanticPass*(fileName: string, nodes: seq[AstNode], throwOnError = false): SemanticPass =
  SemanticPass(fileName: fileName, nodes: nodes, throwOnError: throwOnError, registeredTypes: @NapkinTypes)

#proc contains(t: SemanticPass.registeredEnums, name: string): bool =
#  for i in t.keys: (if i.name == name: return true)

proc processEnum*(p: var SemanticPass, n: AstNode): AstNode =
  result = n

  if result.kind != EnumTypeDef:
    p.report "Enum type definition expected, but got " & $result.kind & " instead!", result.eName
  
  let name = result.eName.strVal

  if name in p.registeredTypes:
    p.report "There's already a type with the name `" & name & "`!", result.eName

  if result.eType.strVal notin OrdinalNapkinTypes:
    p.report "Invalid enum type `" & result.eType.strVal & "`!", result.eType

  let (valHigh, valLow) = case result.eType.strVal
    of "UInt8": (high(uint8).int, low(uint8).int)
    of "UInt16": (high(uint16).int, low(uint16).int)
    of "SInt8": (high(int8).int, low(int8).int)
    of "SInt16": (high(int16).int, low(int16).int)
    of "SInt32": (high(int32).int, low(int32).int)
    of "SInt64": (high(int64).int, low(int64).int)
    of "VInt32": (high(int32).int, low(int32).int)
    of "VInt64": (high(int64).int, low(int64).int)
    else:
      p.report "Invalid enum type `" & result.eType.strVal & "`!", result.eType
      return


  let f = result.eFieldDefs[0]

  var value = 0

  if f.efValue.numVal != 0:
    value = f.efValue.numVal

  if value < valLow:
    p.report "Field `" & f.efName.strVal & "` can't be any lower than " & $valLow, f.efValue

  let typ = registerType(name)

  p.registeredTypes.add typ

  p.registeredEnums[typ] = newSeq[tuple[name: string, value: int]]()
  p.registeredEnums[typ].add (f.efName.strVal, value)

  if result.eFieldDefs.len > 1:
    for i in 1..<result.eFieldDefs.len:
      template field: var AstNode = result.eFieldDefs[i]

      if field.efValue.numVal == 0:
        inc value
        field.efValue.numVal = value

      else:
        if value >= field.efValue.numVal:
          p.report "Field `" & field.efName.strVal & "` is out of order, it must be bigger than the last value!", field.efValue

        value = field.efValue.numVal

      if value > valHigh:
        p.report "Field `" & field.efName.strVal & "` can't be bigger than " & $valHigh & "!", field.efValue

      p.registeredEnums[typ].add (field.efName.strVal, value)

  result.eResolved = true


proc processStruct*(p: var SemanticPass, n: AstNode): AstNode = AstNode(kind: NullLiteral)


proc processPacket*(p: var SemanticPass, n: AstNode): AstNode = AstNode(kind: NullLiteral)