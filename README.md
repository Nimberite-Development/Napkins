# Napkins
A basic data language for defining the differences of the MC protocol across
different MC protocol versions!

## Syntax

## Example
SpawnEntity(4, 0x0C -> 107, 0x00 -> 762, 0x01) -> Client:
  eid: VInt
  uuid: UUID
  (0) typ: SInt8
  (477) typ: VInt
  pos: Vec3[Float64]
  pitch: Angle
  yaw: Angle
  (759) headYaw: Angle
  (47) data: SInt32
  (759) data: VInt
  velocity: Vec3[SInt16]
