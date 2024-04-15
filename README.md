# Napkins
A basic data language for defining the differences of the MC protocol across different MC
protocol versions!

## How-To
To define a packet, you have to write the name of the packet (e.g. `SpawnEntity`), followed by a
`(`, then the protocol version it was introduced in (e.g. `4`), followed by the ID of a packet
(e.g. `0x0C`).

Additionally, if a packet's ID has been changed in a newer protocol version, `->` can be used to
chain together protocol version and packet ID pairs. You must then use an open parenthesis (`)`)
to indicate that all protocol versions with the appropriate packet ID have been defined.

Then you must follow this by `->` and finally use either `Server` or `Client` to indicate the
direction a packet is going to. Using `Client` means that this packet is going *to* a client,
while `Server` means the packet is coming *from* a client/going *to* a server.

This is then followed by a colon (`:`), and if the packet contains no data, it should be followed
by `None` on the same line, though it can be put on a new line.

This language is whitespace sensitive, and the spacing must be consistent. Fields are defined by
using the desired name (e.g. `eid` for entity id), with it's matching type (e.g. `VInt`, meaning
`VarInt`).

Fields may also be prefixed by the protocol version it was introduced in, an example of this being
`(759) headYaw: Angle`, which is to be interpreted as the field not existing beforehand, and it
being defined from that protocol onwards.

(To be finished.)

## Example
```
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
```