# Napkins
A basic data language for defining the differences of the MC protocol across different MC
protocol versions!

I need to also figure out a way to accurately and cleanly document the format so that it
can be used by more people, code examples can only do so much.

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
`VarInt`). The order of how fields are generated are also dependent on the order of the
definitions in the code, with `eid` being before `uuid`, as an example.

Fields may also be prefixed by the protocol version it was introduced in, an example of this being
`(759) headYaw: Angle`, which is to be interpreted as the field not existing beforehand, and it
being defined from that protocol onwards.

As the type of the data may change between protocol versions, it is possible to use the protocol
version prefixed field definition to override the types defined. For example, the `data` field
was introduced in protocol version 47, with the type `SInt32` (a signed 4 bit integer), but was
then updated to `VInt` (a VarInt) in protocol version 759. Adding on to this syntax, if a field
was removed in a certain protocol version, it is also possible to remove a defined field if you
use `None` as the type. For example, while this isn't the case here, you could do
`(800) data: None` to remove the `data` field in fictional protocol version 800.

## Example
```
SpawnEntity(4, 0x0C -> 107, 0x00 -> 762, 0x01) -> Client:
  eid: VInt
  uuid: UUID
  typ: SInt8
  (477) typ: VInt
  pos: Vec3[Float64]
  pitch: Angle
  yaw: Angle
  (759) headYaw: Angle
  (47) data: SInt32
  (759) data: VInt
  velocity: Vec3[SInt16]
```

## Credit
Honestly I wouldn't of came up with this format if [dewycube](https://github.com/dewycube) hadn't
originally came up with it! He originally discussed it in the Minecraft Protocol discord server,
which can be found at <https://discord.com/invite/Tf4xwK3Ke7>, or via
[wiki.vg](https://wiki.vg/Main_Page)!