package org.hablapps.geofenze

import org.hablapps.coalgebraz._
import Coalgebraz._, EntityOps._

object Geofenze {

  val device: Entity[DeviceIn, DeviceOut, Device, Device] =
    raw(identity, s => _ match {
      case Move(x, y) => (List(Moved(x, y)), Option(Device(s.id, (x, y))))
      case Halt => (List(Halted), None)
    })
}
