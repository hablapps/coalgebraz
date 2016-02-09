package org.hablapps.geofenze

case class Device(id: String, pos: (Int, Int))

sealed trait DeviceIn
case class Move(x: Int, y: Int) extends DeviceIn
case object Halt extends DeviceIn

sealed trait DeviceOut
case class Moved(x: Int, y: Int) extends DeviceOut
case object Halted extends DeviceOut
