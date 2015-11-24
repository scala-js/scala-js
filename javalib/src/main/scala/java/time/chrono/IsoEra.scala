package java.time.chrono

import java.time.DateTimeException

final class IsoEra private (name: String, ordinal: Int)
    extends Enum[IsoEra](name, ordinal) with Era {
  def getValue: Int = ordinal
}

object IsoEra {
  final val BCE = new IsoEra("BCE", 0)

  final val CE = new IsoEra("CE", 1)

  private val eras = Seq(BCE, CE)

  def values(): Array[IsoEra] = eras.toArray

  def valueOf(name: String): IsoEra = eras.find(_.name == name).getOrElse {
    throw new IllegalArgumentException(s"No such era: $name")
  }

  def of(isoEra: Int): IsoEra = eras.lift(isoEra).getOrElse {
    throw new DateTimeException(s"Invalid value for isoEra: $isoEra")
  }
}
