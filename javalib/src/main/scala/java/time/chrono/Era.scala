package java.time.chrono

import java.time.temporal._

trait Era extends TemporalAccessor with TemporalAdjuster {
  def getValue(): Int

  def isSupported(field: TemporalField): Boolean = field match {
    case _: ChronoField  => field == ChronoField.ERA
    case null            => false
    case _               => field.isSupportedBy(this)
  }

  // Implemented by TemporalAccessor
  // def range(field: TemporalField): ValueRange

  def getLong(field: TemporalField): Long = field match {
    case ChronoField.ERA => getValue()

    case _: ChronoField =>
      throw new UnsupportedTemporalTypeException(s"Field not supported: $field")

    case _ => field.getFrom(this)
  }

  // Not implemented
  // def query[R](query: TemporalQuery[R]): R

  def adjustInto(temporal: Temporal): Temporal =
    temporal.`with`(ChronoField.ERA, getValue())

  // Not implemented
  // def getDisplayName(style: java.time.format.TextStyle,
  //     locale: ju.Locale): String
}
