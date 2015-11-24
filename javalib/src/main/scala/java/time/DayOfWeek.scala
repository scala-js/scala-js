package java.time

import java.time.temporal._

final class DayOfWeek private (name: String, value: Int)
    extends Enum[DayOfWeek](name, value - 1) with TemporalAccessor
    with TemporalAdjuster {

  def getValue(): Int = value

  // Not implemented
  // def getDisplayName(style: TextStyle, locale: ju.Locale): String

  def isSupported(field: TemporalField): Boolean = field match {
    case _: ChronoField  => field == ChronoField.DAY_OF_WEEK
    case null            => false
    case _               => field.isSupportedBy(this)
  }

  // Implemented by TemporalAccessor
  // def range(field: TemporalField): ValueRange

  // Implemented by TemporalAccessor
  // def get(field: TemporalField): Int

  def getLong(field: TemporalField): Long = field match {
    case ChronoField.DAY_OF_WEEK => ordinal + 1

    case _: ChronoField =>
      throw new UnsupportedTemporalTypeException(s"Field not supported: $field")

    case _ => field.getFrom(this)
  }

  def plus(days: Long): DayOfWeek = {
    val offset = (days % 7 + 7) % 7
    DayOfWeek.of((ordinal + offset.toInt) % 7 + 1)
  }

  def minus(days: Long): DayOfWeek = plus(-(days % 7))

  // Not implemented
  // def query[R](query: TemporalQuery[R]): R

  def adjustInto(temporal: Temporal): Temporal =
    temporal.`with`(ChronoField.DAY_OF_WEEK, ordinal + 1)
}

object DayOfWeek {
  final val MONDAY = new DayOfWeek("MONDAY", 1)

  final val TUESDAY = new DayOfWeek("TUESDAY", 2)

  final val WEDNESDAY = new DayOfWeek("WEDNESDAY", 3)

  final val THURSDAY = new DayOfWeek("THURSDAY", 4)

  final val FRIDAY = new DayOfWeek("FRIDAY", 5)

  final val SATURDAY = new DayOfWeek("SATURDAY", 6)

  final val SUNDAY = new DayOfWeek("SUNDAY", 7)

  private val days =
    Seq(MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY, SATURDAY, SUNDAY)

  def values(): Array[DayOfWeek] = days.toArray

  def valueOf(name: String): DayOfWeek = days.find(_.name == name).getOrElse {
    throw new IllegalArgumentException(s"No such weekday: $name")
  }

  def of(dayOfWeek: Int): DayOfWeek = days.lift(dayOfWeek - 1).getOrElse {
    throw new DateTimeException(s"Invalid value for weekday: $dayOfWeek")
  }

  def from(temporal: TemporalAccessor): DayOfWeek =
    DayOfWeek.of(temporal.get(ChronoField.DAY_OF_WEEK))
}
