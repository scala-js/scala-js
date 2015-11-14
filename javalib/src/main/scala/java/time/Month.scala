package java.time

import java.time.temporal._

final class Month private (name: String, value: Int, defaultLength: Int)
    extends Enum[Month](name, value - 1) with TemporalAccessor
    with TemporalAdjuster {
  import Month._

  private lazy val defaultFirstDayOfYear =
    months.take(value - 1).foldLeft(1)(_ + _.minLength)

  def getValue(): Int = value

  // Not implemented
  // def getDisplayName(style: TextStyle, locale: Locale): Locale

  def isSupported(field: TemporalField): Boolean = field match {
    case _: ChronoField => field == ChronoField.MONTH_OF_YEAR
    case null           => false
    case _              => field.isSupportedBy(this)
  }

  // Implemented by TemporalAccessor
  // def range(field: TemporalField): ValueRange

  // Implemented by TemporalAccessor
  // def get(field: TemporalField): Int

  def getLong(field: TemporalField): Long = field match {
    case ChronoField.MONTH_OF_YEAR => ordinal + 1

    case _: ChronoField =>
      throw new UnsupportedTemporalTypeException(s"Field not supported: $field")

    case _ => field.getFrom(this)
  }

  def plus(months: Long): Month = {
    val offset = if (months < 0) months % 12 + 12 else months % 12
    of((ordinal + offset.toInt) % 12 + 1)
  }

  def minus(months: Long): Month = {
    val offset = if (months < 0) months % 12 else months % 12 - 12
    of((ordinal - offset.toInt) % 12 + 1)
  }

  def length(leapYear: Boolean): Int = if (leapYear) maxLength else minLength

  def minLength(): Int = defaultLength

  def maxLength(): Int =
    if (value == 2) defaultLength + 1 else defaultLength

  def firstDayOfYear(leapYear: Boolean): Int =
    if (leapYear && value > 2) defaultFirstDayOfYear + 1
    else defaultFirstDayOfYear

  def firstMonthOfQuarter(): Month = of((ordinal / 3) * 3 + 1)

  // Not implemented
  // def query[R](query: TemporalQuery[R]): R

  def adjustInto(temporal: Temporal): Temporal =
    temporal.`with`(ChronoField.MONTH_OF_YEAR, value)
}

object Month {
  final lazy val JANUARY = new Month("JANUARY", 1, 31)

  final lazy val FEBRUARY = new Month("FEBRUARY", 2, 28)

  final lazy val MARCH = new Month("MARCH", 3, 31)

  final lazy val APRIL = new Month("APRIL", 4, 30)

  final lazy val MAY = new Month("MAY", 5, 31)

  final lazy val JUNE = new Month("JUNE", 6, 30)

  final lazy val JULY = new Month("JULY", 7, 31)

  final lazy val AUGUST = new Month("AUGUST", 8, 31)

  final lazy val SEPTEMBER = new Month("SEPTEMBER", 9, 30)

  final lazy val OCTOBER = new Month("OCTOBER", 10, 31)

  final lazy val NOVEMBER = new Month("NOVEMBER", 11, 30)

  final lazy val DECEMBER = new Month("DECEMBER", 12, 31)

  private lazy val months = Seq(JANUARY, FEBRUARY, MARCH, APRIL, MAY, JUNE,
      JULY, AUGUST, SEPTEMBER, OCTOBER, NOVEMBER, DECEMBER)

  def values(): Array[Month] = months.toArray

  def valueOf(name: String): Month = months.find(_.name == name).getOrElse {
    throw new IllegalArgumentException(s"No such month: $name")
  }

  def of(month: Int): Month = months.lift(month - 1).getOrElse {
    throw new DateTimeException(s"Invalid value for month: $month")
  }

  def from(temporal: TemporalAccessor): Month =
    Month.of(temporal.get(ChronoField.MONTH_OF_YEAR))
}
