package java.time

import java.time.chrono.{Chronology, IsoChronology}
import java.time.temporal._

/** Created by alonsodomin on 24/12/2015. */
final class Year private (year: Int)
    extends TemporalAccessor with Temporal with TemporalAdjuster with Comparable[Year]
    with java.io.Serializable {

  import Preconditions._
  import Year._
  import ChronoField._
  import ChronoUnit._

  requireDateTime(year >= MIN_VALUE && year <= MAX_VALUE, s"Invalid year value: $year")

  def getValue(): Int = year

  def isSupported(field: TemporalField): Boolean = field match {
    case _: ChronoField => field == YEAR_OF_ERA || field == YEAR || field == ERA
    case null           => false
    case _              => field.isSupportedBy(this)
  }

  def isSupported(unit: TemporalUnit): Boolean = unit match {
    case _: ChronoUnit =>
      unit == YEARS || unit == DECADES || unit == CENTURIES ||
      unit == MILLENNIA || unit == ERAS

    case null => false
    case _    => unit.isSupportedBy(this)
  }

  override def range(field: TemporalField): ValueRange = field match {
    case YEAR_OF_ERA =>
      if (year <= 0) ValueRange.of(1, MAX_VALUE + 1)
      else ValueRange.of(1, MAX_VALUE)

    case _ => super.range(field)
  }

  override def get(field: TemporalField): Int = field match {
    case YEAR_OF_ERA => if (year < 1) 1 - year else year
    case YEAR        => year
    case ERA         => if (year < 1) 0 else 1

    case _: ChronoField =>
      throw new UnsupportedTemporalTypeException(s"Unsupported field: $field")
  }

  def getLong(field: TemporalField): Long = field match {
    case field: ChronoField => get(field)
    case _                  => field.getFrom(this)
  }

  def isLeap(): Boolean = Year.isLeap(year)

  def isValidMonthDay(monthDay: MonthDay): Boolean =
    monthDay.isValidYear(year)

  def length(): Int = if (isLeap) 366 else 365

  def `with`(field: TemporalField, value: Long): Year = {
    def withYear(newYear: Int): Year = {
      if (year != newYear) Year.of(newYear)
      else this
    }

    field match {
      case YEAR_OF_ERA =>
        val yearOfEra = YEAR_OF_ERA.checkValidIntValue(value)
        val newYear = if (year < 1) 1 - yearOfEra else yearOfEra
        withYear(newYear)

      case YEAR =>
        withYear(YEAR.checkValidIntValue(value))

      case ERA =>
        requireDateTime(value == 0 || value == 1,
            s"Invalid value for field $field: $value")
        val era = get(ERA)
        if (era != value) Year.of(1 - year)
        else this

      case _: ChronoField =>
        throw new UnsupportedTemporalTypeException(s"Unsupported field: $field")

      case _ =>
        field.adjustInto(this, value)
    }
  }

  def plus(amount: Long, unit: TemporalUnit): Year = unit match {
    case YEARS     => plusYears(amount)
    case DECADES   => plusYears(MathJDK8Bridge.multiplyExact(amount, 10))
    case CENTURIES => plusYears(MathJDK8Bridge.multiplyExact(amount, 100))
    case MILLENNIA => plusYears(MathJDK8Bridge.multiplyExact(amount, 1000))

    case ERAS      =>
      val era = get(ERA)
      `with`(ERA, MathJDK8Bridge.addExact(era, amount))

    case _: ChronoUnit =>
      throw new UnsupportedTemporalTypeException(s"Unsupported unit: $unit")

    case _ =>
      unit.addTo(this, amount)
  }

  def plusYears(amount: Long): Year = {
    if (amount == 0) {
      this
    } else {
      val newYear = year + amount
      Year.of(YEAR.checkValidIntValue(newYear))
    }
  }

  override def minus(amount: Long, unit: TemporalUnit): Year = {
    if (amount != Long.MinValue) plus(-amount, unit)
    else plus(Long.MaxValue, unit).plus(1, unit)
  }

  def minusYears(amount: Long): Year = minus(amount, YEARS)

  // Not implemented
  // def query[R](query: TemporalQuery[R]): R

  def adjustInto(temporal: Temporal): Temporal =
    temporal.`with`(YEAR, year)

  def until(end: Temporal, unit: TemporalUnit): Long = {
    val other = Year.from(end)
    val yearsDiff: Int = other.getValue - year

    unit match {
      case YEARS     => yearsDiff
      case DECADES   => yearsDiff / 10
      case CENTURIES => yearsDiff / 100
      case MILLENNIA => yearsDiff / 1000
      case ERAS      => other.get(ERA) - get(ERA)

      case _: ChronoUnit =>
        throw new UnsupportedTemporalTypeException(s"Unsupported unit: $unit")

      case _ => unit.between(this, other)
    }
  }

  // Not implemented
  // def format(formatter: DateTimeFormatter): String

  def atDay(dayOfYear: Int): LocalDate =
    LocalDate.ofYearDay(year, dayOfYear)

  def atMonth(month: Month): YearMonth = YearMonth.of(year, month)

  def atMonth(month: Int): YearMonth = YearMonth.of(year, month)

  def atMonthDay(monthDay: MonthDay): LocalDate = monthDay.atYear(year)

  def compareTo(other: Year): Int = year - other.getValue

  def isAfter(other: Year): Boolean = compareTo(other) > 0

  def isBefore(other: Year): Boolean = compareTo(other) < 0

  override def equals(other: Any): Boolean = other match {
    case that: Year => year == that.getValue
    case _          => false
  }

  override def hashCode(): Int = year.hashCode()

  override def toString: String = year.toString

}

object Year {
  import ChronoField._

  final val MIN_VALUE: Int = -999999999
  final val MAX_VALUE: Int = 999999999

  private final val iso = IsoChronology.INSTANCE

  def now(): Year = from(LocalDate.now())

  // Not implemented
  // def now(zone: ZoneId): Year
  // def now(clock: Clock): Year

  def of(year: Int): Year = {
    YEAR.checkValidIntValue(year)
    new Year(year)
  }

  def from(temporal: TemporalAccessor): Year = temporal match {
    case temporal: Year => temporal
    case _              => of(temporal.get(YEAR))
  }

  // Not implemented
  // def parse(text: CharSequence): Year
  // def parse(text: CharSequence, formatter: DateTimeFormatter): Year

  def isLeap(year: Long): Boolean = iso.isLeapYear(year)

}
