package java.time

import java.time.temporal._

/** Created by alonsodomin on 25/12/2015. */
final class YearMonth private (year: Int, month: Int)
    extends TemporalAccessor with Temporal with TemporalAdjuster
    with Comparable[YearMonth] with java.io.Serializable {

  import Preconditions._
  import ChronoField._
  import ChronoUnit._

  requireDateTime(year >= Year.MIN_VALUE && year <= Year.MAX_VALUE,
      s"Invalid year: $year")
  requireDateTime(month >= 1 && month <= 12, s"Invalid month: $month")

  def isSupported(field: TemporalField): Boolean = field match {
    case _: ChronoField =>
      field == YEAR || field == YEAR_OF_ERA || field == MONTH_OF_YEAR ||
      field == PROLEPTIC_MONTH || field == ERA

    case null => false
    case _    => field.isSupportedBy(this)
  }

  def isSupported(unit: TemporalUnit): Boolean = unit match {
    case _: ChronoUnit =>
      unit == MONTHS || unit == YEARS || unit == DECADES || unit == CENTURIES ||
      unit == MILLENNIA || unit == ERAS

    case null => false
    case _    => unit.isSupportedBy(this)
  }

  // Implemented by TemporalAccessor
  // def range(field: TemporalField): ValueRange

  def getLong(field: TemporalField): Long = field match {
    case YEAR            => year
    case YEAR_OF_ERA     => if (year < 1) 1 - year else year
    case MONTH_OF_YEAR   => month
    case PROLEPTIC_MONTH => prolepticMonth
    case ERA             => if (year < 1) 0 else 1

    case _: ChronoField  =>
      throw new UnsupportedTemporalTypeException("Unsupported field: " + field)

    case _ => field.getFrom(this)
  }

  private def prolepticMonth: Long = (year * 12.0 + (month - 1)).toLong

  def getYear(): Int = year

  def getMonthValue(): Int = month

  def getMonth(): Month = Month.of(month)

  def isLeapYear(): Boolean = Year.isLeap(year)

  def isValidDay(dayOfMonth: Int): Boolean =
    dayOfMonth >= 1 && dayOfMonth <= lengthOfMonth()

  def lengthOfMonth(): Int = getMonth().length(isLeapYear())

  def lengthOfYear(): Int = Year.of(year).length()

  def `with`(field: TemporalField, value: Long): YearMonth = {
    def withYearMonth(y: Int, m: Int): YearMonth = {
      if (y == year && m == month) this
      else YearMonth.of(y, m)
    }

    field match {
      case YEAR =>
        withYearMonth(YEAR.checkValidIntValue(value), month)

      case YEAR_OF_ERA =>
        val isoYear = YEAR.checkValidIntValue(
          if (year < 1) 1 - value
          else value
        )
        withYearMonth(isoYear, month)

      case MONTH_OF_YEAR =>
        withYearMonth(year, MONTH_OF_YEAR.checkValidIntValue(value))

      case PROLEPTIC_MONTH =>
        plusMonths(value - getLong(PROLEPTIC_MONTH))

      case ERA =>
        requireDateTime(value >= 0 && value <= 1, s"Invalid value for ERA: $value")
        val era = getLong(ERA)
        if (value == era) this
        else withYearMonth(YEAR.checkValidIntValue(1 - year), month)

      case _: ChronoField =>
        throw new UnsupportedTemporalTypeException("Unsupported field: " + field)

      case _ => field.adjustInto(this, value)
    }
  }

  def withYear(year: Int): YearMonth = `with`(YEAR, year)

  def withMonth(month: Int): YearMonth = `with`(MONTH_OF_YEAR, month)

  def plus(amount: Long, unit: TemporalUnit): YearMonth = unit match {
    case MONTHS    => plusMonths(amount)
    case YEARS     => plusYears(amount)
    case DECADES   => plusYears(MathJDK8Bridge.multiplyExact(amount, 10))
    case CENTURIES => plusYears(MathJDK8Bridge.multiplyExact(amount, 100))
    case MILLENNIA => plusYears(MathJDK8Bridge.multiplyExact(amount, 1000))

    case ERAS =>
      val era = getLong(ERA)
      `with`(ERA, MathJDK8Bridge.addExact(era, amount))

    case _: ChronoUnit =>
      throw new UnsupportedTemporalTypeException("Unsupported unit: " + unit)

    case _ => unit.addTo(this, amount)
  }

  def plusYears(years: Long): YearMonth = {
    if (years == 0) {
      this
    } else {
      val newYear = year + years
      new YearMonth(YEAR.checkValidIntValue(newYear), month)
    }
  }

  def plusMonths(months: Long): YearMonth = {
    if (months == 0) {
      this
    } else {
      val newProlepticMonth = prolepticMonth + months
      val newYear = MathJDK8Bridge.floorDiv(newProlepticMonth, 12)
      val newMonth = MathJDK8Bridge.floorMod(newProlepticMonth, 12) + 1
      new YearMonth(
          YEAR.checkValidIntValue(newYear),
          MONTH_OF_YEAR.checkValidIntValue(newMonth)
      )
    }
  }

  override def minus(amount: Long, unit: TemporalUnit): YearMonth =
    if (amount != Long.MinValue) plus(-amount, unit)
    else plus(Long.MaxValue, unit).plus(1, unit)

  def minusYears(years: Long): YearMonth = minus(years, YEARS)

  def minusMonths(months: Long): YearMonth = minus(months, MONTHS)

  // Not implemented
  // def query[R](query: TemporalQuery[R]): R

  def adjustInto(temporal: Temporal): Temporal =
    temporal.`with`(PROLEPTIC_MONTH, prolepticMonth)

  def until(endExclusive: Temporal, unit: TemporalUnit): Long = {
    def other: YearMonth = YearMonth.from(endExclusive)
    def monthsDiff: Long = other.prolepticMonth - prolepticMonth
    unit match {
      case MONTHS    => monthsDiff
      case YEARS     => monthsDiff / 12
      case DECADES   => until(endExclusive, YEARS) / 10
      case CENTURIES => until(endExclusive, YEARS) / 100
      case MILLENNIA => until(endExclusive, YEARS) / 1000
      case ERAS      => other.getLong(ERA) - getLong(ERA)

      case _: ChronoUnit =>
        throw new UnsupportedTemporalTypeException("Unsupported unit: " + unit)

      case _ => unit.between(this, endExclusive)
    }
  }

  def atDay(dayOfMonth: Int): LocalDate =
    LocalDate.of(year, month, dayOfMonth)

  def atEndOfMonth: LocalDate =
    atDay(getMonth().length(isLeapYear()))

  def compareTo(other: YearMonth): Int =
    if (year == other.getYear) month - other.getMonthValue
    else year - other.getYear

  def isAfter(other: YearMonth): Boolean = compareTo(other) > 0

  def isBefore(other: YearMonth): Boolean = compareTo(other) < 0

  override def equals(other: Any): Boolean = other match {
    case that: YearMonth =>
      year == that.getYear && month == that.getMonthValue

    case _ => false
  }

  override def hashCode(): Int = year + (month << 27)

  override def toString: String = f"$year%04d-$month%02d"

  // Not implemented
  // def format(formatter: DateTimeFormatter): String

}

object YearMonth {
  import ChronoField._

  def now(): YearMonth = from(LocalDate.now())

  // Not implemented
  // def now(zoneId: ZoneId): YearMonth
  // def now(clock: Clock): YearMonth

  def of(year: Int, month: Month): YearMonth = {
    if (month == null) throw new NullPointerException("month")
    of(year, month.getValue)
  }

  def of(year: Int, month: Int): YearMonth = {
    YEAR.checkValidIntValue(year)
    MONTH_OF_YEAR.checkValidIntValue(month)
    new YearMonth(year, month)
  }

  def from(temporal: TemporalAccessor): YearMonth = temporal match {
    case temporal: YearMonth => temporal
    case _                   => of(temporal.get(YEAR), temporal.get(MONTH_OF_YEAR))
  }

  // Not implemented
  // def parse(text: CharSequence): YearMonth
  // def parse(text: CharSequence, formatter: DateTimeFormatter): YearMonth

}
