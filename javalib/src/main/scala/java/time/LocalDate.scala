package java.time

import scala.scalajs.js

import java.time.chrono.{IsoEra, Era, IsoChronology, ChronoLocalDate}
import java.time.temporal._

final class LocalDate private (year: Int, month: Month, dayOfMonth: Int)
    extends ChronoLocalDate with Serializable {
  import Preconditions.requireDateTime
  import ChronoField._
  import ChronoUnit._
  import LocalDate._

  requireDateTime(year >= Year.MIN_VALUE && year <= Year.MAX_VALUE,
      s"Invalid value for year: $year")

  private val _isLeapYear = iso.isLeapYear(year)

  requireDateTime(dayOfMonth > 0 && dayOfMonth <= month.maxLength,
      s"Invalid value for dayOfMonth: $dayOfMonth")
  requireDateTime(_isLeapYear || dayOfMonth <= month.minLength,
      s"Invalid value for dayOfMonth: $dayOfMonth")

  private lazy val dayOfYear =
    month.firstDayOfYear(_isLeapYear) + dayOfMonth - 1

  private lazy val epochDay = {
    val year1 = year - 1970
    val daysBeforeYear = daysBeforeYears(MathJDK8Bridge.floorMod(year1, 400))._2
    val offset =
      MathJDK8Bridge.floorDiv(year1, 400).toLong * daysInFourHundredYears
    offset + daysBeforeYear + dayOfYear - 1
  }

  private lazy val dayOfWeek =
    MathJDK8Bridge.floorMod(epochDay + 3, 7).toInt + 1

  private def prolepticMonth = year.toLong * 12 + getMonthValue - 1

  // Implemented by ChronoLocalDate
  // def isSupported(field: TemporalField): Boolean
  // def isSupported(unit: TemporalUnit): Boolean

  override def range(field: TemporalField): ValueRange = field match {
    case DAY_OF_MONTH => ValueRange.of(1, lengthOfMonth)
    case DAY_OF_YEAR  => ValueRange.of(1, lengthOfYear)

    case ALIGNED_WEEK_OF_MONTH =>
      ValueRange.of(1, if (lengthOfMonth > 28) 5 else 4)

    case _ => super.range(field)
  }

  // Implemented by TemporalAccessor
  // def get(field: TemporalField): Int

  def getLong(field: TemporalField): Long = field match {
    case DAY_OF_WEEK                  => dayOfWeek
    case ALIGNED_DAY_OF_WEEK_IN_MONTH => (dayOfMonth - 1) % 7 + 1
    case ALIGNED_DAY_OF_WEEK_IN_YEAR  => (getLong(DAY_OF_YEAR) - 1) % 7 + 1
    case DAY_OF_MONTH                 => dayOfMonth
    case DAY_OF_YEAR                  => dayOfYear
    case EPOCH_DAY                    => epochDay
    case ALIGNED_WEEK_OF_MONTH        => (dayOfMonth - 1) / 7 + 1
    case ALIGNED_WEEK_OF_YEAR         => (dayOfYear - 1) / 7 + 1
    case MONTH_OF_YEAR                => getMonthValue
    case PROLEPTIC_MONTH              => prolepticMonth
    case YEAR_OF_ERA                  => if (year > 0) year else 1 - year
    case YEAR                         => year
    case ERA                          => if (year > 0) 1 else 0

    case _: ChronoField =>
      throw new UnsupportedTemporalTypeException(s"Unsupported field: $field")

    case _ => field.getFrom(this)
  }

  def getChronology(): IsoChronology = iso

  override def getEra(): Era = if (year > 0) IsoEra.CE else IsoEra.BCE

  def getYear(): Int = year

  def getMonthValue(): Int = month.getValue

  def getMonth(): Month = month

  def getDayOfMonth(): Int = dayOfMonth

  def getDayOfYear(): Int = dayOfYear

  def getDayOfWeek(): DayOfWeek = DayOfWeek.of(dayOfWeek)

  override def isLeapYear(): Boolean = _isLeapYear

  def lengthOfMonth(): Int = month.length(_isLeapYear)

  def lengthOfYear(): Int = if (_isLeapYear) 366 else 365

  override def `with`(adjuster: TemporalAdjuster): LocalDate =
    adjuster.adjustInto(this).asInstanceOf[LocalDate]

  override def `with`(field: TemporalField, value: Long): LocalDate = {
    val msg = s"Invalid value for $field: $value"
    field match {
      case DAY_OF_WEEK | ALIGNED_DAY_OF_WEEK_IN_MONTH |
          ALIGNED_DAY_OF_WEEK_IN_YEAR =>
        requireDateTime(value > 0 && value <= 7, msg)
        plus(value.toInt - get(field), DAYS)

      case DAY_OF_MONTH =>
        requireDateTime(value > 0 && value <= 31, msg)
        withDayOfMonth(value.toInt)

      case DAY_OF_YEAR =>
        requireDateTime(value > 0 && value <= 366, msg)
        withDayOfYear(value.toInt)

      case EPOCH_DAY =>
        ofEpochDay(value)

      case ALIGNED_WEEK_OF_MONTH =>
        requireDateTime(value > 0 && value <= 5, msg)
        plus(value.toInt - get(field), WEEKS)

      case ALIGNED_WEEK_OF_YEAR =>
        requireDateTime(value > 0 && value <= 53, msg)
        plus(value.toInt - get(field), WEEKS)

      case MONTH_OF_YEAR =>
        requireDateTime(value > 0 && value <= 12, msg)
        withMonth(value.toInt)

      case PROLEPTIC_MONTH =>
        requireDateTime(value >= -11999999988L && value <= 11999999999L, msg)
        val year = MathJDK8Bridge.floorDiv(value, 12).toInt
        val month = MathJDK8Bridge.floorMod(value, 12).toInt + 1
        val day = dayOfMonth min Month.of(month).length(iso.isLeapYear(year))
        LocalDate.of(year, month, day)

      case YEAR_OF_ERA =>
        requireDateTime(value > 0 && value <= Year.MAX_VALUE + 1, msg)
        if (getEra == IsoEra.CE) withYear(value.toInt)
        else withYear(1 - value.toInt)

      case YEAR =>
        requireDateTime(value >= Year.MIN_VALUE && value <= Year.MAX_VALUE, msg)
        withYear(value.toInt)

      case ERA =>
        requireDateTime(value >= 0 && value <= 1, msg)
        val yearOfEra = get(YEAR_OF_ERA)
        if (getEra == IsoEra.BCE && value == 1) withYear(yearOfEra)
        else if (getEra == IsoEra.CE && value == 0) withYear(1 - yearOfEra)
        else this

      case _: ChronoField =>
        throw new UnsupportedTemporalTypeException(s"Unsupported field: $field")

      case _ => field.adjustInto(this, value)
    }
  }

  def withYear(year: Int): LocalDate = {
    if (dayOfMonth == 29 && month == Month.FEBRUARY && !iso.isLeapYear(year))
      LocalDate.of(year, 2, 28)
    else
      LocalDate.of(year, month, dayOfMonth)
  }

  def withMonth(month: Int): LocalDate = {
    val m = Month.of(month)
    LocalDate.of(year, m, dayOfMonth min m.length(isLeapYear))
  }

  def withDayOfMonth(dayOfMonth: Int): LocalDate =
    LocalDate.of(year, month, dayOfMonth)

  def withDayOfYear(dayOfYear: Int): LocalDate =
    LocalDate.ofYearDay(year, dayOfYear)

  override def plus(amount: TemporalAmount): LocalDate =
    amount.addTo(this).asInstanceOf[LocalDate]

  override def plus(amount: Long, unit: TemporalUnit): LocalDate = unit match {
    case DAYS   => plusDays(amount)
    case WEEKS  => plusWeeks(amount)
    case MONTHS => plusMonths(amount)
    case YEARS  => plusYears(amount)

    case DECADES =>
      val years = MathJDK8Bridge.multiplyExact(amount, 10)
      plusYears(years)

    case CENTURIES =>
      val years = MathJDK8Bridge.multiplyExact(amount, 100)
      plusYears(years)

    case MILLENNIA =>
      val years = MathJDK8Bridge.multiplyExact(amount, 1000)
      plusYears(years)

    case ERAS =>
      `with`(ERA, MathJDK8Bridge.addExact(get(ERA), amount))

    case _: ChronoUnit =>
      throw new UnsupportedTemporalTypeException(s"Unsupported unit: $unit")

    case _ => unit.addTo(this, amount)
  }

  def plusYears(years: Long): LocalDate = {
    val year1 = year + years
    requireDateTime(year1 >= Year.MIN_VALUE && year1 <= Year.MAX_VALUE,
        s"Invalid value for year: $year1")
    val leap = iso.isLeapYear(year1)
    val day1 =
      if (month == Month.FEBRUARY && dayOfMonth == 29 && !leap) 28
      else dayOfMonth
    LocalDate.of(year1.toInt, month, day1)
  }

  def plusMonths(months: Long): LocalDate = {
    val month1 = getMonthValue + MathJDK8Bridge.floorMod(months, 12).toInt
    val year1 = year + MathJDK8Bridge.floorDiv(months, 12) +
        (if (month1 > 12) 1 else 0)
    requireDateTime(year1 >= Year.MIN_VALUE && year1 <= Year.MAX_VALUE,
        s"Invalid value for year: $year1")
    val month2 = Month.of(if (month1 > 12) month1 - 12 else month1)
    val day = dayOfMonth min month2.length(iso.isLeapYear(year1))
    LocalDate.of(year1.toInt, month2, day)
  }

  def plusWeeks(weeks: Long): LocalDate =
    plusDays(MathJDK8Bridge.multiplyExact(weeks, 7))

  def plusDays(days: Long): LocalDate = {
    val epochDay1 = MathJDK8Bridge.addExact(epochDay, days)
    LocalDate.ofEpochDay(epochDay1)
  }

  override def minus(amount: TemporalAmount): LocalDate =
    amount.subtractFrom(this).asInstanceOf[LocalDate]

  override def minus(amount: Long, unit: TemporalUnit): LocalDate =
    if (amount != Long.MinValue) plus(-amount, unit)
    else plus(Long.MaxValue, unit).plus(1, unit)

  def minusYears(years: Long): LocalDate = plusYears(-years)

  def minusMonths(months: Long): LocalDate = plusMonths(-months)

  def minusWeeks(weeks: Long): LocalDate = plusWeeks(-weeks)

  def minusDays(days: Long): LocalDate =
    if (days != Long.MinValue) plusDays(-days)
    else plusDays(Long.MaxValue).plusDays(1)

  // Not implemented
  // def query[R](query: TemporalQuery[R]): R

  // Implemented by ChronoLocalDate
  // def adjustInto(temporal: Temporal): Temporal

  def until(end: Temporal, unit: TemporalUnit): Long = {
    import scala.math.Ordering.Implicits._

    val other = LocalDate.from(end)
    unit match {
      case DAYS  => other.toEpochDay - epochDay
      case WEEKS => (other.toEpochDay - epochDay) / 7

      case MONTHS =>
        val dmonths = other.prolepticMonth - prolepticMonth
        if (other.getDayOfMonth < dayOfMonth && dmonths > 0) dmonths - 1
        else if (other.getDayOfMonth > dayOfMonth && dmonths < 0) dmonths + 1
        else dmonths

      case YEARS =>
        val dyears = other.getYear - year
        if ((other.getMonthValue, other.getDayOfMonth) < (getMonthValue, dayOfMonth) &&
            dyears > 0)
          dyears - 1
        else if ((other.getMonthValue, other.getDayOfMonth) > (getMonthValue, dayOfMonth) &&
            dyears < 0)
          dyears + 1
        else
          dyears

      case DECADES   => until(end, YEARS) / 10
      case CENTURIES => until(end, YEARS) / 100
      case MILLENNIA => until(end, YEARS) / 1000

      case ERAS =>
        val year1 = other.getYear
        if (year <= 0 && year1 > 0) 1
        else if (year > 0 && year1 <= 0) -1
        else 0

      case _: ChronoUnit =>
        throw new UnsupportedTemporalTypeException(s"Unsupported unit: $unit")

      case _ => unit.between(this, other)
    }
  }

  def until(end: ChronoLocalDate): Period = {
    val other = LocalDate.from(end)
    val dmonths = other.prolepticMonth - prolepticMonth
    val ddays = other.getDayOfMonth - dayOfMonth
    val corr = {
      if (dmonths > 0 && ddays < 0) -1
      else if (dmonths < 0 && ddays > 0) 1
      else 0
    }
    val months = dmonths + corr
    val days = {
      if (corr < 0) plus(months, MONTHS).until(other, DAYS).toInt
      else if (corr > 0) ddays - other.lengthOfMonth
      else ddays
    }
    val years = (months / 12).toInt
    val months1 = (months % 12).toInt
    Period.of(years, months1, days)
  }

  // Not implemented
  // def format(formatter: java.time.format.DateTimeFormatter): String

  // TODO
  // def atTime(time: LocalTime): LocalDateTime
  // def atTime(hour: Int, minute: Int): LocalDateTime
  // def atTime(hour: Int, minute: Int, second: Int): LocalDateTime
  // def atTime(hour: Int, minute: Int, second: Int, nanoOfSecond: Int):
  //     LocalDateTime

  // Not implemented
  // def atTime(time: OffsetTime): OffsetDateTime


  // TODO
  // def atStartOfDay(): LocalDateTime

  // Not implemented
  // def atStartOfDay(id: ZoneId): ZonedDateTime

  override def toEpochDay(): Long = epochDay

  // Implemented by ChronoLocalDate
  // def compare(other: ChronoLocalDate): Int
  // def isAfter(other: ChronoLocalDate): Boolean
  // def isBefore(other: ChronoLocalDate): Boolean
  // def isEqual(other: ChronoLocalDate): Boolean

  override def equals(other: Any): Boolean = other match {
    case other: LocalDate => isEqual(other)
    case _                => false
  }

  override def hashCode(): Int = epochDay.hashCode

  override def toString(): String = {
    if (year >= 0 && year < 10000)
      f"$year%04d-$getMonthValue%02d-$dayOfMonth%02d"
    else
      f"$year%+05d-$getMonthValue%02d-$dayOfMonth%02d"
  }
}

object LocalDate {
  import Preconditions.requireDateTime

  private final val iso = IsoChronology.INSTANCE

  private val daysBeforeYears = Stream.iterate(1970 -> 0) { case (year, day) =>
    if (iso.isLeapYear(year)) (year + 1) -> (day + 366)
    else (year + 1) -> (day + 365)
  }.take(400).toVector

  private final val daysInFourHundredYears = 146097

  final val MIN = new LocalDate(Year.MIN_VALUE, Month.JANUARY, 1)
  final val MAX = new LocalDate(Year.MAX_VALUE, Month.DECEMBER, 31)

  def now(): LocalDate = {
    val d = new js.Date()
    of(d.getFullYear, d.getMonth + 1, d.getDate)
  }

  // Not implemented
  // def now(zone: ZoneId): LocalDate
  // def now(clock: Clock): LocalDate

  def of(year: Int, month: Month, dayOfMonth: Int): LocalDate =
    new LocalDate(year, month, dayOfMonth)

  def of(year: Int, month: Int, dayOfMonth: Int): LocalDate =
    new LocalDate(year, Month.of(month), dayOfMonth)

  def ofYearDay(year: Int, dayOfYear: Int): LocalDate = {
    requireDateTime(dayOfYear > 0 && dayOfYear <= 366,
        s"Invalid value for dayOfYear: $dayOfYear")
    val leap = iso.isLeapYear(year)
    requireDateTime(dayOfYear <= 365 || leap,
        s"Invalid value for dayOfYear: $dayOfYear")
    val month = Month.values().takeWhile(_.firstDayOfYear(leap) <= dayOfYear).last
    val dayOfMonth = dayOfYear - month.firstDayOfYear(leap) + 1
    of(year, month, dayOfMonth)
  }

  def ofEpochDay(epochDay: Long): LocalDate = {
    val quot = MathJDK8Bridge.floorDiv(epochDay, daysInFourHundredYears)
    val rem = MathJDK8Bridge.floorMod(epochDay, daysInFourHundredYears).toInt
    val (year1, start) = daysBeforeYears.takeWhile(_._2 <= rem).last
    val year2 = year1 + quot * 400
    requireDateTime(year2 >= Year.MIN_VALUE && year2 <= Year.MAX_VALUE,
        s"Invalid value for year: $year2")
    val dayOfYear = rem - start + 1
    LocalDate.ofYearDay(year2.toInt, dayOfYear.toInt)
  }

  def from(temporal: TemporalAccessor): LocalDate = temporal match {
    case temporal: LocalDate =>
      temporal

    case _ =>
      ofEpochDay(temporal.getLong(ChronoField.EPOCH_DAY))
  }

  // TODO
  // def parse(text: CharSequence): LocalDate
  // def parse(text: CharSequence,
  //     formatter: java.time.format.DateTimeFormatter): LocalDate
}
