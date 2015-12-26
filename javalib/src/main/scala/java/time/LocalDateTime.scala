package java.time

import java.time.LocalTime._
import java.time.chrono
import java.time.temporal._
import java.util.Objects
import scala.util.Try
import java.time.chrono.{ChronoLocalDateTime, ChronoLocalDate}

final class LocalDateTime private(date: LocalDate, time: LocalTime)
  extends Temporal with TemporalAdjuster with Comparable[LocalDateTime] with java.io.Serializable {

  import java.time.temporal.ChronoUnit._
  import java.time.temporal.ChronoField._

  override def adjustInto(temporal: Temporal): Temporal =
    temporal
      .`with`(EPOCH_DAY, date.toEpochDay)
      .`with`(NANO_OF_DAY, time.toNanoOfDay)

  // def atOffset(offset: ZoneOffset): OffsetDateTime = ???

  // def atZone(zone: ZoneId) = ???

  // def format(formatter: DateTimeFormatter) = ???

  def get(field: TemporalField) = field match {
    case chronoField: ChronoField =>
      if (chronoField.isDateBased) date.get(field) else time.get(field)
    case _ => ChronoLocalDate.super.get(field)
  }

  def getDayOfMonth(): Int = date.getDayOfMonth

  def getDayOfWeek(): DayOfWeek = date.getDayOfWeek

  def getDayOfYear(): Int = date.getDayOfYear

  def getHour(): Int = time.getHour

  override def getLong(field: TemporalField): Long =
    if (field.isDateBased) date.get(field)
    else time.get(field)

  def getMinute(): Int = time.getMinute

  def getMonth(): Month = date.getMonth

  def getMonthValue(): Int = date.getMonthValue

  def getNano(): Int = time.getNano

  def getSecond(): Int = time.getSecond

  def getYear(): Int = date.getYear

  def isAfter(other: ChronoLocalDateTime): Boolean = {
    val otherDate = other.toLocalDate
    if (date.isEqual(other.toLocalDate)) time.isAfter(other.toLocalTime)
    else date.isAfter(otherDate)
  }

  def isBefore(other: ChronoLocalDateTime): Boolean = {
    val otherDate = other.toLocalDate
    if (date.isEqual(otherDate)) time.isBefore(other.toLocalTime)
    else date.isBefore(otherDate)
  }

  def isEqual(other: ChronoLocalDateTime): Boolean =
    date.isEqual(other.toLocalDate) && time.equals(other.toLocalTime)

  def isSupported(field: TemporalField): Boolean = field match {
    case _: ChronoField => field.isTimeBased || field.isDateBased
    case null => false
    case _ => field.isSupportedBy(this)
  }

  def isSupported(unit: TemporalUnit): Boolean = unit match {
    case _: ChronoUnit => unit.isTimeBased || unit.isDateBased
    case null => false
    case _ => unit.isSupportedBy(this)
  }

  def minus(amountToSubtract: Long, unit: TemporalUnit): LocalDateTime = plus(-amountToSubtract, unit)

  def minus(amountToSubtract: TemporalAmount): LocalDateTime = amountToSubtract match {
    case period: Period => `with`(date.minus(period), time)
    case _ => amountToSubtract.subtractFrom(this).asInstanceOf[LocalDateTime]
  }

  def minusDays(days: Long): LocalDateTime = `with`(date.minusDays(days), time)

  def minusHours(hours: Long): LocalDateTime = minus(hours, HOURS)

  def minusMinutes(minutes: Long): LocalDateTime = minus(minutes, MINUTES)

  def minusMonths(months: Long): LocalDateTime = `with`(date.minusMonths(months), time)

  def minusNanos(nanos: Long): LocalDateTime = minus(nanos, NANOS)

  def minusSeconds(seconds: Long): LocalDateTime = minus(seconds, SECONDS)

  def minusWeeks(weeks: Long): LocalDateTime = `with`(date.minusWeeks(weeks), time)

  def minusYears(years: Long): LocalDateTime = `with`(date.minusYears(years), time)

  override def plus(amountToAdd: Long, unit: TemporalUnit): LocalDateTime = {
    if (amountToAdd == 0) {
      this
    } else {
      unit match {
        case NANOS => plusNanos(amountToAdd)
        case MICROS => plusNanos(amountToAdd * 1000)
        case MILLIS => plusNanos(amountToAdd * 1000 * 1000)
        case SECONDS => plusSeconds(amountToAdd)
        case MINUTES => plusMinutes(amountToAdd)
        case HOURS => plusHours(amountToAdd)
        case HALF_DAYS => plusHours(amountToAdd * 12)
        // all units bigger then this have no overflow from time to date, so we can use the LocalDate impl
        case _: ChronoUnit => `with`(date.plus(amountToAdd, unit), time)
        case _ => unit.addTo(this, amountToAdd)
      }
    }
  }

  def plus(amountToAdd: TemporalAmount): LocalDateTime = amountToAdd match {
    case period: Period => `with`(date.plus(period), time)
    case _ => amountToAdd.subtractFrom(this).asInstanceOf[LocalDateTime]
  }

  def plusDays(days: Long): LocalDateTime = `with`(date.plusDays(days), time)

  def plusHours(hours: Long): LocalDateTime =
    overflowPlus(date,
      hours = hours,
      minutes = 0,
      seconds = 0,
      nanos = 0,
      sign = 1
    )

  def plusMinutes(minutes: Long): LocalDateTime =
    overflowPlus(date,
      hours = 0,
      minutes,
      seconds = 0,
      nanos = 0,
      sign = 1
    )

  def plusMonths(months: Long): LocalDateTime = `with`(date.plusMonths(months), time)

  def plusNanos(nanos: Long): LocalDateTime =
    overflowPlus(date,
      hours = 0,
      minutes = 0,
      seconds = 0,
      nanos = nanos,
      sign = 1
    )

  def plusSeconds(seconds: Long): LocalDateTime =
    overflowPlus(date,
      hours = 0,
      minutes = 0,
      seconds = seconds,
      nanos = 0,
      sign = 1
    )

  def plusWeeks(weeks: Long): LocalDateTime = `with`(date.plusWeeks(weeks), time)

  def plusYears(years: Long): LocalDateTime = `with`(date.plusYears(years), time)

  def range(field: TemporalField): ValueRange = field match {
    case chronoField: ChronoField => if (chronoField.isDateBased) date.range(chronoField) else time.range(chronoField)
    case _ => field.rangeRefinedBy(this)
  }

  def toLocalDate(): LocalDate = date

  def toLocalTime(): LocalTime = time

  override def toString(): String = date.toString + "T" + time.toString

  override def compareTo(o: LocalDateTime): Int = {
    val dateCompare = date.compareTo(o.toLocalDate)
    if (dateCompare == 0) time.compareTo(o.toLocalTime)
    else dateCompare
  }

  def truncatedTo(unit: TemporalUnit): LocalDateTime = `with`(date, time.truncatedTo(unit))

  override def until(endExclusive: Temporal, unit: TemporalUnit): Long =
    unit.between(this, LocalDateTime.from(endExclusive))

  override def `with`(field: TemporalField, newValue: Long): Temporal =
    if (field.isDateBased) `with`(date.`with`(field, newValue), time)
    else `with`(date, time.`with`(field, newValue))

  private def `with`(newDate: LocalDate, newTime: LocalTime): LocalDateTime =
    if (date == newDate && time == newTime) this
    else new LocalDateTime(newDate, newTime)

  def withDayOfMonth(dayOfMonth: Int): LocalDateTime = `with`(date.withDayOfMonth(dayOfMonth), time)

  def withDayOfYear(dayOfYear: Int): LocalDateTime = `with`(date.withDayOfYear(dayOfYear), time)

  def withHour(hour: Int): LocalDateTime = `with`(date, time.withHour(hour))

  def withMinute(minute: Int): LocalDateTime = `with`(date, time.withMinute(minute))

  def withMonth(month: Int): LocalDateTime = `with`(date.withMonth(month), time)

  def withNano(nanoOfSecond: Int): LocalDateTime = `with`(date, time.withNano(nanoOfSecond))

  def withSecond(second: Int): LocalDateTime = `with`(date, time.withSecond(second))

  def withYear(year: Int): LocalDateTime = `with`(date.withYear(year), time)

  private def overflowPlus(date: LocalDate,
                           hours: Long,
                           minutes: Long,
                           seconds: Long,
                           nanos: Long,
                           sign: Int): LocalDateTime = {
    val nanoOfDay = time.toNanoOfDay

    val totNanos: Long =
      (nanos % NANOS_PER_DAY +
        (seconds % SECONDS_PER_DAY) * NANOS_PER_SECOND +
        (minutes % MINUTES_PER_DAY) * NANOS_PER_MINUTE +
        (hours % HOURS_PER_DAY) * NANOS_PER_HOUR) * sign + nanoOfDay

    val totDays: Long =
      (nanos / NANOS_PER_DAY +
        seconds / SECONDS_PER_DAY +
        minutes / MINUTES_PER_DAY +
        hours / HOURS_PER_DAY) * sign + Math.floorDiv(totNanos, NANOS_PER_DAY)

    val newNoD: Long = Math.floorMod(totNanos, NANOS_PER_DAY)
    val newTime: LocalTime = if (newNoD == nanoOfDay) time else LocalTime.ofNanoOfDay(newNoD)

    `with`(date.plusDays(totDays), newTime)
  }
}

object LocalDateTime {
  val MAX: LocalDateTime = new LocalDateTime(LocalDate.MAX, LocalTime.MAX)

  val MIN: LocalDateTime = new LocalDateTime(LocalDate.MIN, LocalTime.MIN)

  def from(temporal: Temporal): LocalDateTime = temporal match {
    case localDateTime: LocalDateTime => localDateTime
    // case zonedDateTime: ZonedDateTime => zonedDateTime.toLocalDateTime
    // case offsetDateTime: OffsetDateTime => offsetDateTime.toLocalDateTime
    case _ => new LocalDateTime(LocalDate.from(temporal), LocalTime.from(temporal))
  }

  def now(): LocalDateTime = new LocalDateTime(LocalDate.now(), LocalTime.now())

  // def now(clock: Clock) = ???

  // def now(zone: ZoneId): LocalDateTime = ???

  def of(year: Int, month: Month, dayOfMonth: Int, hour: Int, minute: Int): LocalDateTime = {
    val date: LocalDate = LocalDate.of(year, month, dayOfMonth)
    val time: LocalTime = LocalTime.of(hour, minute)
    new LocalDateTime(date, time)
  }

  def of(year: Int, month: Month, dayOfMonth: Int, hour: Int, minute: Int, second: Int): LocalDateTime = {
    val date = LocalDate.of(year, month, dayOfMonth)
    val time = LocalTime.of(hour, minute, second)
    new LocalDateTime(date, time)
  }

  def of(year: Int, month: Month, dayOfMonth: Int,
         hour: Int, minute: Int, second: Int, nanoOfSecond: Int): LocalDateTime = {
    val date = LocalDate.of(year, month, dayOfMonth)
    val time = LocalTime.of(hour, minute, second, nanoOfSecond)
    new LocalDateTime(date, time)
  }

  def of(year: Int, month: Int, dayOfMonth: Int, hour: Int, minute: Int): LocalDateTime = {
    val date = LocalDate.of(year, month, dayOfMonth)
    val time = LocalTime.of(hour, minute)
    new LocalDateTime(date, time)
  }

  def of(year: Int, month: Int, dayOfMonth: Int, hour: Int, minute: Int, second: Int): LocalDateTime = {
    val date = LocalDate.of(year, month, dayOfMonth)
    val time = LocalTime.of(hour, minute, second)
    new LocalDateTime(date, time)
  }

  def of(year: Int, month: Int, dayOfMonth: Int,
         hour: Int, minute: Int, second: Int, nanoOfSecond: Int): LocalDateTime = {
    val date = LocalDate.of(year, month, dayOfMonth)
    val time = LocalTime.of(hour, minute, second, nanoOfSecond)
    new LocalDateTime(date, time)
  }

  def of(date: LocalDate, time: LocalTime): LocalDateTime = new LocalDateTime(date, time)

  // def ofEpochSecond(epochSecond: Long, nanoOfSecond: Int, offset: ZoneOffset) = ???

  // def ofInstant(instant: Instant, zoneId: ZoneId) = ???

  // def parse(text: CharSequence) = ???

  // def parse(text: CharSequence, formatter: DateTimeFormatter) = ???

}
