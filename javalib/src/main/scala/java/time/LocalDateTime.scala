package java.time

import java.time.LocalTime._
import java.time.temporal._

final class LocalDateTime private(date: LocalDate, time: LocalTime)
  extends Temporal
    with TemporalAdjuster
    with Comparable[LocalDateTime]
    with java.io.Serializable {

  import java.time.temporal.ChronoUnit._
  import java.time.temporal.ChronoField._

  override def until(endExclusive: Temporal, unit: TemporalUnit): Long =
    unit.between(this, LocalDateTime.from(endExclusive))

  override def `with`(field: TemporalField, newValue: Long): Temporal =
    if (field.isDateBased)
      `with`(date.`with`(field, newValue), time)
    else
      `with`(date, time.`with`(field, newValue))

  private def `with`(newDate: LocalDate, newTime: LocalTime): LocalDateTime = {
    if (date == newDate && time == newTime)
      this
    else
      new LocalDateTime(newDate, newTime)
  }

  override def adjustInto(temporal: Temporal): Temporal =
    temporal
      .`with`(EPOCH_DAY, date.toEpochDay)
      .`with`(NANO_OF_DAY, time.toNanoOfDay)

  override def compareTo(o: LocalDateTime): Int = {
    val dateCompare = date.compareTo(o.toLocalDate)
    if (dateCompare == 0)
      time.compareTo(o.toLocalTime)
    else
      dateCompare
  }

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

  override def getLong(field: TemporalField): Long = date.get(field) + time.get(field)

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

  def toLocalDate(): LocalDate = date

  def toLocalTime(): LocalTime = time

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

  def plusMinutes(minutes: Long): LocalDateTime =
    overflowPlus(date,
      hours = 0,
      minutes,
      seconds = 0,
      nanos = 0,
      sign = 1
    )

  def plusHours(hours: Long): LocalDateTime =
    overflowPlus(date,
      hours = hours,
      minutes = 0,
      seconds = 0,
      nanos = 0,
      sign = 1
    )

  def plusDays(days: Long): LocalDateTime = `with`(date.plusDays(days), time)

  def plusWeeks(weeks: Long): LocalDateTime = `with`(date.plusWeeks(weeks), time)

  def plusMonths(months: Long): LocalDateTime = `with`(date.plusMonths(months), time)

  def plusYears(years: Long): LocalDateTime = `with`(date.plusYears(years), time)

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

  def from(temporal: Temporal): LocalDateTime = ???
}
