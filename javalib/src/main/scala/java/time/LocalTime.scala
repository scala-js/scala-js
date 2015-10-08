package java.time

import scala.scalajs.js

import java.time.temporal._

final class LocalTime private (hour: Int, minute: Int, second: Int, nano: Int)
    extends Temporal with TemporalAdjuster with Comparable[LocalTime]
    with java.io.Serializable {

  import Preconditions.requireDateTime
  import LocalTime._
  import Constants._
  import ChronoField._
  import ChronoUnit._

  private val totalNanos = (nano.toLong + NANOS_IN_SECOND.toLong * second +
      NANOS_IN_MINUTE * minute + NANOS_IN_HOUR * hour)

  requireDateTime(hour >= 0 && hour <= 23, s"Invalid value for hour: $hour")
  requireDateTime(minute >= 0 && minute <= 59, s"Invalid value for minute: $minute")
  requireDateTime(second >= 0 && second <= 59, s"Invalid value for second: $second")
  requireDateTime(nano >= 0 && nano <= 999999999, s"Invalid value for nanoOfSecond $nano")

  def isSupported(field: TemporalField): Boolean = field match {
    case NANO_OF_SECOND | NANO_OF_DAY | MICRO_OF_SECOND | MICRO_OF_DAY |
        MILLI_OF_SECOND | MILLI_OF_DAY | SECOND_OF_MINUTE | SECOND_OF_DAY |
        MINUTE_OF_HOUR | MINUTE_OF_DAY | HOUR_OF_AMPM | CLOCK_HOUR_OF_AMPM |
        HOUR_OF_DAY | CLOCK_HOUR_OF_DAY | AMPM_OF_DAY =>
      true

    case _: ChronoField => false
    case _              => field.isSupportedBy(this)
  }

  def isSupported(unit: TemporalUnit): Boolean = unit match {
    case NANOS | MICROS | MILLIS | SECONDS | MINUTES | HOURS | HALF_DAYS => true

    case _: ChronoUnit => false
    case _             => unit.isSupportedBy(this)
  }

  // Implemented by TemporalAccessor
  // def range(field: TemporalField): ValueRange

  // Implemented by TemporalAccessor
  // def get(field: TemporalField): Int

  def getLong(field: TemporalField): Long = field match {
    case NANO_OF_SECOND     => nano
    case NANO_OF_DAY        => totalNanos
    case MICRO_OF_SECOND    => nano / NANOS_IN_MICRO
    case MICRO_OF_DAY       => totalNanos / NANOS_IN_MICRO
    case MILLI_OF_SECOND    => nano / NANOS_IN_MILLI
    case MILLI_OF_DAY       => totalNanos / NANOS_IN_MILLI
    case SECOND_OF_MINUTE   => second
    case SECOND_OF_DAY      => totalNanos / NANOS_IN_SECOND
    case MINUTE_OF_HOUR     => minute
    case MINUTE_OF_DAY      => totalNanos / NANOS_IN_MINUTE
    case HOUR_OF_AMPM       => hour % 12
    case CLOCK_HOUR_OF_AMPM => if (hour % 12 == 0) 12 else hour % 12
    case HOUR_OF_DAY        => hour
    case CLOCK_HOUR_OF_DAY  => if (hour == 0) 24 else hour
    case AMPM_OF_DAY        => hour / 12

    case _: ChronoField =>
      throw new UnsupportedTemporalTypeException(s"Field not supported: $field")

    case _ => field.getFrom(this)
  }

  def getHour(): Int = hour

  def getMinute(): Int = minute

  def getSecond(): Int = second

  def getNano(): Int = nano

  override def `with`(adjuster: TemporalAdjuster): LocalTime = {
    adjuster.adjustInto(this).asInstanceOf[LocalTime]
  }

  def `with`(field: TemporalField, value: Long): LocalTime = {
    val msg = s"Invalid value: $value"
    field match {
      case NANO_OF_SECOND =>
        requireDateTime(value >= 0 && value < NANOS_IN_SECOND, msg)
        new LocalTime(hour, minute, second, value.toInt)

      case NANO_OF_DAY =>
        requireDateTime(value >= 0 && value < NANOS_IN_DAY, msg)
        ofNanoOfDay(value)

      case MICRO_OF_SECOND =>
        requireDateTime(value >= 0 && value < MICROS_IN_SECOND, msg)
        new LocalTime(hour, minute, second, value.toInt * 1000)

      case MICRO_OF_DAY =>
        requireDateTime(value >= 0 && value < MICROS_IN_DAY, msg)
        ofNanoOfDay(value * NANOS_IN_MICRO)

      case MILLI_OF_SECOND =>
        requireDateTime(value >= 0 && value < MILLIS_IN_SECOND, msg)
        new LocalTime(hour, minute, second, value.toInt * 1000000)

      case MILLI_OF_DAY =>
        requireDateTime(value >= 0 && value < MILLIS_IN_DAY, msg)
        ofNanoOfDay(value * NANOS_IN_MILLI)

      case SECOND_OF_MINUTE =>
        requireDateTime(value >= 0 && value < SECONDS_IN_MINUTE, msg)
        new LocalTime(hour, minute, value.toInt, nano)

      case SECOND_OF_DAY =>
        requireDateTime(value >= 0 && value < SECONDS_IN_DAY, msg)
        ofNanoOfDay(value * NANOS_IN_SECOND + nano)

      case MINUTE_OF_HOUR =>
        requireDateTime(value >= 0 && value < MINUTES_IN_HOUR, msg)
        new LocalTime(hour, value.toInt, second, nano)

      case MINUTE_OF_DAY =>
        requireDateTime(value >= 0 && value <= MINUTES_IN_DAY, msg)
        ofNanoOfDay(value * NANOS_IN_MINUTE + second.toLong * NANOS_IN_SECOND + nano)

      case HOUR_OF_AMPM =>
        requireDateTime(value >= 0 && value <= 11, msg)
        if (hour < 12) new LocalTime(value.toInt, minute, second, nano)
        else new LocalTime(value.toInt + 12, minute, second, nano)

      case CLOCK_HOUR_OF_AMPM =>
        requireDateTime(value >= 1 && value <= 12, msg)
        `with`(HOUR_OF_AMPM, value % 12)

      case HOUR_OF_DAY =>
        requireDateTime(value >= 0 && value <= 23, msg)
        new LocalTime(value.toInt, minute, second, nano)

      case CLOCK_HOUR_OF_DAY =>
        requireDateTime(value >= 1 && value <= 24, msg)
        new LocalTime(value.toInt % 24, minute, second, nano)

      case AMPM_OF_DAY =>
        requireDateTime(value >= 0 && value <= 1, msg)
        new LocalTime(hour % 12 + value.toInt * 12, minute, second, nano)

      case _: ChronoField =>
        throw new UnsupportedTemporalTypeException(s"Field not supported: $field")

      case _ => field.adjustInto(this, value)
    }
  }

  def withHour(hour: Int): LocalTime =
    new LocalTime(hour, minute, second, nano)

  def withMinute(minute: Int): LocalTime =
    new LocalTime(hour, minute, second, nano)

  def withSecond(second: Int): LocalTime =
    new LocalTime(hour, minute, second, nano)

  def withNano(nanoOfSecond: Int): LocalTime =
    new LocalTime(hour, minute, second, nanoOfSecond)

  def truncatedTo(unit: TemporalUnit): LocalTime = {
    if (unit.getDuration.getSeconds > SECONDS_IN_DAY) {
      throw new UnsupportedTemporalTypeException("Unit too large")
    } else if (NANOS_IN_DAY % unit.getDuration.toNanos != 0) {
      val msg = "Unit must divide into a standard day without remainder"
      throw new UnsupportedTemporalTypeException(msg)
    } else {
      val unitNanos = unit.getDuration.toNanos
      ofNanoOfDay(totalNanos / unitNanos * unitNanos)
    }
  }

  override def plus(amount: TemporalAmount): LocalTime =
    amount.addTo(this).asInstanceOf[LocalTime]

  def plus(amount: Long, unit: TemporalUnit): LocalTime = unit match {
    case NANOS     => plusNanos(amount)
    case MICROS    => plusNanos((amount % MICROS_IN_DAY) * NANOS_IN_MICRO)
    case MILLIS    => plusNanos((amount % MILLIS_IN_DAY) * NANOS_IN_MILLI)
    case SECONDS   => plusSeconds(amount)
    case MINUTES   => plusMinutes(amount)
    case HOURS     => plusHours(amount)
    case HALF_DAYS => plusHours((amount % 2) * 12)

    case _: ChronoUnit =>
      throw new UnsupportedTemporalTypeException(s"Unit not supported: $unit")

    case _ => unit.addTo(this, amount).asInstanceOf[LocalTime]
  }

  def plusHours(hours: Long): LocalTime = {
    val offset = (hours % 24).toInt + 24
    new LocalTime((hour + offset) % 24, minute, second, nano)
  }

  def plusMinutes(minutes: Long): LocalTime =
    plusNanos((minutes % MINUTES_IN_DAY) * NANOS_IN_MINUTE)

  def plusSeconds(seconds: Long): LocalTime =
    plusNanos((seconds % SECONDS_IN_DAY) * NANOS_IN_SECOND)

  def plusNanos(nanos: Long): LocalTime = {
    val offset = nanos % NANOS_IN_DAY + NANOS_IN_DAY
    ofNanoOfDay((totalNanos + offset) % NANOS_IN_DAY)
  }

  override def minus(amount: TemporalAmount): LocalTime =
    amount.subtractFrom(this).asInstanceOf[LocalTime]

  override def minus(amount: Long, unit: TemporalUnit): LocalTime = unit match {
    case NANOS     => minusNanos(amount)
    case MICROS    => minusNanos((amount % MICROS_IN_DAY) * NANOS_IN_MICRO)
    case MILLIS    => minusNanos((amount % MILLIS_IN_DAY) * NANOS_IN_MILLI)
    case SECONDS   => minusSeconds(amount)
    case MINUTES   => minusMinutes(amount)
    case HOURS     => minusHours(amount)
    case HALF_DAYS => minusHours((amount % 2) * 12)

    case _: ChronoUnit =>
      throw new UnsupportedTemporalTypeException(s"Unit not supported: $unit")

    case _ =>
      super.minus(amount, unit).asInstanceOf[LocalTime]
  }

  def minusHours(hours: Long): LocalTime = {
    val offset = if (hours < 0) hours % 24 else hours % 24 - 24
    new LocalTime((hour - offset.toInt) % 24, minute, second, nano)
  }

  def minusMinutes(minutes: Long): LocalTime =
    minusNanos((minutes % MINUTES_IN_DAY) * NANOS_IN_MINUTE)

  def minusSeconds(seconds: Long): LocalTime =
    minusNanos((seconds % SECONDS_IN_DAY) * NANOS_IN_SECOND)

  def minusNanos(nanos: Long): LocalTime = {
    val offset =
      if (nanos < 0) nanos % NANOS_IN_DAY
      else nanos % NANOS_IN_DAY - NANOS_IN_DAY
    ofNanoOfDay((totalNanos - offset) % NANOS_IN_DAY)
  }

  // Not implemented
  // def query[R](query: TemporalQuery[R]): R

  def adjustInto(temporal: Temporal): Temporal =
    temporal.`with`(NANO_OF_DAY, totalNanos)

  def until(end: Temporal, unit: TemporalUnit): Long = {
    val endTime = from(end)
    val diff = endTime.totalNanos - totalNanos
    unit match {
      case NANOS     => diff
      case MICROS    => diff / NANOS_IN_MICRO
      case MILLIS    => diff / NANOS_IN_MILLI
      case SECONDS   => diff / NANOS_IN_SECOND
      case MINUTES   => diff / NANOS_IN_MINUTE
      case HOURS     => diff / NANOS_IN_HOUR
      case HALF_DAYS => diff / (12 * NANOS_IN_HOUR)

      case _: ChronoUnit =>
        throw new UnsupportedTemporalTypeException(s"Unit not supported: $unit")

      case _ => unit.between(this, endTime)
    }
  }

  // Not implemented
  // def format(format: DateTimeFormatter): String

  // TODO
  // def atDate(date: LocalDate): LocalDateTime

  // Not implemented
  // def atOffset(offset: ZoneOffset): OffsetTime

  def toSecondOfDay(): Int = (totalNanos / NANOS_IN_SECOND).toInt

  def toNanoOfDay(): Long = totalNanos

  def compareTo(other: LocalTime): Int = totalNanos.compareTo(other.totalNanos)

  def isAfter(other: LocalTime): Boolean = totalNanos > other.totalNanos

  def isBefore(other: LocalTime): Boolean = totalNanos < other.totalNanos

  override def equals(that: Any): Boolean = that match {
    case that: LocalTime => totalNanos == that.totalNanos
    case _               => false
  }

  override def hashCode(): Int = totalNanos.hashCode

  override def toString(): String = {
    val prefix = f"$hour%02d:$minute%02d"
    val nanoPart = nano match {
      case 0 => ""
      case _ if nano % 1000000 == 0 => f".${nano / 1000000}%03d"
      case _ if nano % 1000 == 0 => f".${nano / 1000}%06d"
      case _ => f".$nano%09d"
    }
    val secondPart = second match {
      case 0 if nanoPart.isEmpty => ""
      case _ => f":$second%02d"
    }
    prefix + secondPart + nanoPart
  }
}

object LocalTime {
  import Preconditions.requireDateTime
  import Constants._
  import ChronoField._

  val MIN = new LocalTime(0, 0, 0, 0)

  val MAX = new LocalTime(23, 59, 59, 999999999)

  val MIDNIGHT = MIN

  val NOON = new LocalTime(12, 0, 0, 0)

  def now(): LocalTime = {
    val date = new js.Date()
    val nano = date.getMilliseconds * 1000000
    new LocalTime(date.getHours, date.getMinutes, date.getSeconds, nano)
  }

  // Not implemented
  // def now(zone: ZoneId): LocalTime

  // Not implemented
  // def now(clock: Clock): LocalTime

  def of(hour: Int, minute: Int): LocalTime =
    new LocalTime(hour, minute, 0, 0)

  def of(hour: Int, minute: Int, second: Int): LocalTime =
    new LocalTime(hour, minute, second, 0)

  def of(hour: Int, minute: Int, second: Int, nanoOfSecond: Int): LocalTime =
    new LocalTime(hour, minute, second, nanoOfSecond)

  def ofSecondOfDay(secondOfDay: Long): LocalTime = {
    requireDateTime(secondOfDay >= 0 && secondOfDay < SECONDS_IN_DAY,
      s"Invalid value for secondOfDay: $secondOfDay")

    ofNanoOfDay(secondOfDay * 1000000000)
  }

  def ofNanoOfDay(nanoOfDay: Long): LocalTime = {
    requireDateTime(nanoOfDay >= 0 && nanoOfDay < NANOS_IN_DAY,
      s"Invalid value for nanoOfDay: $nanoOfDay")

    val nano = nanoOfDay % NANOS_IN_SECOND
    val seconds = nanoOfDay / NANOS_IN_SECOND
    val second = seconds % SECONDS_IN_MINUTE
    val minutes = seconds / SECONDS_IN_MINUTE
    val minute = minutes % MINUTES_IN_HOUR
    val hour = minutes / MINUTES_IN_HOUR
    new LocalTime(hour.toInt, minute.toInt, second.toInt, nano.toInt)
  }

  def from(temporal: TemporalAccessor): LocalTime =
    ofNanoOfDay(temporal.getLong(NANO_OF_DAY))

  // Not implemented
  // def parse(text: CharSequence): LocalTime

  // Not implemented
  // def parse(text: CharSequence, formatter: DateTimeFormatter): LocalTime
}
