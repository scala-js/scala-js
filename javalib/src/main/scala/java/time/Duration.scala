package java.time

import scala.collection.JavaConverters._

import java.time.temporal._

final class Duration private (seconds: Long, nanos: Int)
    extends TemporalAmount with Comparable[Duration]
    with java.io.Serializable {

  import Preconditions.requireDateTime
  import Constants._
  import ChronoUnit._

  requireDateTime(nanos >= 0 && nanos <= 999999999,
      "nanos must be >= 0 and <= 999999999")

  private val (normalizedSeconds, normalizedNanos) =
    if (seconds < 0 && nanos > 0) (seconds + 1, nanos - NANOS_IN_SECOND)
    else (seconds, nanos)

  def get(unit: TemporalUnit): Long = unit match {
    case SECONDS => seconds

    case NANOS => nanos

    case _ =>
      throw new UnsupportedTemporalTypeException(s"Unit not supported: $unit")
  }

  def getUnits(): java.util.List[TemporalUnit] =
    Seq[TemporalUnit](SECONDS, NANOS).asJava

  def isZero(): Boolean = seconds == 0 && nanos == 0

  def isNegative(): Boolean = seconds < 0

  def getSeconds(): Long = seconds

  def getNano(): Int = nanos

  def withSeconds(seconds: Long): Duration =
    new Duration(seconds, nanos)

  def withNanos(nanosOfSecond: Int): Duration =
    new Duration(seconds, nanosOfSecond)

  def plus(duration: Duration): Duration = {
    val seconds1 = duration.getSeconds
    val sumNanos = nanos + duration.getNano
    if (seconds1 >= 0) {
      val sumSeconds = MathJDK8Bridge.addExact(seconds, seconds1)
      if (sumNanos >= NANOS_IN_SECOND)
        new Duration(MathJDK8Bridge.incrementExact(sumSeconds),
            sumNanos - NANOS_IN_SECOND)
      else
        new Duration(sumSeconds, sumNanos)
    } else {
      val sumSeconds = MathJDK8Bridge.addExact(seconds, seconds1 + 1)
      if (sumNanos >= NANOS_IN_SECOND)
        new Duration(sumSeconds, sumNanos - NANOS_IN_SECOND)
      else
        new Duration(MathJDK8Bridge.decrementExact(sumSeconds), sumNanos)
    }
  }

  def plus(amount: Long, unit: TemporalUnit): Duration = {
   if (!unit.isDurationEstimated || unit == DAYS)
      plus(unit.getDuration.multipliedBy(amount))
    else
      throw new UnsupportedTemporalTypeException(s"Unit not supported: $unit")
  }

  def plusDays(days: Long): Duration = plus(days, DAYS)

  def plusHours(hours: Long): Duration = plus(hours, HOURS)

  def plusMinutes(minutes: Long): Duration = plus(minutes, MINUTES)

  def plusSeconds(seconds: Long): Duration = plus(seconds, SECONDS)

  def plusMillis(millis: Long): Duration = plus(millis, MILLIS)

  def plusNanos(nanos: Long): Duration = plus(nanos, NANOS)

  def minus(duration: Duration): Duration = {
    if (duration == Duration.Min)
      plus(Duration.Max).plusNanos(1)
    else
      plus(duration.negated())
  }

  def minus(amount: Long, unit: TemporalUnit): Duration = {
    if (!unit.isDurationEstimated || unit == DAYS)
      minus(unit.getDuration.multipliedBy(amount))
    else
      throw new UnsupportedTemporalTypeException(s"Unit not supported: $unit")
  }

  def minusDays(days: Long): Duration = minus(days, DAYS)

  def minusHours(hours: Long): Duration = minus(hours, HOURS)

  def minusMinutes(minutes: Long): Duration = minus(minutes, MINUTES)

  def minusSeconds(seconds: Long): Duration = minus(seconds, SECONDS)

  def minusMillis(millis: Long): Duration = minus(millis, MILLIS)

  def minusNanos(nanos: Long): Duration = minus(nanos, NANOS)

  def multipliedBy(multiplicand: Long): Duration = {
    val (prodNanosQuot, prodNanosRem) = {
      try {
        val prodNanos = MathJDK8Bridge.multiplyExact(normalizedNanos, multiplicand)
        (prodNanos / NANOS_IN_SECOND, (prodNanos % NANOS_IN_SECOND).toInt)
      } catch {
        case _: ArithmeticException =>
          val prodNanos = BigInt(normalizedNanos) * multiplicand
          ((prodNanos / NANOS_IN_SECOND).toLong, (prodNanos % NANOS_IN_SECOND).toInt)
      }
    }
    val prodSeconds = MathJDK8Bridge.multiplyExact(normalizedSeconds, multiplicand)
    val newSeconds =
      if (prodNanosRem >= 0) MathJDK8Bridge.addExact(prodSeconds, prodNanosQuot)
      else MathJDK8Bridge.addExact(prodSeconds, prodNanosQuot - 1)
    val newNanos =
      if (prodNanosRem >= 0) prodNanosRem
      else prodNanosRem + NANOS_IN_SECOND
    new Duration(newSeconds, newNanos)
  }

  def dividedBy(divisor: Long): Duration = {
    val secondsQuot = normalizedSeconds / divisor
    val secondsRem = normalizedSeconds % divisor
    val nanos = {
      try {
        val total = MathJDK8Bridge.addExact(
            MathJDK8Bridge.multiplyExact(secondsRem, NANOS_IN_SECOND),
            normalizedNanos)
        total / divisor
      } catch {
        case _: ArithmeticException =>
          val total = BigInt(secondsRem) * NANOS_IN_SECOND + normalizedNanos
          (total / divisor).toLong
      }
    }
    Duration.ofSeconds(secondsQuot).plusNanos(nanos)
  }

  def negated(): Duration = multipliedBy(-1)

  def abs(): Duration = if (isNegative()) negated() else this

  def addTo(temporal: Temporal): Temporal = {
    val t1 =
      if (seconds == 0) temporal
      else temporal.plus(seconds, SECONDS)
    if (nanos == 0) t1
    else t1.plus(nanos, NANOS)
  }

  def subtractFrom(temporal: Temporal): Temporal = {
    val t1 =
      if (seconds == 0) temporal
      else temporal.minus(seconds, SECONDS)
    if (nanos == 0) t1
    else t1.minus(nanos, NANOS)
  }

  def toDays(): Long = seconds / SECONDS_IN_DAY

  def toHours(): Long = seconds / SECONDS_IN_HOUR

  def toMinutes(): Long = seconds / SECONDS_IN_MINUTE

  def toMillis(): Long = {
    val millis1 = MathJDK8Bridge.multiplyExact(seconds, MILLIS_IN_SECOND)
    val millis2 = nanos / NANOS_IN_MILLI
    MathJDK8Bridge.addExact(millis1, millis2)
  }

  def toNanos(): Long =
    MathJDK8Bridge.addExact(
        MathJDK8Bridge.multiplyExact(seconds, NANOS_IN_SECOND), nanos)

  def compareTo(that: Duration): Int = {
    val secCmp = seconds.compareTo(that.getSeconds)
    if (secCmp == 0) nanos.compareTo(that.getNano)
    else secCmp
  }

  override def equals(that: Any): Boolean = that match {
    case that: Duration =>
      seconds == that.getSeconds && nanos == that.getNano

    case _ => false
  }

  override def hashCode(): Int = 31 * seconds.hashCode + nanos

  override def toString(): String = {
    val mins = normalizedSeconds / 60
    val secsOfMin = normalizedSeconds % 60
    val hours = mins / 60
    val minsOfHour = mins % 60
    val hourPart = if (hours == 0) "" else hours.toString + "H"
    val minPart = if (minsOfHour == 0) "" else minsOfHour.toString + "M"
    val nanos1 = math.abs(normalizedNanos)
    val decimals = f"$nanos1%09d".reverse.dropWhile(_ == '0').reverse
    val decimalPart = if (decimals.isEmpty) "" else "." + decimals
    val secsPart = secsOfMin match {
      case 0 if seconds != 0 && nanos == 0 => ""
      case 0 if seconds < 0                => "-0" + decimalPart + "S"
      case n                               => n.toString + decimalPart + "S"
    }
    "PT" + hourPart + minPart + secsPart
  }
}

object Duration {
  import Constants._

  final val ZERO = new Duration(0, 0)

  private[time] final val Min = new Duration(Long.MinValue, 0)

  private[time] final val Max = new Duration(Long.MaxValue, 999999999)

  private[time] final val OneNano = new Duration(0, 1)

  private[time] final val OneMicro = new Duration(0, NANOS_IN_MICRO)

  private[time] final val OneMilli = new Duration(0, NANOS_IN_MILLI)

  private[time] final val OneSecond = new Duration(1, 0)

  private[time] final val OneMinute = new Duration(SECONDS_IN_MINUTE, 0)

  private[time] final val OneHour = new Duration(SECONDS_IN_HOUR, 0)

  private[time] final val OneDay = new Duration(SECONDS_IN_DAY, 0)

  private[time] final val OneWeek = new Duration(SECONDS_IN_WEEK, 0)

  private[time] final val OneMonth = new Duration(SECONDS_IN_MONTH, 0)

  private[time] final val OneYear = OneMonth.multipliedBy(12)

  def ofDays(days: Long): Duration = OneDay.multipliedBy(days)

  def ofHours(hours: Long): Duration = OneHour.multipliedBy(hours)

  def ofMinutes(minutes: Long): Duration = OneMinute.multipliedBy(minutes)

  def ofSeconds(seconds: Long): Duration = new Duration(seconds, 0)

  def ofSeconds(seconds: Long, nanoAdjustment: Long): Duration =
    ofSeconds(seconds).plusNanos(nanoAdjustment)

  def ofMillis(millis: Long): Duration = OneMilli.multipliedBy(millis)

  def ofNanos(nanos: Long): Duration = OneNano.multipliedBy(nanos)

  def of(amount: Long, unit: TemporalUnit): Duration = {
    if (!unit.isDurationEstimated || unit == ChronoUnit.DAYS)
      unit.getDuration.multipliedBy(amount)
    else
      throw new UnsupportedTemporalTypeException(s"Unit not supported: $unit")
  }

  def from(amount: TemporalAmount): Duration = {
    amount.getUnits.asScala.foldLeft(ZERO) { (d, u) =>
      d.plus(amount.get(u), u)
    }
  }

  // Not implemented
  // def parse(text: CharSequence): Duration

  def between(start: Temporal, end: Temporal): Duration = {
    try {
      val nanos = start.until(end, ChronoUnit.NANOS)
      Duration.ofNanos(nanos)
    } catch {
      case _:DateTimeException | _:ArithmeticException =>
        val seconds = start.until(end, ChronoUnit.SECONDS)
        val nanos = {
          try {
            end.get(ChronoField.NANO_OF_SECOND) -
                start.get(ChronoField.NANO_OF_SECOND)
          } catch {
            case _: DateTimeException => 0
          }
        }
        Duration.ofSeconds(seconds, nanos)
    }
  }
}
