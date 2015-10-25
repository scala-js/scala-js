package java.time.temporal

import java.time.Duration

final class ChronoUnit private (name: String, ordinal: Int, duration: Duration,
    flags: Int) extends Enum[ChronoUnit](name, ordinal) with TemporalUnit {

  import ChronoUnit._

  def getDuration(): Duration = duration

  def isDurationEstimated(): Boolean = (flags & isTimeBasedFlag) == 0

  def isDateBased(): Boolean = (flags & isDateBasedFlag) != 0

  def isTimeBased(): Boolean = (flags & isTimeBasedFlag) != 0

  override def isSupportedBy(temporal: Temporal): Boolean =
    temporal.isSupported(this)

  def addTo[R <: Temporal](temporal: R, amount: Long): R =
    temporal.plus(amount, this).asInstanceOf[R]

  def between(start: Temporal, end: Temporal): Long = start.until(end, this)
}

object ChronoUnit {
  private final val isTimeBasedFlag = 1
  private final val isDateBasedFlag = 2

  final val NANOS =
    new ChronoUnit("Nanos", 0, Duration.OneNano, isTimeBasedFlag)

  final val MICROS =
    new ChronoUnit("Micros", 1, Duration.OneMicro, isTimeBasedFlag)

  final val MILLIS =
    new ChronoUnit("Millis", 2, Duration.OneMilli, isTimeBasedFlag)

  final val SECONDS =
    new ChronoUnit("Seconds", 3, Duration.OneSecond, isTimeBasedFlag)

  final val MINUTES =
    new ChronoUnit("Minutes", 4, Duration.OneMinute, isTimeBasedFlag)

  final val HOURS =
    new ChronoUnit("Hours", 5, Duration.OneHour, isTimeBasedFlag)

  final val HALF_DAYS =
    new ChronoUnit("HalfDays", 6, Duration.ofHours(12), isTimeBasedFlag)

  final val DAYS = new ChronoUnit("Days", 7, Duration.OneDay, isDateBasedFlag)

  final val WEEKS =
    new ChronoUnit("Weeks", 8, Duration.OneWeek, isDateBasedFlag)

  final val MONTHS =
    new ChronoUnit("Months", 9, Duration.OneMonth, isDateBasedFlag)

  final val YEARS =
    new ChronoUnit("Years", 10, Duration.OneYear, isDateBasedFlag)

  final val DECADES = new ChronoUnit("Decades", 11,
      Duration.OneYear.multipliedBy(10), isDateBasedFlag)

  final val CENTURIES = new ChronoUnit("Centuries", 12,
      Duration.OneYear.multipliedBy(100), isDateBasedFlag)

  final val MILLENNIA = new ChronoUnit("Millennia", 13,
      Duration.OneYear.multipliedBy(1000), isDateBasedFlag)

  final val ERAS = new ChronoUnit("Eras", 14,
      Duration.OneYear.multipliedBy(1000000000), isDateBasedFlag)

  final val FOREVER = new ChronoUnit("Forever", 15, Duration.Max, 0)

  private val units =
    Array(NANOS, MICROS, MILLIS, SECONDS, MINUTES, HOURS, HALF_DAYS, DAYS,
        WEEKS, MONTHS, YEARS, DECADES, CENTURIES, MILLENNIA, ERAS, FOREVER)

  def values(): Array[ChronoUnit] = units.clone()

  def valueOf(name: String): ChronoUnit = {
    units.find(f => name == toScreamingSnakeCase(f.name)).getOrElse {
      throw new IllegalArgumentException(s"No such unit: $name")
    }
  }
}
