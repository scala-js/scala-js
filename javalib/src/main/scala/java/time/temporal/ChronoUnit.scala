package java.time.temporal

import java.time.Duration

final class ChronoUnit private (name: String, ordinal: Int, duration: Duration,
    dateBased: Boolean) extends Enum[ChronoUnit](name, ordinal)
    with TemporalUnit {

  def getDuration(): Duration = duration

  def isDurationEstimated(): Boolean = dateBased

  def isDateBased(): Boolean = dateBased

  def isTimeBased(): Boolean = !dateBased

  override def isSupportedBy(temporal: Temporal): Boolean =
    temporal.isSupported(this)

  def addTo[R <: Temporal](temporal: R, amount: Long): R =
    temporal.plus(amount, this).asInstanceOf[R]

  def between(start: Temporal, end: Temporal): Long = start.until(end, this)
}

object ChronoUnit {
  final val NANOS = new ChronoUnit("Nanos", 0, Duration.OneNano, false)

  final val MICROS = new ChronoUnit("Micros", 1, Duration.OneMicro, false)

  final val MILLIS = new ChronoUnit("Millis", 2, Duration.OneMilli, false)

  final val SECONDS = new ChronoUnit("Seconds", 3, Duration.OneSecond, false)

  final val MINUTES = new ChronoUnit("Minutes", 4, Duration.OneMinute, false)

  final val HOURS = new ChronoUnit("Hours", 5, Duration.OneHour, false)

  final val HALF_DAYS = new ChronoUnit("HalfDays", 6, Duration.ofHours(12), false)

  final val DAYS = new ChronoUnit("Days", 7, Duration.OneDay, true)

  final val WEEKS = new ChronoUnit("Weeks", 8, Duration.OneWeek, true)

  final val MONTHS = new ChronoUnit("Months", 9, Duration.OneMonth, true)

  final val YEARS = new ChronoUnit("Years", 10, Duration.OneYear, true)

  final val DECADES = new ChronoUnit("Decades", 11,
      Duration.OneYear.multipliedBy(10), true)

  final val CENTURIES = new ChronoUnit("Centuries", 12,
      Duration.OneYear.multipliedBy(100), true)

  final val MILLENNIA = new ChronoUnit("Millenia", 13,
      Duration.OneYear.multipliedBy(1000), true)

  final val ERAS = new ChronoUnit("Eras", 14,
      Duration.OneYear.multipliedBy(1000000000), true)

  final val FOREVER = new ChronoUnit("Forever", 15, Duration.Max, true)

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
