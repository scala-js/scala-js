package java.time.temporal

final class ChronoField private (name: String, ordinal: Int,
    _range: ValueRange, dateBased: Boolean, baseUnit: ChronoUnit,
    rangeUnit: ChronoUnit) extends Enum[ChronoField](name, ordinal)
    with TemporalField {

  // Not implemented:
  // def String getDisplayName(locale: java.util.Locale)

  def getBaseUnit(): TemporalUnit = baseUnit

  def getRangeUnit(): TemporalUnit = rangeUnit

  def range(): ValueRange = _range

  def isDateBased(): Boolean = dateBased

  def isTimeBased(): Boolean = !dateBased

  def checkValidValue(value: Long): Long =
    _range.checkValidValue(value, this)

  def checkValidIntValue(value: Long): Int =
    _range.checkValidIntValue(value, this)

  def isSupportedBy(temporal: TemporalAccessor): Boolean =
    temporal.isSupported(this)

  def rangeRefinedBy(temporal: TemporalAccessor): ValueRange =
    temporal.range(this)

  def getFrom(temporal: TemporalAccessor): Long = temporal.getLong(this)

  def adjustInto[R <: Temporal](temporal: R, newValue: Long): R =
    temporal.`with`(this, newValue).asInstanceOf[R]
}

object ChronoField {

  import ChronoUnit._

  final val NANO_OF_SECOND = new ChronoField("NanoOfSecond", 0,
      ValueRange.of(0, 999999999), false, NANOS, SECONDS)

  final val NANO_OF_DAY = new ChronoField("NanoOfDay", 1,
      ValueRange.of(0, 86399999999999L), false, NANOS, DAYS)

  final val MICRO_OF_SECOND = new ChronoField("MicroOfSecond", 2,
      ValueRange.of(0, 999999), false, MICROS, SECONDS)

  final val MICRO_OF_DAY = new ChronoField("MicroOfDay", 3,
      ValueRange.of(0, 86399999999L), false, MICROS, DAYS)

  final val MILLI_OF_SECOND = new ChronoField("MilliOfSecond", 4,
      ValueRange.of(0, 999), false, MILLIS, SECONDS)

  final val MILLI_OF_DAY = new ChronoField("MilliOfDay", 5,
      ValueRange.of(0, 86399999), false, MILLIS, DAYS)

  final val SECOND_OF_MINUTE = new ChronoField("SecondOfMinute", 6,
      ValueRange.of(0, 59), false, SECONDS, MINUTES)

  final val SECOND_OF_DAY = new ChronoField("SecondOfDay", 7,
      ValueRange.of(0, 86399), false, SECONDS, DAYS)

  final val MINUTE_OF_HOUR = new ChronoField("MinuteOfHour", 8,
      ValueRange.of(0, 59), false, MINUTES, HOURS)

  final val MINUTE_OF_DAY = new ChronoField("MinuteOfDay", 9,
      ValueRange.of(0, 1439), false, MINUTES, DAYS)

  final val HOUR_OF_AMPM = new ChronoField("HourOfAmPm", 10,
      ValueRange.of(0, 11), false, HOURS, HALF_DAYS)

  final val CLOCK_HOUR_OF_AMPM = new ChronoField("ClockHourOfAmPm", 11,
      ValueRange.of(1, 12), false, HOURS, HALF_DAYS)

  final val HOUR_OF_DAY = new ChronoField("HourOfDay", 12,
      ValueRange.of(0, 23), false, HOURS, DAYS)

  final val CLOCK_HOUR_OF_DAY = new ChronoField("ClockHourOfDay", 13,
      ValueRange.of(1, 24), false, HOURS, DAYS)

  final val AMPM_OF_DAY = new ChronoField("AmPmOfDay", 14,
      ValueRange.of(0, 1), false, HALF_DAYS, DAYS)

  final val DAY_OF_WEEK = new ChronoField("DayOfWeek", 15,
      ValueRange.of(1, 7), true, DAYS, WEEKS)

  final val ALIGNED_DAY_OF_WEEK_IN_MONTH = new ChronoField("AlignedDayOfWeekInMonth",
      16, ValueRange.of(1, 7), true, DAYS, WEEKS)

  final val ALIGNED_DAY_OF_WEEK_IN_YEAR = new ChronoField("AlignedDayOfWeekInYear",
      17, ValueRange.of(1, 7), true, DAYS, WEEKS)

  final val DAY_OF_MONTH = new ChronoField("DayOfMonth", 18,
      ValueRange.of(1, 28, 31), true, DAYS, MONTHS)

  final val DAY_OF_YEAR = new ChronoField("DayOfYear", 19,
      ValueRange.of(1, 365, 366), true, DAYS, YEARS)

  final val EPOCH_DAY = new ChronoField("EpochDay", 20,
      ValueRange.of(-365249999634L, 365249999634L), true, DAYS, FOREVER)

  final val ALIGNED_WEEK_OF_MONTH = new ChronoField("AlignedWeekOfMonth", 21,
      ValueRange.of(1, 4, 5), true, WEEKS, MONTHS)

  final val ALIGNED_WEEK_OF_YEAR = new ChronoField("AlignedWeekOfYear", 22,
      ValueRange.of(1, 53), true, WEEKS, YEARS)

  final val MONTH_OF_YEAR = new ChronoField("MonthOfYear", 23,
      ValueRange.of(1, 12), true, MONTHS, YEARS)

  final val PROLEPTIC_MONTH = new ChronoField("ProlepticMonth", 24,
      ValueRange.of(-11999999988L, 11999999999L), true, MONTHS, FOREVER)

  final val YEAR_OF_ERA = new ChronoField("YearOfEra", 25,
      ValueRange.of(1, 999999999, 1000000000), true, YEARS, ERAS)

  final val YEAR = new ChronoField("Year", 26,
      ValueRange.of(-999999999, 999999999), true, YEARS, FOREVER)

  final val ERA = new ChronoField("Era", 27, ValueRange.of(0, 1), true, ERAS,
      FOREVER)

  final val INSTANT_SECONDS = new ChronoField("InstantSeconds", 28,
      ValueRange.of(Long.MinValue, Long.MaxValue), false, SECONDS, FOREVER)

  final val OFFSET_SECONDS = new ChronoField("OffsetSeconds", 29,
      ValueRange.of(-64800, 64800), false, SECONDS, FOREVER)

  private val fields = Array(NANO_OF_SECOND, NANO_OF_DAY, MICRO_OF_SECOND,
      MICRO_OF_DAY, MILLI_OF_SECOND, MILLI_OF_DAY, SECOND_OF_MINUTE,
      SECOND_OF_DAY, MINUTE_OF_HOUR, MINUTE_OF_DAY, HOUR_OF_AMPM,
      CLOCK_HOUR_OF_AMPM, HOUR_OF_DAY, CLOCK_HOUR_OF_DAY,
      AMPM_OF_DAY, DAY_OF_WEEK, ALIGNED_DAY_OF_WEEK_IN_MONTH,
      ALIGNED_DAY_OF_WEEK_IN_YEAR, DAY_OF_MONTH, DAY_OF_YEAR, EPOCH_DAY,
      ALIGNED_WEEK_OF_MONTH, ALIGNED_WEEK_OF_YEAR, MONTH_OF_YEAR, PROLEPTIC_MONTH,
      YEAR_OF_ERA, YEAR, ERA, INSTANT_SECONDS, OFFSET_SECONDS)

  def values(): Array[ChronoField] = fields.clone()

  def valueOf(name: String): ChronoField = {
    fields.find(f => name == toScreamingSnakeCase(f.name)).getOrElse {
      throw new IllegalArgumentException(s"No such field: $name")
    }
  }
}
