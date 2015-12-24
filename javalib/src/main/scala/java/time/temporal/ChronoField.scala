package java.time.temporal

import java.time.Year

final class ChronoField private (name: String, ordinal: Int,
    _range: ValueRange, baseUnit: ChronoUnit, rangeUnit: ChronoUnit, flags: Int)
    extends Enum[ChronoField](name, ordinal) with TemporalField {

  import ChronoField._

  // Not implemented:
  // def String getDisplayName(locale: java.util.Locale)

  def getBaseUnit(): TemporalUnit = baseUnit

  def getRangeUnit(): TemporalUnit = rangeUnit

  def range(): ValueRange = _range

  def isDateBased(): Boolean = (flags & isDateBasedFlag) != 0

  def isTimeBased(): Boolean = (flags & isTimeBasedFlag) != 0

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

  private final val isTimeBasedFlag = 1
  private final val isDateBasedFlag = 2

  final val NANO_OF_SECOND = new ChronoField("NanoOfSecond", 0,
      ValueRange.of(0, 999999999), NANOS, SECONDS, isTimeBasedFlag)

  final val NANO_OF_DAY = new ChronoField("NanoOfDay", 1,
      ValueRange.of(0, 86399999999999L), NANOS, DAYS, isTimeBasedFlag)

  final val MICRO_OF_SECOND = new ChronoField("MicroOfSecond", 2,
      ValueRange.of(0, 999999), MICROS, SECONDS, isTimeBasedFlag)

  final val MICRO_OF_DAY = new ChronoField("MicroOfDay", 3,
      ValueRange.of(0, 86399999999L), MICROS, DAYS, isTimeBasedFlag)

  final val MILLI_OF_SECOND = new ChronoField("MilliOfSecond", 4,
      ValueRange.of(0, 999), MILLIS, SECONDS, isTimeBasedFlag)

  final val MILLI_OF_DAY = new ChronoField("MilliOfDay", 5,
      ValueRange.of(0, 86399999), MILLIS, DAYS, isTimeBasedFlag)

  final val SECOND_OF_MINUTE = new ChronoField("SecondOfMinute", 6,
      ValueRange.of(0, 59), SECONDS, MINUTES, isTimeBasedFlag)

  final val SECOND_OF_DAY = new ChronoField("SecondOfDay", 7,
      ValueRange.of(0, 86399), SECONDS, DAYS, isTimeBasedFlag)

  final val MINUTE_OF_HOUR = new ChronoField("MinuteOfHour", 8,
      ValueRange.of(0, 59), MINUTES, HOURS, isTimeBasedFlag)

  final val MINUTE_OF_DAY = new ChronoField("MinuteOfDay", 9,
      ValueRange.of(0, 1439), MINUTES, DAYS, isTimeBasedFlag)

  final val HOUR_OF_AMPM = new ChronoField("HourOfAmPm", 10,
      ValueRange.of(0, 11), HOURS, HALF_DAYS, isTimeBasedFlag)

  final val CLOCK_HOUR_OF_AMPM = new ChronoField("ClockHourOfAmPm", 11,
      ValueRange.of(1, 12), HOURS, HALF_DAYS, isTimeBasedFlag)

  final val HOUR_OF_DAY = new ChronoField("HourOfDay", 12,
      ValueRange.of(0, 23), HOURS, DAYS, isTimeBasedFlag)

  final val CLOCK_HOUR_OF_DAY = new ChronoField("ClockHourOfDay", 13,
      ValueRange.of(1, 24), HOURS, DAYS, isTimeBasedFlag)

  final val AMPM_OF_DAY = new ChronoField("AmPmOfDay", 14,
      ValueRange.of(0, 1), HALF_DAYS, DAYS, isTimeBasedFlag)

  final val DAY_OF_WEEK = new ChronoField("DayOfWeek", 15,
      ValueRange.of(1, 7), DAYS, WEEKS, isDateBasedFlag)

  final val ALIGNED_DAY_OF_WEEK_IN_MONTH = new ChronoField("AlignedDayOfWeekInMonth",
      16, ValueRange.of(1, 7), DAYS, WEEKS, isDateBasedFlag)

  final val ALIGNED_DAY_OF_WEEK_IN_YEAR = new ChronoField("AlignedDayOfWeekInYear",
      17, ValueRange.of(1, 7), DAYS, WEEKS, isDateBasedFlag)

  final val DAY_OF_MONTH = new ChronoField("DayOfMonth", 18,
      ValueRange.of(1, 28, 31), DAYS, MONTHS, isDateBasedFlag)

  final val DAY_OF_YEAR = new ChronoField("DayOfYear", 19,
      ValueRange.of(1, 365, 366), DAYS, YEARS, isDateBasedFlag)

  final val EPOCH_DAY = new ChronoField("EpochDay", 20,
      ValueRange.of(-365249999634L, 365249999634L), DAYS, FOREVER, isDateBasedFlag)

  final val ALIGNED_WEEK_OF_MONTH = new ChronoField("AlignedWeekOfMonth", 21,
      ValueRange.of(1, 4, 5), WEEKS, MONTHS, isDateBasedFlag)

  final val ALIGNED_WEEK_OF_YEAR = new ChronoField("AlignedWeekOfYear", 22,
      ValueRange.of(1, 53), WEEKS, YEARS, isDateBasedFlag)

  final val MONTH_OF_YEAR = new ChronoField("MonthOfYear", 23,
      ValueRange.of(1, 12), MONTHS, YEARS, isDateBasedFlag)

  final val PROLEPTIC_MONTH = new ChronoField("ProlepticMonth", 24,
      ValueRange.of(-11999999988L, 11999999999L), MONTHS, FOREVER, isDateBasedFlag)

  final val YEAR_OF_ERA = new ChronoField("YearOfEra", 25,
      ValueRange.of(1, 999999999, 1000000000), YEARS, ERAS, isDateBasedFlag)

  final val YEAR = new ChronoField("Year", 26,
      ValueRange.of(Year.MIN_VALUE, Year.MAX_VALUE), YEARS, FOREVER, isDateBasedFlag)

  final val ERA = new ChronoField("Era", 27, ValueRange.of(0, 1), ERAS, FOREVER,
      isDateBasedFlag)

  final val INSTANT_SECONDS = new ChronoField("InstantSeconds", 28,
      ValueRange.of(Long.MinValue, Long.MaxValue), SECONDS, FOREVER, 0)

  final val OFFSET_SECONDS = new ChronoField("OffsetSeconds", 29,
      ValueRange.of(-64800, 64800), SECONDS, FOREVER, 0)

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
