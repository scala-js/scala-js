package java.time

import java.time.chrono.IsoChronology

import scala.scalajs.js

import java.time.temporal._

/** Created by alonsodomin on 22/12/2015. */
final class MonthDay private (month: Int, day: Int)
    extends TemporalAccessor with TemporalAdjuster with Comparable[MonthDay]
    with java.io.Serializable {

  import Preconditions._
  import ChronoField._

  requireDateTime(month >= 1 && month <= 12, s"Invalid month value: $month")
  requireDateTime(day >= 1 && day <= 31, s"Invalid day value: $day")

  def isSupported(field: TemporalField): Boolean = field match {
    case _: ChronoField => field == DAY_OF_MONTH || field == MONTH_OF_YEAR
    case null           => false
    case _              => field.isSupportedBy(this)
  }

  override def range(field: TemporalField): ValueRange = field match {
    case DAY_OF_MONTH => ValueRange.of(1, getMonth().minLength(), getMonth().maxLength())
    case _            => super.range(field)
  }

  // Implemented by TemporalAccessor
  // def get(field: TemporalField): Int

  def getLong(field: TemporalField): Long = field match {
    case DAY_OF_MONTH  => day
    case MONTH_OF_YEAR => month

    case _: ChronoField =>
      throw new UnsupportedTemporalTypeException(s"Field not supported: $field")

    case _ => field.getFrom(this)
  }

  def getMonthValue(): Int = month

  def getMonth(): Month = Month.of(month)

  def getDayOfMonth(): Int = day

  override def equals(other: Any): Boolean = other match {
    case that: MonthDay =>
      that.getMonthValue == getMonthValue() && that.getDayOfMonth == getDayOfMonth()

    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(month, day)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  def isValidYear(year: Int): Boolean = {
    if (month == 2 && day == 29)
      IsoChronology.INSTANCE.isLeapYear(year)
    else
      true
  }

  def `with`(month: Month): MonthDay = {
    require(month != null, s"'month' can not be null")
    if (month == getMonth()) {
      this
    } else {
      val dayOfMonth = js.Math.min(day, month.maxLength())
      MonthDay.of(month, dayOfMonth)
    }
  }

  def withMonth(month: Int): MonthDay = `with`(Month.of(month))

  def withDayOfMonth(dayOfMonth: Int): MonthDay = {
    requireDateTime(dayOfMonth <= getMonth().maxLength(),
        s"The day $dayOfMonth is invalid for mont $month")
    if (dayOfMonth == day) this
    else MonthDay.of(month, dayOfMonth)
  }

  // Not implemented
  // def query[R](query: TemporalQuery[R]): R

  def adjustInto(temporal: Temporal): Temporal = {
    val temporalWithMonth = temporal.`with`(MONTH_OF_YEAR, month)
    val dayOfMonth: Int = js.Math.min(day,
        temporalWithMonth.range(DAY_OF_MONTH).getMaximum.toInt)
    temporalWithMonth.`with`(DAY_OF_MONTH, dayOfMonth)
  }

  def atYear(year: Int): LocalDate =
    LocalDate.of(year, month, if (isValidYear(year)) day else 28)

  def compareTo(that: MonthDay): Int = {
    if (month == that.getMonthValue) day - that.getDayOfMonth
    else month - that.getMonthValue
  }

  def isAfter(that: MonthDay): Boolean = compareTo(that) > 0

  def isBefore(that: MonthDay): Boolean = compareTo(that) < 0

  override def toString(): String = f"--$month%02d-$day%02d"

  // Not implemented
  // def format(formatter: DateTimeFormatter): String

}

object MonthDay {

  import Preconditions._
  import ChronoField._

  def now(): MonthDay = {
    val date = LocalDate.now()
    of(date.getMonthValue, date.getDayOfMonth)
  }

  // Not implemented
  // def now(zoneId: ZoneId): MonthDay

  // Not implemented
  // def now(clock: Clock): MonthDay

  def of(month: Month, dayOfMonth: Int): MonthDay = {
    if (month == null)
      throw new NullPointerException("month")
    DAY_OF_MONTH.checkValidValue(dayOfMonth)
    requireDateTime(dayOfMonth <= month.maxLength(),
        s"The day $dayOfMonth is invalid for month $month")
    new MonthDay(month.getValue, dayOfMonth)
  }

  def of(month: Int, dayOfMonth: Int): MonthDay = {
    MONTH_OF_YEAR.checkValidIntValue(month)
    of(Month.of(month), dayOfMonth)
  }

  def from(accessor: TemporalAccessor): MonthDay = accessor match {
    case md: MonthDay => md
    case _ => of(accessor.get(MONTH_OF_YEAR), accessor.get(DAY_OF_MONTH))
  }

  // Not implemented
  // def parse(text: CharSequence): MonthDay

  // Not implemented
  // def parse(text: CharSequence, formatter: DateTimeFormatter): MonthDay

}
