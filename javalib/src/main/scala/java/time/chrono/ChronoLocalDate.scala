package java.time.chrono

import java.time.temporal._
import java.{util => ju}

trait ChronoLocalDate
    extends Temporal with TemporalAdjuster with Comparable[ChronoLocalDate] {
  import ChronoField._

  def getChronology(): Chronology

  def getEra(): Era = getChronology().eraOf(get(ERA))

  def isLeapYear(): Boolean = getChronology().isLeapYear(get(YEAR))

  def lengthOfMonth(): Int

  def lengthOfYear(): Int

  def isSupported(field: TemporalField): Boolean = field match {
    case _: ChronoField => field.isDateBased
    case null           => false
    case _              => field.isSupportedBy(this)
  }

  def isSupported(unit: TemporalUnit): Boolean = unit match {
    case _: ChronoUnit => unit.isDateBased
    case null          => false
    case _             => unit.isSupportedBy(this)
  }

  override def `with`(adjuster: TemporalAdjuster): ChronoLocalDate =
    adjuster.adjustInto(this).asInstanceOf[ChronoLocalDate]

  def `with`(field: TemporalField, value: Long): ChronoLocalDate = field match {
    case _: ChronoField =>
      throw new UnsupportedTemporalTypeException(s"Unsupported field: $field")

    case _ => field.adjustInto(this, value)
  }

  override def plus(amount: TemporalAmount): ChronoLocalDate =
    amount.addTo(this).asInstanceOf[ChronoLocalDate]

  def plus(amount: Long, unit: TemporalUnit): ChronoLocalDate = unit match {
    case _: ChronoUnit =>
      throw new UnsupportedTemporalTypeException(s"Unsupported unit: $unit")

    case _ => unit.addTo(this, amount)
  }

  override def minus(amount: TemporalAmount): ChronoLocalDate =
    amount.subtractFrom(this).asInstanceOf[ChronoLocalDate]

  override def minus(amount: Long, unit: TemporalUnit): ChronoLocalDate =
    if (amount != Long.MinValue) plus(-amount, unit)
    else plus(Long.MaxValue, unit).plus(1, unit)

  // Not implemented
  // def query[R](query: TemporalQuery[R]): R

  def adjustInto(temporal: Temporal): Temporal =
    temporal.`with`(EPOCH_DAY, toEpochDay)

  def until(end: Temporal, unit: TemporalUnit): Long

  def until(end: ChronoLocalDate): ChronoPeriod

  // Not implemented
  // def format(formatter: java.time.format.DateFormatter): String

  // TODO
  // def atTime(localTime: LocalTime): ChronoLocalDateTime[_]

  def toEpochDay(): Long = getLong(EPOCH_DAY)

  def compareTo(other: ChronoLocalDate): Int = {
    val r = toEpochDay.compareTo(other.toEpochDay)
    if (r == 0) getChronology().compareTo(other.getChronology)
    else r
  }

  def isAfter(other: ChronoLocalDate): Boolean =
    toEpochDay > other.toEpochDay

  def isBefore(other: ChronoLocalDate): Boolean =
    toEpochDay < other.toEpochDay

  def isEqual(other: ChronoLocalDate): Boolean =
    other.toEpochDay == toEpochDay

  override def equals(other: Any): Boolean = other match {
    case other: ChronoLocalDate =>
      isEqual(other) && getChronology == other.getChronology

    case _ => false
  }

  override def hashCode: Int = super.hashCode
}

object ChronoLocalDate {
  private val tlo = new ju.Comparator[ChronoLocalDate] {
    def compare(date1: ChronoLocalDate, date2: ChronoLocalDate): Int =
      date1.toEpochDay.compareTo(date2.toEpochDay)
  }

  def timeLineOrder(): ju.Comparator[ChronoLocalDate] = tlo

  def from(temporal: TemporalAccessor): ChronoLocalDate = temporal match {
    case temporal: ChronoLocalDate => temporal

    case _ =>
      // TODO: Get correct chronology (needs TemporalQuery)
      IsoChronology.INSTANCE.date(temporal)
  }
}
