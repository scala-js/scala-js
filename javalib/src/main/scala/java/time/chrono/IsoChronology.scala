package java.time.chrono

import scala.collection.JavaConverters._

import java.time.{Period, LocalDate}
import java.time.temporal.{ValueRange, ChronoField, TemporalAccessor}
import java.{util => ju}

final class IsoChronology private () extends AbstractChronology with Serializable {
  def getId(): String = "ISO"

  def getCalendarType(): String = "iso8601"

  override def date(era: Era, yearOfEra: Int, month: Int, dayOfMonth: Int): LocalDate =
    date(prolepticYear(era, yearOfEra), month, dayOfMonth)

  def date(prolepticYear: Int, month: Int, dayOfMonth: Int): LocalDate =
    LocalDate.of(prolepticYear, month, dayOfMonth)

  override def dateYearDay(era: Era, yearOfEra: Int, dayOfYear: Int): LocalDate =
    dateYearDay(prolepticYear(era, yearOfEra), dayOfYear)

  def dateYearDay(prolepticYear: Int, dayOfYear: Int): LocalDate =
    LocalDate.ofYearDay(prolepticYear, dayOfYear)

  def dateEpochDay(epochDay: Long): LocalDate = LocalDate.ofEpochDay(epochDay)

  def date(temporal: TemporalAccessor): LocalDate = LocalDate.from(temporal)

  // TODO
  // def localDateTime(temporal: TemporalAccessor): LocalDateTime

  // Not implemented
  // def zonedDateTime(temporal: TemporalAccessor): ZonedDateTime
  // def zonedDateTime(instant: Instant, zone: ZoneId): ZonedDateTime

  override def dateNow(): LocalDate = LocalDate.now()

  // Not implemented
  // def dateNow(zone: ZoneId): ChronoLocalDate
  // def dateNow(clock: Clock): ChronoLocalDate

  def isLeapYear(prolepticYear: Long): Boolean = {
    (prolepticYear % 400 == 0) ||
    (prolepticYear % 4 == 0 && prolepticYear % 100 != 0)
  }

  def prolepticYear(era: Era, yearOfEra: Int): Int = era match {
    case IsoEra.CE  => yearOfEra
    case IsoEra.BCE => 1 - yearOfEra
    case _          => throw new ClassCastException("Era must be IsoEra")
  }

  def eraOf(eraValue: Int): IsoEra = IsoEra.of(eraValue)

  def eras(): ju.List[Era] = Seq[Era](IsoEra.BCE, IsoEra.CE).asJava

  // Not implemented
  // def resolveDate(fieldValues: ju.Map[TemporalField, Long],
  //     resolverStyle: java.time.format.ResolverStyle): ChronoLocalDate

  def range(field: ChronoField): ValueRange = field.range()

  override def period(years: Int, months: Int, days: Int): Period =
    Period.of(years, months, days)
}

object IsoChronology {
  final val INSTANCE = new IsoChronology()
}
