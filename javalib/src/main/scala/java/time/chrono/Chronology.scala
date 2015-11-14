package java.time.chrono

import scala.collection.JavaConverters._
import scala.scalajs.js

import java.time.{Period, DateTimeException}
import java.time.temporal.{ValueRange, ChronoField, TemporalAccessor}
import java.{util => ju}

trait Chronology extends Comparable[Chronology] {
  def getId(): String

  def getCalendarType(): String

  def date(era: Era, yearOfEra: Int, month: Int, dayOfMonth: Int): ChronoLocalDate =
    date(prolepticYear(era, yearOfEra), month, dayOfMonth)

  def date(prolepticYear: Int, month: Int, dayOfMonth: Int): ChronoLocalDate

  def dateYearDay(era: Era, yearOfEra: Int, dayOfYear: Int): ChronoLocalDate =
    dateYearDay(prolepticYear(era, yearOfEra), dayOfYear)

  def dateYearDay(prolepticYear: Int, dayOfYear: Int): ChronoLocalDate

  def dateEpochDay(epochDay: Long): ChronoLocalDate

  def dateNow(): ChronoLocalDate = {
    val d = new js.Date()
    date(d.getFullYear, d.getMonth, d.getDate)
  }

  // Not implemented
  // def dateNow(zone: ZoneId): ChronoLocalDate
  // def dateNow(clock: Clock): ChronoLocalDate

  def date(temporal: TemporalAccessor): ChronoLocalDate

  // TODO
  // def localDateTime(temporal: TemporalAccessor): ChronoLocalDateTime[_]

  // Not implemented
  // def zonedDateTime(temporal: TemporalAccessor): ChronoZonedDateTime[_]
  // def zonedDateTime(instant: Instant, zone: ZoneId): ChronoZonedDateTime[_]

  def isLeapYear(prolepticYear: Long): Boolean

  def prolepticYear(era: Era, yearOfEra: Int): Int

  def eraOf(eraValue: Int): Era

  def eras(): ju.List[Era]

  def range(field: ChronoField): ValueRange

  // Not implemented
  // def getDisplayName(style: java.time.format.TextStyle,
  //     locale: ju.Locale): String

  // Not implemented
  // def resolveDate(fieldValues: ju.Map[TemporalField, Long],
  //     resolverStyle: java.time.format.ResolverStyle): ChronoLocalDate

  def period(years: Int, months: Int, days: Int): ChronoPeriod =
    Period.of(years, months, days)
}

object Chronology {
  // Not implemented
  // def from(temporal: TemporalAccessor): Chronology
  // def ofLocale(locale: ju.Locale): Chronology

  def of(id: String): Chronology = {
    getAvailableChronologies().asScala.find(_.getId == id).getOrElse {
      throw new DateTimeException(s"Unknown chronology: $id")
    }
  }

  def getAvailableChronologies(): ju.Set[Chronology] =
    Set[Chronology](IsoChronology.INSTANCE).asJava
}
