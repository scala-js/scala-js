package java.time

import scala.collection.JavaConverters._

import java.time.chrono.{IsoChronology, ChronoPeriod}
import java.time.temporal._
import java.{util => ju}

final class Period private (years: Int, months: Int, days: Int)
    extends ChronoPeriod with Serializable {
  import ChronoUnit._

  def get(unit: TemporalUnit): Long = unit match {
    case YEARS  => years
    case MONTHS => months
    case DAYS   => days

    case _ =>
      throw new UnsupportedTemporalTypeException(s"Unsupported unit: $unit")
  }

  def getUnits(): ju.List[TemporalUnit] =
    Seq[TemporalUnit](YEARS, MONTHS, DAYS).asJava

  def getChronology(): IsoChronology = IsoChronology.INSTANCE

  override def isZero(): Boolean =
    years == 0 && months == 0 && days == 0

  override def isNegative(): Boolean =
    years < 0 || months < 0 || days < 0

  def getYears(): Int = years

  def getMonths(): Int = months

  def getDays(): Int = days

  def withYears(years: Int): Period = new Period(years, months, days)

  def withMonths(months: Int): Period = new Period(years, months, days)

  def withDays(days: Int): Period = new Period(years, months, days)

  def plus(amount: TemporalAmount): Period = {
    val other = Period.from(amount)
    val years1 = MathJDK8Bridge.addExact(years, other.getYears)
    val months1 = MathJDK8Bridge.addExact(months, other.getMonths)
    val days1 = MathJDK8Bridge.addExact(days, other.getDays)
    new Period(years1, months1, days1)
  }

  def plusYears(yearsToAdd: Long): Period = {
    val years1 = MathJDK8Bridge.addExact(years, yearsToAdd)
    new Period(MathJDK8Bridge.toIntExact(years1), months, days)
  }

  def plusMonths(monthsToAdd: Long): Period = {
    val months1 = MathJDK8Bridge.addExact(months, monthsToAdd)
    new Period(years, MathJDK8Bridge.toIntExact(months1), days)
  }

  def plusDays(daysToAdd: Long): Period = {
    val days1 = MathJDK8Bridge.addExact(days, daysToAdd)
    new Period(years, months, MathJDK8Bridge.toIntExact(days1))
  }

  def minus(amount: TemporalAmount): Period = {
    val other = Period.from(amount)
    val years1 = MathJDK8Bridge.subtractExact(years, other.getYears)
    val months1 = MathJDK8Bridge.subtractExact(months, other.getMonths)
    val days1 = MathJDK8Bridge.subtractExact(days, other.getDays)
    new Period(years1, months1, days1)
  }

  def minusYears(yearsToSubtract: Long): Period = {
    val years1 = MathJDK8Bridge.subtractExact(years, yearsToSubtract)
    new Period(MathJDK8Bridge.toIntExact(years1), months, days)
  }

  def minusMonths(monthsToSubtract: Long): Period = {
    val months1 = MathJDK8Bridge.subtractExact(months, monthsToSubtract)
    new Period(years, MathJDK8Bridge.toIntExact(months1), days)
  }

  def minusDays(daysToSubtract: Long): Period = {
    val days1 = MathJDK8Bridge.subtractExact(days, daysToSubtract)
    new Period(years, months, MathJDK8Bridge.toIntExact(days1))
  }

  def multipliedBy(scalar: Int): Period = {
    val years1 = MathJDK8Bridge.multiplyExact(years, scalar)
    val months1 = MathJDK8Bridge.multiplyExact(months, scalar)
    val days1 = MathJDK8Bridge.multiplyExact(days, scalar)
    new Period(years1, months1, days1)
  }

  override def negated(): Period = multipliedBy(-1)

  def normalized(): Period = {
    val quot = months / 12
    val months1 = months % 12
    val years1 = MathJDK8Bridge.addExact(years, quot)
    if (years1 > 0 && months1 < 0)
      new Period(years1 - 1, months1 + 12, days)
    else if (years1 < 0 && months1 > 0)
      new Period(years1 + 1, months1 - 12, days)
    else
      new Period(years1, months1, days)
  }

  def toTotalMonths(): Long = years.toLong * 12 + months

  def addTo(temporal: Temporal): Temporal = {
    // TODO: Check chronology (needs TemporalQuery)
    val t1 = {
      if (months == 0 && years != 0) temporal.plus(years, YEARS)
      else if (months != 0) temporal.plus(toTotalMonths, MONTHS)
      else temporal
    }
    if (days != 0) t1.plus(days, DAYS) else t1
  }

  def subtractFrom(temporal: Temporal): Temporal = {
    // TODO: Check chronology (needs TemporalQueries)
    val t1 = {
      if (months == 0 && years != 0) temporal.minus(years, YEARS)
      else if (months != 0) temporal.minus(toTotalMonths, MONTHS)
      else temporal
    }
    if (days != 0) t1.minus(days, DAYS) else t1
  }

  override def equals(other: Any): Boolean = other match {
    case that: Period =>
      years == that.getYears && months == that.getMonths && days == that.getDays

    case _ => false
  }

  override def hashCode(): Int =
    31 * (31 * years.hashCode + months.hashCode) + days.hashCode

  override def toString(): String = {
    if (isZero) "P0D"
    else {
      val yearPart = if (years != 0) years.toString + "Y" else ""
      val monthPart = if (months != 0) months.toString + "M" else ""
      val dayPart = if (days != 0) days.toString + "D" else ""
      "P" + yearPart + monthPart + dayPart
    }
  }
}

object Period {
  final lazy val ZERO = of(0, 0, 0)

  def ofYears(years: Int): Period = of(years, 0, 0)

  def ofMonths(months: Int): Period = of(0, months, 0)

  def ofWeeks(weeks: Int): Period = ofDays(weeks * 7)

  def ofDays(days: Int): Period = of(0, 0, days)

  def of(years: Int, months: Int, days: Int): Period =
    new Period(years, months, days)

  def from(amount: TemporalAmount): Period = amount match {
    case amount: Period => amount

    case _ =>
      amount.getUnits().asScala.foldLeft(ZERO) { (p, unit) =>
        unit match {
          case ChronoUnit.YEARS =>
            p.withYears(MathJDK8Bridge.toIntExact(amount.get(unit)))

          case ChronoUnit.MONTHS =>
            p.withMonths(MathJDK8Bridge.toIntExact(amount.get(unit)))

          case ChronoUnit.DAYS =>
            p.withDays(MathJDK8Bridge.toIntExact(amount.get(unit)))

          case _ =>
            throw new DateTimeException(s"Unit not allowed: $unit")
        }
      }
  }

  def between(start: LocalDate, end: LocalDate): Period = start.until(end)

  // TODO
  // def parse(text: CharSequence): Period
}
