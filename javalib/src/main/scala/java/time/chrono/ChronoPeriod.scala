package java.time.chrono

import scala.collection.JavaConverters._

import java.time.temporal.{Temporal, TemporalAmount}

trait ChronoPeriod extends TemporalAmount {
  def getChronology(): Chronology

  def isZero(): Boolean = getUnits.asScala.forall(get(_) == 0)

  def isNegative(): Boolean = getUnits.asScala.exists(get(_) < 0)

  def plus(amount: TemporalAmount): ChronoPeriod

  def minus(amount: TemporalAmount): ChronoPeriod

  def multipliedBy(scalar: Int): ChronoPeriod

  def negated(): ChronoPeriod = multipliedBy(-1)

  def normalized(): ChronoPeriod

  def addTo(temporal: Temporal): Temporal

  def subtractFrom(temporal: Temporal): Temporal
}

object ChronoPeriod {
  def between(start: ChronoLocalDate, end: ChronoLocalDate): ChronoPeriod =
    start.until(end)
}
