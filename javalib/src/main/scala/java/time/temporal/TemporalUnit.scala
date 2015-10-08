package java.time.temporal

import java.time.{DateTimeException, Duration}

trait TemporalUnit {
  def getDuration(): Duration

  def isDurationEstimated(): Boolean

  def isDateBased(): Boolean

  def isTimeBased(): Boolean

  def isSupportedBy(temporal: Temporal): Boolean = {
    try {
      temporal.plus(1, this)
      true
    } catch {
      case _: DateTimeException => false
      case _: ArithmeticException => true
    }
  }

  def addTo[R <: Temporal](temporal: R, amount: Long): R

  def between(start: Temporal, end: Temporal): Long
}
