package java.time.temporal

trait Temporal extends TemporalAccessor {
  def isSupported(unit: TemporalUnit): Boolean

  def `with`(adjuster: TemporalAdjuster): Temporal = adjuster.adjustInto(this)

  def `with`(field: TemporalField, newValue: Long): Temporal

  def plus(amount: TemporalAmount): Temporal = amount.addTo(this)

  def plus(amount: Long, unit: TemporalUnit): Temporal

  def minus(amount: TemporalAmount): Temporal = amount.subtractFrom(this)

  def minus(amount: Long, unit: TemporalUnit): Temporal =
    if (amount == Long.MinValue) plus(Long.MaxValue, unit).plus(1, unit)
    else plus(-amount, unit)

  def until(end: Temporal, unit: TemporalUnit): Long
}
