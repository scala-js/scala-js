package java.time.temporal

trait TemporalAccessor {
  def isSupported(field: TemporalField): Boolean

  def range(field: TemporalField): ValueRange = field match {
    case _: ChronoField if isSupported(field) => field.range

    case _: ChronoField =>
      throw new UnsupportedTemporalTypeException(s"Unsupported field: $field")

    case _ => field.rangeRefinedBy(this)
  }

  def get(field: TemporalField): Int = {
    val r = range(field)
    val msg = s"Invalid field $field for get() method, use getLong() instead"
    if (r.isIntValue) r.checkValidIntValue(getLong(field), field)
    else throw new UnsupportedTemporalTypeException(msg)
  }

  def getLong(field: TemporalField): Long

  // Not implemented
  // def query[R](query: TemporalQuery[R]): R
}
