package java.time.chrono

abstract class AbstractChronology protected () extends Chronology {
  // Not implemented
  // def resolveDate(fieldValues: ju.Map[TemporalField, Long],
  //     resolverStyle: java.time.format.ResolverStyle): ChronoLocalDate

  def compareTo(other: Chronology): Int = getId().compareTo(other.getId())

  override def equals(that: Any): Boolean = that match {
    case c: Chronology => compareTo(c) == 0
    case _             => false
  }

  override def hashCode(): Int = getId().hashCode()

  override def toString: String = getId()
}
