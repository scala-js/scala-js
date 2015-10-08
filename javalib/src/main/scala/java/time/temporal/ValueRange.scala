package java.time.temporal

import java.time.DateTimeException

final class ValueRange private (minSmallest: Long, minLargest: Long,
    maxSmallest: Long, maxLargest: Long) extends java.io.Serializable {

  require(maxSmallest >= minSmallest,
      "Smallest maximum value must be >= smallest minimum value")
  require(maxLargest >= minLargest,
      "Largest maximum value must be >= largest minimum value")
  require(maxLargest >= maxSmallest,
      "Largest maximum value must be >= smallest maximum value")
  require(minLargest >= minSmallest,
      "Largest minimum value must be >= smallest minimum value")

  def isFixed(): Boolean =
    (minSmallest == minLargest) && (maxSmallest == maxLargest)

  def getMinimum(): Long = minSmallest

  def getLargestMinimum(): Long = minLargest

  def getSmallestMaximum(): Long = maxSmallest

  def getMaximum(): Long = maxLargest

  def isIntValue(): Boolean =
    minSmallest.isValidInt && maxLargest.isValidInt

  def isValidValue(value: Long): Boolean =
    (minSmallest <= value) && (value <= maxLargest)

  def isValidIntValue(value: Long): Boolean =
    isIntValue && isValidValue(value)

  def checkValidValue(value: Long, field: TemporalField): Long =
    if (isValidValue(value)) value
    else throw new DateTimeException(s"Invalid value for $field: $value")

  def checkValidIntValue(value: Long, field: TemporalField): Int =
    if (isValidIntValue(value)) value.toInt
    else throw new DateTimeException(s"Invalid value for $field: $value")

  override def toString(): String = {
    val prefix = minSmallest.toString +
        (if (minSmallest == minLargest) "" else "/" + minLargest)
    val suffix = maxSmallest.toString +
        (if (maxSmallest == maxLargest) "" else "/" + maxLargest)
    prefix + " - " + suffix
  }

  override def equals(that: Any): Boolean = that match {
    case that: ValueRange =>
      getMinimum == that.getMinimum &&
      getLargestMinimum == that.getLargestMinimum &&
      getSmallestMaximum == that.getSmallestMaximum &&
      getMaximum == that.getMaximum

    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(minSmallest, minLargest, maxSmallest, maxLargest)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object ValueRange {
  def of(min: Long, max: Long): ValueRange =
    new ValueRange(min, min, max, max)

  def of(min: Long, maxSmallest: Long, maxLargest: Long): ValueRange =
    new ValueRange(min, min, maxSmallest, maxLargest)

  def of(minSmallest: Long, minLargest: Long,
         maxSmallest: Long, maxLargest: Long): ValueRange =
    new ValueRange(minSmallest, minLargest, maxSmallest, maxLargest)
}
