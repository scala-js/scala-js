package scala.scalajs.compiler

object JSConversions {
  // Boldly copied from library/ scala.scalajs.runtime.Long
  
  /** number of relevant bits in each Long.l and Long.m */
  private val BITS:    Int = 22
  /** number of relevant bits in Long.l and Long.m together */
  private val BITS01:  Int = 2 * BITS
  /** number of relevant bits in Long.h */
  private val BITS2:   Int = 64 - BITS01
  /** bitmask for Long.l and Long.m */
  private val MASK:    Int = (1 << BITS) - 1
  /** bitmask for Long.h */
  private val MASK_2:  Int = (1 << BITS2) - 1
  
  private[scalajs] def scalaLongToTriplet(value: scala.Long) =
	(value.toInt & MASK, (value >> BITS).toInt & MASK, (value >> BITS01).toInt & MASK_2)
}