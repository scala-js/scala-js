package java.lang

import scala.scalajs.js

/** Stripped down version of `java.lang.FloatingPointBits` with the bare
 *  minimum to support a correct `numberHashCode()`.
 *
 *  We cannot use the full `java.lang.FloatingPointBits` out of the box,
 *  because it internally uses typed arrays (and static facade types thereof)
 *  to implement a better `numberHashCode()`.
 */
private[lang] object FloatingPointBits {
  // Simpler version than the original, technically valid but of lesser quality
  def numberHashCode(value: scala.Double): Int =
    rawToInt(value)

  @inline private def rawToInt(x: scala.Double): Int =
    (x.asInstanceOf[js.Dynamic] | 0.asInstanceOf[js.Dynamic]).asInstanceOf[Int]
}
