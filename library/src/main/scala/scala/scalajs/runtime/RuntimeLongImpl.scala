package scala.scalajs.runtime

import scala.annotation.tailrec

/** Independent object holding cached constants and non-inlineable methods
 *  for RuntimeLong. These constants and methods are extracted here so that the
 *  RuntimeLong module itself is elideable.
 */
private[runtime] object RuntimeLongImpl {
  import RuntimeLong.{
    toRuntimeLong,
    fromInt,
    TWO_PWR_63_DBL,
    TWO_PWR_22_DBL,
    TWO_PWR_44_DBL
  }

  // Do not make these 'final' vals. The goal is to cache the instances.
  val Zero     = toRuntimeLong(0L)
  val One      = toRuntimeLong(1L)
  val MinValue = toRuntimeLong(Long.MinValue)
  val MaxValue = toRuntimeLong(Long.MaxValue)
  val TenPow9  = toRuntimeLong(1000000000L) // 9 zeros

  def fromDouble(value: Double): RuntimeLong = {
    if (java.lang.Double.isNaN(value)) Zero
    else if (value < -TWO_PWR_63_DBL) MinValue
    else if (value >= TWO_PWR_63_DBL) MaxValue
    else if (value < 0) -fromDouble(-value)
    else {
      var acc = value
      val a2 = if (acc >= TWO_PWR_44_DBL) (acc / TWO_PWR_44_DBL).toInt else 0
      acc -= a2 * TWO_PWR_44_DBL
      val a1 = if (acc >= TWO_PWR_22_DBL) (acc / TWO_PWR_22_DBL).toInt else 0
      acc -= a1 * TWO_PWR_22_DBL
      val a0 = acc.toInt
      RuntimeLong(a0, a1, a2)
    }
  }

  def fromString(str: String, radix: Int): RuntimeLong = {
    if (str.isEmpty) {
      throw new java.lang.NumberFormatException(
          s"""For input string: "$str"""")
    } else if (str.charAt(0) == '-') {
      -fromString(str.substring(1), radix)
    } else {
      import scalajs.js

      val maxLen = 9
      @tailrec
      def fromString0(str0: String, acc: RuntimeLong): RuntimeLong = if (str0.length > 0) {
        val cur = (str0: js.prim.String).substring(0, maxLen): String
        val macc = acc * fromInt(math.pow(radix, cur.length).toInt)
        val ival = js.parseInt(cur, radix)
        if (js.isNaN(ival)) {
          throw new java.lang.NumberFormatException(
            s"""For input string: "$str"""")
        }
        val cval = fromInt(ival.toInt)
        fromString0((str0: js.prim.String).substring(maxLen), macc + cval)
      } else acc

      fromString0(str, Zero)
    }
  }
}
