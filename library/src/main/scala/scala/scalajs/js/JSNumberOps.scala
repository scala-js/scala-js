/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


/**
 * All doc-comments marked as "MDN" are by Mozilla Contributors,
 * distributed under the Creative Commons Attribution-ShareAlike license from
 * https://developer.mozilla.org/en-US/docs/Web/Reference/API
 */
package scala.scalajs.js

import scala.language.implicitConversions

import scala.scalajs.js.annotation._

/** Operations on JavaScript numbers. */
@native
trait JSNumberOps extends Any {

  def toString(radix: Int): String = native

  /**
   * Returns a string representation of number that does not use exponential
   * notation and has exactly digits digits after the decimal place. The number
   * is rounded if necessary, and the fractional part is padded with zeros if
   * necessary so that it has the specified length. If number is greater than
   * 1e+21, this method simply calls Number.prototype.toString() and returns
   * a string in exponential notation.
   *
   * MDN
   */
  def toFixed(fractionDigits: Int): String = native
  def toFixed(): String = native

  /**
   * Returns a string representing a Number object in exponential notation with one
   * digit before the decimal point, rounded to fractionDigits digits after the
   * decimal point. If the fractionDigits argument is omitted, the number of
   * digits after the decimal point defaults to the number of digits necessary
   * to represent the value uniquely.
   *
   * If a number has more digits that requested by the fractionDigits parameter,
   * the number is rounded to the nearest number represented by fractionDigits
   * digits. See the discussion of rounding in the description of the toFixed()
   * method, which also applies to toExponential().
   *
   * MDN
   */
  def toExponential(fractionDigits: Int): String = native
  def toExponential(): String = native

  /**
   * Returns a string representing a Number object in fixed-point or exponential
   * notation rounded to precision significant digits. See the discussion of
   * rounding in the description of the Number.prototype.toFixed() method, which
   * also applies to toPrecision.
   *
   * If the precision argument is omitted, behaves as Number.prototype.toString().
   * If it is a non-integer value, it is rounded to the nearest integer.
   *
   * MDN
   */
  def toPrecision(precision: Int): String = native
  def toPrecision(): String = native
}

object JSNumberOps {
  implicit def enableJSNumberOps(x: Int): JSNumberOps =
    x.asInstanceOf[JSNumberOps]

  implicit def enableJSNumberOps(x: Double): JSNumberOps =
    x.asInstanceOf[JSNumberOps]

  implicit def enableJSNumberExtOps(x: Int): ExtOps =
    new ExtOps(x.asInstanceOf[Dynamic])

  implicit def enableJSNumberExtOps(x: Double): ExtOps =
    new ExtOps(x.asInstanceOf[Dynamic])

  final class ExtOps(val self: Dynamic) extends AnyVal {
    @inline def toUint: Double =
      (self >>> 0.asInstanceOf[Dynamic]).asInstanceOf[Double]
  }

  /* The following overloads make sure that the developer does not use JS
   * number operations on a Long by error.
   */

  @deprecated("A Long is converted to Double to perform JavaScript "+
      "operations. This is almost certainly not what you want. "+
      "Use `.toDouble` explicitly if you need it.", "0.6.0")
  implicit def enableJSNumberOps(x: Long): JSNumberOps =
    x.toDouble.asInstanceOf[JSNumberOps]

  @deprecated("A Long is converted to Double to perform JavaScript "+
      "operations. This is almost certainly not what you want. "+
      "Use `.toDouble` explicitly if you need it.", "0.6.0")
  implicit def enableJSNumberExtOps(x: Long): ExtOps =
    new ExtOps(x.toDouble.asInstanceOf[Dynamic])
}
