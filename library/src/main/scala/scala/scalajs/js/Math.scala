/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

/** All doc-comments marked as "MDN" are by Mozilla Contributors,
 *  distributed under the Creative Commons Attribution-ShareAlike license from
 *  https://developer.mozilla.org/en-US/docs/Web/Reference/API
 */
package scala.scalajs.js

import scala.scalajs.js
import scala.scalajs.js.annotation._

/** Math is a built-in object that has properties and methods for mathematical
 *  constants and functions. Not a function object.
 *
 *  MDN
 */
@js.native
@JSGlobal
object Math extends js.Object {

  /** Euler's constant and the base of natural logarithms, approximately 2.718.
   *
   *  MDN
   */
  val E: Double = js.native

  /** Natural logarithm of 10, approximately 2.303.
   *
   *  MDN
   */
  val LN10: Double = js.native

  /** Natural logarithm of 2, approximately 0.693.
   *
   *  MDN
   */
  val LN2: Double = js.native

  /** Base 2 logarithm of E, approximately 1.443.
   *
   *  MDN
   */
  val LOG2E: Double = js.native

  /** Base 10 logarithm of E, approximately 0.434.
   *
   *  MSN
   */
  val LOG10E: Double = js.native

  /** Ratio of the circumference of a circle to its diameter, approximately 3.14159.
   *
   *  MDN
   */
  val PI: Double = js.native

  /** Square root of 1/2; equivalently, 1 over the square root of 2, approximately 0.707.
   *
   *  MDN
   */
  val SQRT1_2: Double = js.native

  /** Square root of 2, approximately 1.414.
   *
   *  MDN
   */
  val SQRT2: Double = js.native

  /** Returns the absolute value of a number.
   *
   *  Passing a non-numeric string or undefined/empty variable returns NaN.
   *  Passing null returns 0.
   *
   *  MDN
   */
  def abs(x: Int): Int = js.native

  /** Returns the absolute value of a number.
   *
   *  Passing a non-numeric string or undefined/empty variable returns NaN.
   *  Passing null returns 0.
   *
   *  MDN
   */
  def abs(x: Double): Double = js.native

  /** The Math.acos() function returns the arccosine (in radians) of a number.
   *
   *  The acos method returns a numeric value between 0 and pi radians for x
   *  between -1 and 1. If the value of number is outside this range, it returns NaN.
   *
   *  MDN
   */
  def acos(x: Double): Double = js.native

  /** The Math.asin() function returns the arcsine (in radians) of a number.
   *
   *  The asin method returns a numeric value between -pi/2 and pi/2 radians for x
   *  between -1 and 1. If the value of number is outside this range, it returns NaN.
   *
   *  MDN
   */
  def asin(x: Double): Double = js.native

  /** The Math.atan() function returns the arctangent (in radians) of a number.
   *
   *  The atan method returns a numeric value between -pi/2 and pi/2 radians.
   *
   *  MDN
   */
  def atan(x: Double): Double = js.native

  /** The Math.atan2() function returns the arctangent of the quotient of its
   *  arguments.
   *
   *  The atan2 method returns a numeric value between -pi and pi representing
   *  the angle theta of an (x,y) point. This is the counterclockwise angle,
   *  measured in radians, between the positive X axis, and the point (x,y).
   *  Note that the arguments to this function pass the y-coordinate first and
   *  the x-coordinate second.
   *
   *  atan2 is passed separate x and y arguments, and atan is passed the ratio
   *  of those two arguments.
   *
   *  MDN
   */
  def atan2(y: Double, x: Double): Double = js.native

  /** The Math.ceil() function returns the smallest integer greater than or
   *  equal to a number.
   *
   *  MDN
   */
  def ceil(x: Double): Double = js.native

  /** <span class="badge badge-ecma2015" style="float: right;">ECMAScript 2015</span>
   *
   *  The Math.clz32() function returns the number of leading zero bits in the
   *  32-bit binary representation of a number.
   *
   *  MDN
   */
  def clz32(x: Int): Int = js.native

  /** The Math.cos() function returns the cosine of a number.
   *
   *  The cos method returns a numeric value between -1 and 1, which represents
   *  the cosine of the angle.
   *
   *  MDN
   */
  def cos(x: Double): Double = js.native

  /** The Math.exp() function returns E^x, where x is the argument, and E is
   *  Euler's constant, the base of the natural logarithms.
   *
   *  MDN
   */
  def exp(x: Double): Double = js.native

  /** The Math.floor() function returns the largest integer less than or equal
   *  to a number.
   *
   *  MDN
   */
  def floor(x: Double): Double = js.native

  /** The Math.log() function returns the natural logarithm (base E) of a number.
   *
   *  If the value of number is negative, the return value is always NaN.
   *
   *  MDN
   */
  def log(x: Double): Double = js.native

  /** The Math.max() function returns the largest of zero or more numbers.
   *
   *  MDN
   */
  def max(value1: Int, values: Int*): Int = js.native

  /** The Math.max() function returns the largest of zero or more numbers.
   *
   *  If no arguments are given, the result is - Infinity.
   *
   *  If at least one of arguments cannot be converted to a number, the result is NaN.
   *
   *  MDN
   */
  def max(values: Double*): Double = js.native

  /** The Math.min() function returns the smallest of zero or more numbers.
   *
   *  MDN
   */
  def min(value1: Int, values: Int*): Int = js.native

  /** The Math.min() function returns the smallest of zero or more numbers.
   *
   *  If no arguments are given, the result is Infinity.
   *
   *  If at least one of arguments cannot be converted to a number, the result is NaN.
   *
   *  MDN
   */
  def min(values: Double*): Double = js.native

  /** The Math.pow() function returns the base to the exponent Power,  that is, base^^exponent.
   *
   *  MDN
   */
  def pow(x: Double, y: Double): Double = js.native

  /** The Math.random() function returns a floating-point, pseudo-random number in
   *  the range [0, 1) that is, from 0 (inclusive) up to but not including 1
   *  (exclusive), which you can then scale to your desired range.
   *
   *  The random number generator is seeded from the current time, as in Java.
   *
   *  MDN
   */
  def random(): Double = js.native

  /** The Math.round() function returns the value of a number rounded to the
   *  nearest integer.
   *
   *  If the fractional portion of number is .5 or greater, the argument is
   *  rounded to the next higher integer. If the fractional portion of number
   *  is less than .5, the argument is rounded to the next lower integer.
   *
   *  MDN
   */
  def round(x: Double): Double = js.native

  /** The Math.sin() function returns the sine of a number.
   *
   *  The sin method returns a numeric value between -1 and 1, which represents
   *  the sine of the angle given in radians.
   *
   *  MDN
   */
  def sin(x: Double): Double = js.native

  /** The Math.sqrt() function returns the square root (x\sqrt{x}) of a number.
   *
   *  If the value of number is negative, sqrt returns NaN.
   *
   *  MDN
   */
  def sqrt(x: Double): Double = js.native

  /** The Math.tan() function returns the tangent of a number.
   *
   *  The tan method returns a numeric value that represents the tangent of the angle.
   *
   *  MDN
   */
  def tan(x: Double): Double = js.native

  /** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
   *  The Math.log1p() function returns the natural logarithm (base e) of
   *  1 + a number
   *
   *  @return The natural logarithm (base e) of 1 plus the given number.
   *         If the number is less than -1, NaN is returned.
   *  MDN
   */
  def log1p(x: Double): Double = js.native

  /** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
   *  The Math.log10() function returns the base 10 logarithm of a number
   *
   *  @return The base 10 logarithm of the given number. If the number is
   *         negative, NaN is returned.
   *
   *  MDN
   */
  def log10(x: Double): Double = js.native

  /** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
   *  The Math.sinh() function returns the hyperbolic sine of a number
   *
   *  @return The hyperbolic sine of the given number
   *
   *  MDN
   */
  def sinh(x: Double): Double = js.native

  /** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
   *  The Math.cosh() function returns the hyperbolic cosine of a number
   *
   *  @return The hyperbolic cosine of the given number
   *
   *  MDN
   */
  def cosh(x: Double): Double = js.native

  /** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
   *  The Math.tanh() function returns the hyperbolic tangent of a number
   *
   *  @return The hyperbolic tangent of the given number
   *
   *  MDN
   */
  def tanh(x: Double): Double = js.native

  /** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
   *  The Math.cbrt() function returns the cube root of a number
   *
   *  @return The cube root of the given number.
   *
   *  MDN
   */
  def cbrt(x: Double): Double = js.native

  /** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
   *  The Math.hypot() function returns the square root of the sum of squares
   *  of its arguments
   *
   *  @return The square root of the sum of squares of the given arguments.
   *
   *  MDN
   */
  def hypot(x: Double*): Double = js.native

  /** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
   *  The Math.expm1() function returns e^x - 1, where x is the argument,
   *  and e the base of the natural logarithms.
   *
   *  @return A number representing e^x - 1, where e is Euler's number and
   *         x is the argument.
   */
  def expm1(x: Double): Double = js.native
}
