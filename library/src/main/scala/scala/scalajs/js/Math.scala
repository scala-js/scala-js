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

import scala.scalajs.js

/**
 * Math is a built-in object that has properties and methods for mathematical
 * constants and functions. Not a function object.
 *
 * MDN
 */
@js.native
object Math extends Object {

  /**
   * Euler's constant and the base of natural logarithms, approximately 2.718.
   *
   * MDN
   */
  val E: Double = js.native

  /**
   * Natural logarithm of 10, approximately 2.303.
   *
   * MDN
   */
  val LN10: Double = js.native

  /**
   * Natural logarithm of 2, approximately 0.693.
   *
   * MDN
   */
  val LN2: Double = js.native

  /**
   * Base 2 logarithm of E, approximately 1.443.
   *
   * MDN
   */
  val LOG2E: Double = js.native

  /**
   * Base 10 logarithm of E, approximately 0.434.
   *
   * MSN
   */
  val LOG10E: Double = js.native

  /**
   * Ratio of the circumference of a circle to its diameter, approximately 3.14159.
   *
   * MDN
   */
  val PI: Double = js.native

  /**
   * Square root of 1/2; equivalently, 1 over the square root of 2, approximately 0.707.
   *
   * MDN
   */
  val SQRT1_2: Double = js.native

  /**
   * Square root of 2, approximately 1.414.
   *
   * MDN
   */
  val SQRT2: Double = js.native

  /**
   * Returns the absolute value of a number.
   *
   * Passing a non-numeric string or undefined/empty variable returns NaN.
   * Passing null returns 0.
   *
   * MDN
   */
  def abs(x: Int): Int = js.native

  /**
   * Returns the absolute value of a number.
   *
   * Passing a non-numeric string or undefined/empty variable returns NaN.
   * Passing null returns 0.
   *
   * MDN
   */
  def abs(x: Double): Double = js.native

  /**
   * The Math.acos() function returns the arccosine (in radians) of a number.
   *
   * The acos method returns a numeric value between 0 and pi radians for x
   * between -1 and 1. If the value of number is outside this range, it returns NaN.
   *
   * MDN
   */
  def acos(x: Double): Double = js.native

  /**
   * The Math.asin() function returns the arcsine (in radians) of a number.
   *
   * The asin method returns a numeric value between -pi/2 and pi/2 radians for x
   * between -1 and 1. If the value of number is outside this range, it returns NaN.
   *
   * MDN
   */
  def asin(x: Double): Double = js.native

  /**
   * The Math.atan() function returns the arctangent (in radians) of a number.
   *
   * The atan method returns a numeric value between -pi/2 and pi/2 radians.
   *
   * MDN
   */
  def atan(x: Double): Double = js.native

  /**
   * The Math.atan2() function returns the arctangent of the quotient of its
   * arguments.
   *
   * The atan2 method returns a numeric value between -pi and pi representing
   * the angle theta of an (x,y) point. This is the counterclockwise angle,
   * measured in radians, between the positive X axis, and the point (x,y).
   * Note that the arguments to this function pass the y-coordinate first and
   * the x-coordinate second.
   *
   * atan2 is passed separate x and y arguments, and atan is passed the ratio
   * of those two arguments.
   *
   * MDN
   */
  def atan2(y: Double, x: Double): Double = js.native

  /**
   * The Math.ceil() function returns the smallest integer greater than or
   * equal to a number.
   *
   * MDN
   */
  def ceil(x: Double): Double = js.native

  /**
   * The Math.cos() function returns the cosine of a number.
   *
   * The cos method returns a numeric value between -1 and 1, which represents
   * the cosine of the angle.
   *
   * MDN
   */
  def cos(x: Double): Double = js.native

  /**
   * The Math.exp() function returns E^x, where x is the argument, and E is
   * Euler's constant, the base of the natural logarithms.
   *
   * MDN
   */
  def exp(x: Double): Double = js.native

  /**
   * The Math.floor() function returns the largest integer less than or equal
   * to a number.
   *
   * MDN
   */
  def floor(x: Double): Double = js.native

  /**
   * The Math.log() function returns the natural logarithm (base E) of a number.
   *
   * If the value of number is negative, the return value is always NaN.
   *
   * MDN
   */
  def log(x: Double): Double = js.native

  /**
   * The Math.max() function returns the largest of zero or more numbers.
   *
   * MDN
   */
  def max(value1: Int, values: Int*): Int = js.native

  /**
   * The Math.max() function returns the largest of zero or more numbers.
   *
   * If no arguments are given, the result is - Infinity.
   *
   * If at least one of arguments cannot be converted to a number, the result is NaN.
   *
   * MDN
   */
  def max(values: Double*): Double = js.native

  /**
   * The Math.min() function returns the smallest of zero or more numbers.
   *
   * MDN
   */
  def min(value1: Int, values: Int*): Int = js.native

  /**
   * The Math.min() function returns the smallest of zero or more numbers.
   *
   * If no arguments are given, the result is Infinity.
   *
   * If at least one of arguments cannot be converted to a number, the result is NaN.
   *
   * MDN
   */
  def min(values: Double*): Double = js.native

  /**
   * The Math.pow() function returns the base to the exponent Power,  that is, base^^exponent.
   *
   * MDN
   */
  def pow(x: Double, y: Double): Double = js.native

  /**
   * The Math.random() function returns a floating-point, pseudo-random number in
   * the range [0, 1) that is, from 0 (inclusive) up to but not including 1
   * (exclusive), which you can then scale to your desired range.
   *
   * The random number generator is seeded from the current time, as in Java.
   *
   * MDN
   */
  def random(): Double = js.native

  /**
   * The Math.round() function returns the value of a number rounded to the
   * nearest integer.
   *
   * If the fractional portion of number is .5 or greater, the argument is
   * rounded to the next higher integer. If the fractional portion of number
   * is less than .5, the argument is rounded to the next lower integer.
   *
   * MDN
   */
  def round(x: Double): Double = js.native

  /**
   * The Math.sin() function returns the sine of a number.
   *
   * The sin method returns a numeric value between -1 and 1, which represents
   * the sine of the angle given in radians.
   *
   * MDN
   */
  def sin(x: Double): Double = js.native

  /**
   * The Math.sqrt() function returns the square root (x\sqrt{x}) of a number.
   *
   * If the value of number is negative, sqrt returns NaN.
   *
   * MDN
   */
  def sqrt(x: Double): Double = js.native

  /**
   * The Math.tan() function returns the tangent of a number.
   *
   * The tan method returns a numeric value that represents the tangent of the angle.
   *
   * MDN
   */
  def tan(x: Double): Double = js.native
}
