package java
package lang

import scala.js._
import JSDynamic.window

object Math {
  private trait MathStatic extends JSObject {
    val E: JSNumber
    val PI: JSNumber

    def abs(a: JSNumber): JSNumber
    def acos(a: JSNumber): JSNumber
    def asin(a: JSNumber): JSNumber
    def atan(a: JSNumber): JSNumber
    def atan2(a: JSNumber, b: JSNumber): JSNumber
    def ceil(a: JSNumber): JSNumber
    def cos(a: JSNumber): JSNumber
    def exp(a: JSNumber): JSNumber
    def floor(a: JSNumber): JSNumber
    def max(values: JSNumber*): JSNumber
    def min(values: JSNumber*): JSNumber
    def pow(a: JSNumber, b: JSNumber): JSNumber
    def random(): JSNumber
    def round(a: JSNumber): JSNumber
    def sin(a: JSNumber): JSNumber
    def sqrt(a: JSNumber): JSNumber
    def tan(a: JSNumber): JSNumber
  }

  private val JSMath = window.Math.asInstanceOf[MathStatic]

  val E: scala.Double = JSMath.E.asInstanceOf[JSNumber]
  val PI: scala.Double = JSMath.PI.asInstanceOf[JSNumber]

  def abs(a: scala.Int) = JSMath.abs(a).toInt
  def abs(a: scala.Long) = JSMath.abs(a).toLong
  def abs(a: scala.Float) = JSMath.abs(a).toFloat
  def abs(a: scala.Double) = JSMath.abs(a).toDouble

  def max(a: scala.Int, b: scala.Int) = JSMath.max(a, b).toInt
  def max(a: scala.Long, b: scala.Long) = JSMath.max(a, b).toLong
  def max(a: scala.Float, b: scala.Float) = JSMath.max(a, b).toFloat
  def max(a: scala.Double, b: scala.Double) = JSMath.max(a, b).toDouble

  def min(a: scala.Int, b: scala.Int) = JSMath.min(a, b).toInt
  def min(a: scala.Long, b: scala.Long) = JSMath.min(a, b).toLong
  def min(a: scala.Float, b: scala.Float) = JSMath.min(a, b).toFloat
  def min(a: scala.Double, b: scala.Double) = JSMath.min(a, b).toDouble

  def ceil(a: scala.Double): scala.Double = JSMath.ceil(a)
  def floor(a: scala.Double): scala.Double = JSMath.floor(a)

  def round(a: scala.Float): scala.Int = JSMath.round(a).toInt
  def round(a: scala.Double): scala.Long = JSMath.round(a).toLong

  def sqrt(a: scala.Double): scala.Double = JSMath.sqrt(a)
  def pow(a: scala.Double, b: scala.Double) = JSMath.pow(a, b)

  def sin(a: scala.Double): scala.Double = JSMath.sin(a)
  def cos(a: scala.Double): scala.Double = JSMath.cos(a)
  def tan(a: scala.Double): scala.Double = JSMath.tan(a)
  def asin(a: scala.Double): scala.Double = JSMath.asin(a)
  def acos(a: scala.Double): scala.Double = JSMath.acos(a)
  def atan(a: scala.Double): scala.Double = JSMath.atan(a)
  def atan2(y: scala.Double, x: scala.Double): scala.Double = JSMath.atan2(y, x)

  def random(): scala.Double = JSMath.random()

  // TODO The methods not available in the JavaScript Math object
}
