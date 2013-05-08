package java
package lang

import scala.js

object Math {
  private trait MathStatic extends js.Object {
    val E: js.Number
    val PI: js.Number

    def abs(a: js.Number): js.Number
    def acos(a: js.Number): js.Number
    def asin(a: js.Number): js.Number
    def atan(a: js.Number): js.Number
    def atan2(a: js.Number, b: js.Number): js.Number
    def ceil(a: js.Number): js.Number
    def cos(a: js.Number): js.Number
    def exp(a: js.Number): js.Number
    def floor(a: js.Number): js.Number
    def max(values: js.Number*): js.Number
    def min(values: js.Number*): js.Number
    def pow(a: js.Number, b: js.Number): js.Number
    def random(): js.Number
    def round(a: js.Number): js.Number
    def sin(a: js.Number): js.Number
    def sqrt(a: js.Number): js.Number
    def tan(a: js.Number): js.Number
  }

  private val JSMath = js.Dynamic.global.Math.asInstanceOf[MathStatic]

  val E: scala.Double = JSMath.E
  val PI: scala.Double = JSMath.PI

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
  def pow(a: scala.Double, b: scala.Double): scala.Double = JSMath.pow(a, b)

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
