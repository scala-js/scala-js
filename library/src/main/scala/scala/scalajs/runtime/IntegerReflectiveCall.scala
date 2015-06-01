package scala.scalajs.runtime

import java.lang.{Double => JDouble, Integer => JInteger}

/** Explicit box for number values when doing a reflective call that was
 *  identified to be a call on Int rather than on Double (based on the
 *  result type of the method called reflectively).
 *  This class and its methods are only here to properly support reflective
 *  calls on numbers.
 */
class IntegerReflectiveCall(value: Int) {

  // Methods of scala.Int whose result type is different than in scala.Double

  // scalastyle:off disallow.space.before.token
  def unary_+ : scala.Int = value
  def unary_- : scala.Int = -value
  // scalastyle:on disallow.space.before.token

  def +(x: scala.Byte): scala.Int = value + x
  def +(x: scala.Short): scala.Int = value + x
  def +(x: scala.Char): scala.Int = value + x
  def +(x: scala.Int): scala.Int = value + x
  def +(x: scala.Long): scala.Long = value + x
  def +(x: scala.Float): scala.Float = value + x
  def +(x: scala.Double): scala.Double = value + x

  def -(x: scala.Byte): scala.Int = value - x
  def -(x: scala.Short): scala.Int = value - x
  def -(x: scala.Char): scala.Int = value - x
  def -(x: scala.Int): scala.Int = value - x
  def -(x: scala.Long): scala.Long = value - x
  def -(x: scala.Float): scala.Float = value - x
  def -(x: scala.Double): scala.Double = value - x

  def *(x: scala.Byte): scala.Int = value * x
  def *(x: scala.Short): scala.Int = value * x
  def *(x: scala.Char): scala.Int = value * x
  def *(x: scala.Int): scala.Int = value * x
  def *(x: scala.Long): scala.Long = value * x
  def *(x: scala.Float): scala.Float = value * x
  def *(x: scala.Double): scala.Double = value * x

  def /(x: scala.Byte): scala.Int = value / x
  def /(x: scala.Short): scala.Int = value / x
  def /(x: scala.Char): scala.Int = value / x
  def /(x: scala.Int): scala.Int = value / x
  def /(x: scala.Long): scala.Long = value / x
  def /(x: scala.Float): scala.Float = value / x
  def /(x: scala.Double): scala.Double = value / x

  def %(x: scala.Byte): scala.Int = value % x
  def %(x: scala.Short): scala.Int = value % x
  def %(x: scala.Char): scala.Int = value % x
  def %(x: scala.Int): scala.Int = value % x
  def %(x: scala.Long): scala.Long = value % x
  def %(x: scala.Float): scala.Float = value % x
  def %(x: scala.Double): scala.Double = value % x

  // Methods of scala.Int that are not defined on scala.Double

  // scalastyle:off disallow.space.before.token
  def unary_~ : scala.Int = ~value
  // scalastyle:on disallow.space.before.token

  def <<(x: scala.Int): scala.Int = value << x
  def <<(x: scala.Long): scala.Int = value << x
  def >>>(x: scala.Int): scala.Int = value >>> x
  def >>>(x: scala.Long): scala.Int = value >>> x
  def >>(x: scala.Int): scala.Int = value >> x
  def >>(x: scala.Long): scala.Int = value >> x

  def |(x: scala.Byte): scala.Int = value | x
  def |(x: scala.Short): scala.Int = value | x
  def |(x: scala.Char): scala.Int = value | x
  def |(x: scala.Int): scala.Int = value | x
  def |(x: scala.Long): scala.Long = value | x

  def &(x: scala.Byte): scala.Int = value & x
  def &(x: scala.Short): scala.Int = value & x
  def &(x: scala.Char): scala.Int = value & x
  def &(x: scala.Int): scala.Int = value & x
  def &(x: scala.Long): scala.Long = value & x

  def ^(x: scala.Byte): scala.Int = value ^ x
  def ^(x: scala.Short): scala.Int = value ^ x
  def ^(x: scala.Char): scala.Int = value ^ x
  def ^(x: scala.Int): scala.Int = value ^ x
  def ^(x: scala.Long): scala.Long = value ^ x

}
