package scala.scalajs.runtime

import java.lang.{Boolean => JBoolean}

/** Explicit box for boolean values when doing a reflective call.
 *  This class and its methods are only here to properly support reflective
 *  calls on booleans.
 */
class BooleanReflectiveCall(value: Boolean) {

  // Methods of java.lang.Boolean

  def booleanValue(): Boolean = value

  def compareTo(that: JBoolean): Int =
    new JBoolean(value).compareTo(that)
  def compareTo(that: AnyRef): Int =
    new JBoolean(value).compareTo(that.asInstanceOf[JBoolean])

  // Methods of scala.Boolean

  // scalastyle:off disallow.space.before.token
  def unary_! : Boolean = !value
  // scalastyle:on disallow.space.before.token

  def ==(x: Boolean): Boolean = value == x
  def !=(x: Boolean): Boolean = value != x
  def ||(x: Boolean): Boolean = value || x
  def &&(x: Boolean): Boolean = value && x
  def |(x: Boolean): Boolean = value | x
  def &(x: Boolean): Boolean = value & x
  def ^(x: Boolean): Boolean = value ^ x

}
