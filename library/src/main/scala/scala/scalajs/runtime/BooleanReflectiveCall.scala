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
    JBoolean.valueOf(value).compareTo(that)
  def compareTo(that: AnyRef): Int =
    JBoolean.valueOf(value).compareTo(that.asInstanceOf[JBoolean])

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
