/*
 * Ported from https://github.com/hamcrest/JavaHamcrest/
 */
package org.hamcrest.core

import org.hamcrest.BaseMatcher
import org.hamcrest.Description
import org.hamcrest.Matcher

import org.hamcrest.core.IsNot.not

class IsNull[T] extends BaseMatcher[T] {
  override def matches(o: AnyRef): Boolean =
    o == null

  override def describeTo(description: Description): Unit =
    description.appendText("null")
}

object IsNull {
  def nullValue(): Matcher[AnyRef] =
    new IsNull[AnyRef]

  def notNullValue(): Matcher[AnyRef] =
    not(nullValue())

  def nullValue[T](tpe: Class[T]): Matcher[T] =
    new IsNull[T]()

  def notNullValue[T](typ: Class[T]): Matcher[T] =
    not(nullValue(typ))
}
