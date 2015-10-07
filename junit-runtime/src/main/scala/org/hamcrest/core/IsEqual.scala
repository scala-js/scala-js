/*
 * Ported from https://github.com/hamcrest/JavaHamcrest/
 */
package org.hamcrest.core

import org.hamcrest.BaseMatcher
import org.hamcrest.Description
import org.hamcrest.Matcher

class IsEqual[T](expectedValue: AnyRef) extends BaseMatcher[T] {

  override def matches(actualValue: AnyRef): Boolean =
    IsEqual.areEqual(actualValue, expectedValue)

  override def describeTo(description: Description): Unit =
    description.appendValue(expectedValue)
}

object IsEqual {
  private[IsEqual] def areEqual(actual: AnyRef, expected: AnyRef): Boolean = {
    (actual, expected) match {
      case (null, _)                              => expected == null
      case (actual: Array[_], expected: Array[_]) => actual.toList == expected.toList
      case _                                      => actual.equals(expected)
    }
  }

  def equalTo[T](operand: T): Matcher[T] =
    new IsEqual[T](operand.asInstanceOf[AnyRef])

  def equalToObject(operand: AnyRef): Matcher[AnyRef] =
    new IsEqual[AnyRef](operand)
}
