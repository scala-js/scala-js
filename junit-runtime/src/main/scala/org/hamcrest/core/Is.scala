/*
 * Ported from https://github.com/hamcrest/JavaHamcrest/
 */
package org.hamcrest.core

import org.hamcrest.BaseMatcher
import org.hamcrest.Description
import org.hamcrest.Matcher

import org.hamcrest.core.IsEqual.equalTo
import org.hamcrest.core.IsInstanceOf.instanceOf

class Is[T](matcher: Matcher[T]) extends BaseMatcher[T] {

  override def matches(arg: AnyRef): Boolean =
    matcher.matches(arg)

  override def describeTo(description: Description): Unit =
    description.appendText("is ").appendDescriptionOf(matcher)

  override def describeMismatch(item: AnyRef,
      mismatchDescription: Description): Unit = {
    matcher.describeMismatch(item, mismatchDescription)
  }
}

object Is {
  def is[T](matcher: Matcher[T]): Matcher[T] =
    new Is[T](matcher)

  def is[T](value: T): Matcher[T] =
    is(equalTo(value))

  def isA[T](typ: Class[T]): Matcher[T] =
    is(instanceOf(typ))
}
