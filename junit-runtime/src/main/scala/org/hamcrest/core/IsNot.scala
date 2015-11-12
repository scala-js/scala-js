/*
 * Ported from https://github.com/hamcrest/JavaHamcrest/
 */
package org.hamcrest.core

import org.hamcrest.BaseMatcher
import org.hamcrest.Description
import org.hamcrest.Matcher

import org.hamcrest.core.IsEqual.equalTo

class IsNot[T](matcher: Matcher[T]) extends BaseMatcher[T] {
  override def matches(arg: AnyRef): Boolean =
    !matcher.matches(arg)

  override def describeTo(description: Description): Unit =
    description.appendText("not ").appendDescriptionOf(matcher)
}

object IsNot {
  def not[T](matcher: Matcher[T]): Matcher[T] =
    new IsNot[T](matcher)

  def not[T](value: T): Matcher[T] =
    not(equalTo(value))
}
