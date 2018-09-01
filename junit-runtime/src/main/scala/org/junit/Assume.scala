/*
 * Ported from https://github.com/junit-team/junit
 */
package org.junit

import org.hamcrest.CoreMatchers.is
import org.hamcrest.CoreMatchers.notNullValue
import org.hamcrest.CoreMatchers.nullValue
import org.hamcrest.Matcher

object Assume {

  @noinline
  def assumeTrue(b: Boolean): Unit =
    assumeThat(b, is(true))

  @noinline
  def assumeFalse(b: Boolean): Unit =
    assumeTrue(!b)

  @noinline
  def assumeTrue(message: String, b: Boolean): Unit =
    if (!b) throw new AssumptionViolatedException(message)

  @noinline
  def assumeFalse(message: String, b: Boolean): Unit =
    assumeTrue(message, !b)

  @noinline
  def assumeNotNull(objects: AnyRef*): Unit =
    objects.foreach(assumeThat(_, notNullValue()))

  @noinline
  def assumeThat[T](actual: T, matcher: Matcher[T]): Unit = {
    if (!matcher.matches(actual.asInstanceOf[AnyRef]))
      throw new AssumptionViolatedException(actual, matcher)
  }

  @noinline
  def assumeThat[T](message: String, actual: T, matcher: Matcher[T]): Unit = {
    if (!matcher.matches(actual.asInstanceOf[AnyRef]))
      throw new AssumptionViolatedException(message, actual, matcher)
  }

  @noinline
  def assumeNoException(e: Throwable): Unit =
    assumeThat(e, nullValue())

  @noinline
  def assumeNoException(message: String, e: Throwable): Unit =
    assumeThat(message, e, nullValue())
}
