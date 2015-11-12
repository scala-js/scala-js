/*
 * Ported from https://github.com/junit-team/junit
 */
package org.junit

import org.hamcrest.CoreMatchers.is
import org.hamcrest.CoreMatchers.notNullValue
import org.hamcrest.CoreMatchers.nullValue
import org.hamcrest.Matcher

object Assume {

  def assumeTrue(b: Boolean): Unit =
    assumeThat(b, is(true))

  def assumeFalse(b: Boolean): Unit =
    assumeTrue(!b)

  def assumeTrue(message: String, b: Boolean): Unit =
    if (!b) throw new AssumptionViolatedException(message)

  def assumeFalse(message: String, b: Boolean): Unit =
    assumeTrue(message, !b)

  def assumeNotNull(objects: AnyRef*): Unit =
    objects.foreach(assumeThat(_, notNullValue()))

  def assumeThat[T](actual: T, matcher: Matcher[T]): Unit = {
    if (!matcher.matches(actual.asInstanceOf[AnyRef]))
      throw new AssumptionViolatedException(actual, matcher)
  }

  def assumeThat[T](message: String, actual: T, matcher: Matcher[T]): Unit = {
    if (!matcher.matches(actual.asInstanceOf[AnyRef]))
      throw new AssumptionViolatedException(message, actual, matcher)
  }

  def assumeNoException(e: Throwable): Unit =
    assumeThat(e, nullValue())

  def assumeNoException(message: String, e: Throwable): Unit =
    assumeThat(message, e, nullValue())
}
