/*
 * Ported from https://github.com/junit-team/junit
 */
package org.junit

import org.hamcrest.Matcher

// @SuppressWarnings("deprecation")
class AssumptionViolatedException protected (fAssumption: String,
    fValueMatcher: Boolean, fMatcher: Matcher[_], fValue: AnyRef)
    extends org.junit.internal.AssumptionViolatedException(fAssumption,
        fValueMatcher, fMatcher, fValue) {

  @Deprecated
  def this(actual: Any,  matcher: Matcher[_]) =
    this(null, true, fMatcher = matcher, fValue = actual.asInstanceOf[AnyRef])

  @Deprecated
  def this(message: String, expected: Any, matcher: Matcher[_]) =
    this(message, true, fMatcher = matcher, fValue = expected.asInstanceOf[AnyRef])

  def this(message: String) =
    this(message, false, null, null)

  def this(assumption: String, t: Throwable) = {
    this(assumption, false, null, null)
    initCause(t)
  }
}
