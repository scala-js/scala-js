/*
 * Ported from https://github.com/junit-team/junit
 */
package org.junit.internal;

import org.hamcrest.Description
import org.hamcrest.Matcher
import org.hamcrest.SelfDescribing
import org.hamcrest.StringDescription

class AssumptionViolatedException protected(
    fAssumption: String,
    fValueMatcher: Boolean,
    fMatcher: Matcher[_],
    fValue: AnyRef) extends RuntimeException with SelfDescribing {

  //  @deprecated
  //  def this(assumption: String, hasValue: Boolean, value: AnyRef, matcher: Matcher[AnyRef]) {
  //    this(assumption, hasValue, matcher, value)
  //    if (value.isInstanceOf[Throwable]) {
  //      initCause(value.asInstanceOf[Throwable])
  //    }
  //  }

  //  @deprecated
  //  def this(value: AnyRef, matcher: Matcher[AnyRef]) {
  //    this(null, true, matcher, value)
  //  }

  //  @deprecated
  //  def this(assumption: String, value: AnyRef, matcher: Matcher[AnyRef]) {
  //    this(assumption, true, matcher, value)
  //  }

  //  @deprecated
  //  def this(assumption: String) {
  //    this(assumption, false, fMatcher = null, null)
  //  }

  //  @deprecated
  //  def this(assumption: String, e: Throwable) {
  //    this(assumption, false, fMatcher = null, null)
  //    initCause(e)
  //  }

  override def getMessage(): String =
    StringDescription.asString(this)

  def describeTo(description: Description): Unit = {
    if (fAssumption != null)
      description.appendText(fAssumption)

    if (fValueMatcher) {
      if (fAssumption != null)
        description.appendText(": ")

      description.appendText("got: ")
      description.appendValue(fValue)

      if (fMatcher != null) {
        description.appendText(", expected: ")
        description.appendDescriptionOf(fMatcher)
      }
    }
  }
}
