/*
 * Ported from https://github.com/hamcrest/JavaHamcrest/
 */
package org.hamcrest

abstract class BaseMatcher[T] extends Matcher[T] {
  override def describeMismatch(item: AnyRef, description: Description): Unit =
    description.appendText("was ").appendValue(item)

  override def toString(): String = StringDescription.toString(this)
}
