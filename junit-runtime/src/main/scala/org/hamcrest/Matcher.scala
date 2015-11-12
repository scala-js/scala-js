/*
 * Ported from https://github.com/hamcrest/JavaHamcrest/
 */
package org.hamcrest

trait Matcher[+T] extends SelfDescribing {
  def matches(item: AnyRef): Boolean

  def describeMismatch(item: AnyRef, mismatchDescription: Description): Unit
}
