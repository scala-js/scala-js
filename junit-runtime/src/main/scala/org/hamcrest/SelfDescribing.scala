/*
 * Ported from https://github.com/hamcrest/JavaHamcrest/
 */
package org.hamcrest

trait SelfDescribing {
  def describeTo(description: Description): Unit
}
