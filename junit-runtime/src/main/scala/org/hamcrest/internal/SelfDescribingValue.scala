/*
 * Ported from https://github.com/hamcrest/JavaHamcrest/
 */
package org.hamcrest.internal

import org.hamcrest.Description
import org.hamcrest.SelfDescribing

class SelfDescribingValue[T](value: T) extends SelfDescribing {
  override def describeTo(description: Description): Unit =
    description.appendValue(value.asInstanceOf[AnyRef])
}
