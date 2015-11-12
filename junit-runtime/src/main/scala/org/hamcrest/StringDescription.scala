/*
 * Ported from https://github.com/hamcrest/JavaHamcrest/
 */
package org.hamcrest

import java.io.IOException
import java.lang.StringBuilder

object StringDescription {
  def toString(selfDescribing: SelfDescribing): String =
    new StringDescription().appendDescriptionOf(selfDescribing).toString()

  def asString(selfDescribing: SelfDescribing): String =
    toString(selfDescribing)
}

class StringDescription(out: Appendable = new StringBuilder())
    extends BaseDescription {
  override protected def append(str: String): Unit = {
    try {
      out.append(str)
    } catch {
      case e: IOException =>
        throw new RuntimeException("Could not write description", e)
    }
  }

  override protected def append(c: Char): Unit = {
    try {
      out.append(c)
    } catch {
      case e: IOException =>
        throw new RuntimeException("Could not write description", e)
    }
  }

  override def toString(): String =
    out.toString()
}
