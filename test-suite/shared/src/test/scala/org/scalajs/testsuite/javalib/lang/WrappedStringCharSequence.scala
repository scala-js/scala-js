/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.testsuite.javalib.lang

object WrappedStringCharSequence {
  def charSequence(content: String): CharSequence = new CharSequence {
    require(content != null)

    def length(): Int = content.length()

    def charAt(index: Int): Char = {
      if (index < 0 || index >= length())
        throw new StringIndexOutOfBoundsException(index)
      content.charAt(index)
    }

    def subSequence(start: Int, end: Int): CharSequence = {
      if (start < 0 || start > end || end > length())
        throw new StringIndexOutOfBoundsException()
      charSequence(content.substring(start, end))
    }

    override def toString(): String = content
  }
}
