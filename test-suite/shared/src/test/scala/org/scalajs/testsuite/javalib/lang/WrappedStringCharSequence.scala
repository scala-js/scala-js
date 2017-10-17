/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
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
