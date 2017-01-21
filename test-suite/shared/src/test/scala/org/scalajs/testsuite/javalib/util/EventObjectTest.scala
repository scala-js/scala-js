/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2016, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import org.junit.Test
import org.junit.Assert._

import java.util.EventObject

class EventObjectTest {
  @Test def getSource(): Unit = {
    val src = new AnyRef
    val e = new EventObject(src)
    assertSame(src, e.getSource)
  }

  @Test def sourceField(): Unit = {
    class E(s: AnyRef) extends EventObject(s) {
      def setSource(s: AnyRef): Unit = source = s

      def otherGetSource: AnyRef = source
    }

    val src1 = new AnyRef
    val e = new E(src1)
    assertSame(src1, e.otherGetSource)
    val src2 = new AnyRef
    e.setSource(src2)
    assertSame(src2, e.otherGetSource)
    assertSame(src2, e.getSource)
  }

  @Test def testToString(): Unit = {
    /* There is not much we can test about toString, but it should not be the
     * default inherited from Object.
     */
    val e = new EventObject(new AnyRef)
    assertNotNull(e.toString())
    val default = classOf[EventObject].getName + "@" + Integer.toHexString(e.##)
    assertNotEquals(default, e.toString())
  }
}
