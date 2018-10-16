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
