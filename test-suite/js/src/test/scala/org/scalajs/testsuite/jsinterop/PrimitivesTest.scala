/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.jsinterop

import org.junit.Assert._
import org.junit.Test

class PrimitivesTest {

  @Test def should_convert_Java_boxed_types_to_js_Any(): Unit = {
    assertEquals(false, new java.lang.Boolean(false))
    assertNull(null: java.lang.Boolean)

    assertEquals(42, new java.lang.Byte(42.toByte))
    assertNull(null: java.lang.Byte)

    assertEquals(42, new java.lang.Short(42.toShort))
    assertNull(null: java.lang.Short)

    assertEquals(42, new java.lang.Integer(42))
    assertNull(null: java.lang.Integer)

    assertEquals(42L, new java.lang.Long(42L))
    assertNull(null: java.lang.Long)

    assertEquals(42.0f, new java.lang.Float(42.0f), 0.0f)
    assertNull(null: java.lang.Float)

    assertEquals(42.0, new java.lang.Double(42.0), 0.0)
    assertNull(null: java.lang.Double)
  }

}
