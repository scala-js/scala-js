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

package org.scalajs.testsuite.compiler

import org.junit.Test
import org.junit.Assert._

/** Test that `eq` and `ne` have the additional guarantees provided by
 *  Scala.js for instances of hijacked classes.
 *
 *  These tests would be too restrictive on the JVM.
 */
class EqJSTest {
  @Test
  def testEqNe(): Unit = {
    @noinline def testNoInline(expected: Boolean, x: Any, y: Any): Unit = {
      assertEquals(expected, x.asInstanceOf[AnyRef] eq y.asInstanceOf[AnyRef])
      assertEquals(!expected, x.asInstanceOf[AnyRef] ne y.asInstanceOf[AnyRef])
    }

    @inline def test(expected: Boolean, x: Any, y: Any): Unit = {
      testNoInline(expected, x, y)
      assertEquals(expected, x.asInstanceOf[AnyRef] eq y.asInstanceOf[AnyRef])
      assertEquals(!expected, x.asInstanceOf[AnyRef] ne y.asInstanceOf[AnyRef])
    }

    val o1 = new Object
    val o2 = new Object

    val t1 = ("foo", "bar")
    val t2 = ("foo", "bar")

    test(true, o1, o1)
    test(false, o1, o2)
    test(true, t1, t1)
    test(false, t1, t2)
    test(true, "foo", "foo")
    test(false, "foo", "bar")
    test(true, Double.NaN, Double.NaN)
    test(true, 0.0, 0.0)
    test(true, -0.0, -0.0)
    test(false, 0.0, -0.0)
    test(true, 0, 0.0)
    test(true, 5, 5)
    test(false, 5, 4)
  }
}
