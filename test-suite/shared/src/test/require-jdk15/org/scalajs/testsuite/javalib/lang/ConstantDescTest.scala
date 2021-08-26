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

package org.scalajs.testsuite.javalib.lang.constant

import org.junit.Test
import org.junit.Assert._

import java.lang.constant.ConstantDesc

import org.scalajs.testsuite.utils.Platform.executingInJVM

class ConstantDescTest {

  @Test def knownConstantDescs(): Unit = {
    def test(expected: Boolean, value: Any): Unit =
      assertEquals("" + value, expected, value.isInstanceOf[ConstantDesc])

    test(true, 100000)
    test(true, 5L)
    test(true, 1.5f)
    test(true, 1.4)
    test(true, "foo")

    // The following are transitively Integers, and therefore must be ConstantDescs
    test(!executingInJVM, 5.toByte)
    test(!executingInJVM, 300.toShort)

    // Nevertheless, their j.l.Class'es do not extend ConstantDesc
    assertFalse(classOf[ConstantDesc].isAssignableFrom(classOf[java.lang.Byte]))
    assertFalse(classOf[ConstantDesc].isAssignableFrom(classOf[java.lang.Short]))

    test(false, false)
    test(false, 'A')
    test(false, null)
    test(false, ())
    test(false, List(5))
  }

}
