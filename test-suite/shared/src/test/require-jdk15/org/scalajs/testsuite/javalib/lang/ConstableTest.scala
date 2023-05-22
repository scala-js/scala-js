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

import java.lang.constant.Constable

class ConstableTest {

  @Test def knownConstables(): Unit = {
    def test(expected: Boolean, value: Any): Unit = {
      assertEquals("" + value, expected, value.isInstanceOf[Constable])
    }

    test(true, false)
    test(true, 'A')
    test(true, 5.toByte)
    test(true, 300.toShort)
    test(true, 100000)
    test(true, 5L)
    test(true, 1.5f)
    test(true, 1.4)
    test(true, "foo")
    test(true, classOf[Product])

    test(false, null)
    test(false, ())
    test(false, List(5))
  }

}
