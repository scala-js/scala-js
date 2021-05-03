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

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows.assertThrows

class StringTestOnJDK11 {
  @Test def repeat(): Unit = {
    assertThrows(classOf[IllegalArgumentException], "".repeat(-1))
    assertTrue("".repeat(0) == "")
    assertTrue("".repeat(1) == "")
    assertTrue("".repeat(100) == "")

    val str = "a_"
    assertThrows(classOf[IllegalArgumentException], str.repeat(-1))
    assertTrue(str.repeat(0) == "")
    assertTrue(str.repeat(1) == "a_")
    assertTrue(str.repeat(3) == "a_a_a_")
    assertTrue(str.repeat(10) == List.fill(10)(str).mkString(""))
    assertTrue(str.repeat(100) == List.fill(100)(str).mkString(""))
    assertTrue(str.repeat(1000) == List.fill(1000)(str).mkString(""))
  }
}
