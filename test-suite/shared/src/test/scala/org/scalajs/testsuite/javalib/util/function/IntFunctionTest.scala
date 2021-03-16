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

package org.scalajs.testsuite.javalib.util.function

import org.junit.Assert._
import org.junit.Test

import java.util.function._

class IntFunctionTest {
  @Test def testApply(): Unit = {
    val repeat = new IntFunction[String] {
      override def apply(value: Int): String = "." * value
    }
    assertEquals(repeat.apply(1), ".")
    assertEquals(repeat.apply(3), "...")
  }
}
