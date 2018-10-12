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

package org.scalajs.junit

import org.junit.Assert._
import org.junit.Test

import org.scalajs.junit.utils._

class AssertEqualsDoubleTest {
  @Test def failsWithDouble(): Unit = {
    assertEquals(1.0, 1.0)
  }

  @Test def failsWithDoubleMessage(): Unit = {
    assertEquals("Message", 1.0, 1.0)
  }

  @Test def worksWithEpsilon(): Unit = {
    assertEquals(1.0, 1.0, 0.1)
    assertEquals("Message", 1.0, 1.0, 0.1)
  }

  @Test def worksWithByte(): Unit = {
    // This is supposed to take the (long, long) overload.
    assertEquals(1.toByte, 1.toByte)
  }

  @Test def worksWithShort(): Unit = {
    // This is supposed to take the (long, long) overload.
    assertEquals(2.toShort, 2.toShort)
  }

  @Test def worksWithInt(): Unit = {
    // This is supposed to take the (long, long) overload.
    assertEquals(1, 1)
  }
}

class AssertEqualsDoubleTestAssertions extends JUnitTest {
  protected def expectedOutput(builder: OutputBuilder): OutputBuilder = {
    builder
      .success("worksWithEpsilon")
      .assertion("failsWithDouble",
          "Use assertEquals(expected, actual, delta) to compare floating-point numbers")
      .assertion("failsWithDoubleMessage",
          "Use assertEquals(expected, actual, delta) to compare floating-point numbers")
      .success("worksWithByte")
      .success("worksWithShort")
      .success("worksWithInt")
  }
}
