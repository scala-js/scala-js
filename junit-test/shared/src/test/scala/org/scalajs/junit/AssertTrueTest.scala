package org.scalajs.junit

import org.junit.Assert._
import org.junit.Test

import org.scalajs.junit.utils._

class AssertTrueTest {
  @Test def failTest(): Unit = {
    assertTrue(false)
  }

  @Test def successTest(): Unit = {
    assertTrue(true)
  }
}

class AssertTrueTestAssertions extends JUnitTest {
  protected def expectedOutput(builder: OutputBuilder): OutputBuilder =
  builder
    .success("successTest")
    .assertion("failTest", "null")
}
