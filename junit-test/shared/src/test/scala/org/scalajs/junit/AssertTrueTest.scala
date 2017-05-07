package org.scalajs.junit

import org.junit.Assert._
import org.junit.Test

import org.scalajs.junit.utils._

class AssertTrueTest {
  @Test def test(): Unit = {
    assertTrue(false)
  }
}

class AssertTrueTestAssertions extends JUnitTest {
  protected def expectedOutput(builder: OutputBuilder): OutputBuilder =
    builder.assertion("test", "null")
}
