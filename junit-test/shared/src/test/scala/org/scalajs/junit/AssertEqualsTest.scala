package org.scalajs.junit

import org.junit.Assert._
import org.junit.Test

import org.scalajs.junit.utils._

class AssertEqualsTest {
  @Test def test(): Unit = {
    assertEquals(false, true)
  }
}

class AssertEqualsTestAssertions extends JUnitTest {
  protected def expectedOutput(builder: OutputBuilder): OutputBuilder =
    builder.assertion("test", "expected:<false> but was:<true>")
}
