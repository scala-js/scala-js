package org.scalajs.junit

import org.junit.Assert._
import org.junit.Test

import org.scalajs.junit.utils._

class AssertFalseTest {
  @Test def test(): Unit = {
    assertFalse(true)
  }
}

class AssertFalseTestAssertions extends JUnitTest {
  protected def expectedOutput(builder: OutputBuilder): OutputBuilder =
    builder.assertion("test", "null")
}
