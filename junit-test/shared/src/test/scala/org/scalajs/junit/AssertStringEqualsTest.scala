package org.scalajs.junit

import org.junit.Assert._
import org.junit.Test

import org.scalajs.junit.utils._

class AssertStringEqualsTest {
  @Test def test(): Unit = {
    assertEquals("foobar", "foobbbr")
  }
}

class AssertStringEqualsTestAssertions extends JUnitTest {
  protected def expectedOutput(builder: OutputBuilder): OutputBuilder = {
    builder.assertion("test",
        "expected:<foob[a]r> but was:<foob[bb]r>",
        classOf[org.junit.ComparisonFailure])
  }
}
