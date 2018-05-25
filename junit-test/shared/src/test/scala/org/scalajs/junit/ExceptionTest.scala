package org.scalajs.junit

import org.junit.Test

import org.scalajs.junit.utils._

class ExceptionTest {
  @Test def test(): Unit = {
    throw new IndexOutOfBoundsException("Exception message")
  }
}

class ExceptionTestAssertions extends JUnitTest {
  protected def expectedOutput(builder: OutputBuilder): OutputBuilder = {
    builder.exception("test",
        "Exception message",
        classOf[IndexOutOfBoundsException])
  }
}
