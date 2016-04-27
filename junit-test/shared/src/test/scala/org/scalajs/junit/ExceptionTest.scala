package org.scalajs.junit

import org.junit.Test

import org.scalajs.junit.utils._

class ExceptionTest {
  @Test def test(): Unit = {
    throw new IndexOutOfBoundsException("Exception message")
  }
}

class ExceptionTestAssertions extends JUnitTest with FailureFrameworkArgs {

  override val expectedFail: Int = 1
  override val expectedTotal: Int = 1

  protected def expectedOutput(context: OutputContext): List[Output] = {
    import context._
    List(
        testRunStartedOutput,
        testStartedOutput("test"),
        testExceptionMsgOutput("test", "Exception message",
            "java.lang", "IndexOutOfBoundsException"),
        failureEvent,
        testFinishedOutput("test"),
        testRunFinishedOutput,
        done
    )
  }
}
