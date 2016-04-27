package org.scalajs.junit

import org.junit.Assert._
import org.junit.Test
import org.scalajs.junit.utils._

class AssertEquals2Test {
  @Test def test(): Unit = {
    assertEquals("This is the message", false, true)
  }
}

class AssertEquals2TestAssertions extends JUnitTest with FailureFrameworkArgs {

  override val expectedFail: Int = 1
  override val expectedTotal: Int = 1

  protected def expectedOutput(context: OutputContext): List[Output] = {
    import context._
    List(
        testRunStartedOutput,
        testStartedOutput("test"),
        testAssertionErrorMsgOutput("test",
            "This is the message expected:<false> but was:<true>"),
        failureEvent,
        testFinishedOutput("test"),
        testRunFinishedOutput,
        done
    )
  }
}
