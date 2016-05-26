package org.scalajs.junit

import org.junit.Assert._
import org.junit.Test

import org.scalajs.junit.utils._

class AssertTrueTest {
  @Test def test(): Unit = {
    assertTrue(false)
  }
}

class AssertTrueTestAssertions extends JUnitTest with FailureFrameworkArgs {

  override val expectedFail: Int = 1
  override val expectedTotal: Int = 1

  protected def expectedOutput(context: OutputContext): List[Output] = {
    import context._
    List(
        testRunStartedOutput,
        testStartedOutput("test"),
        testAssertionErrorMsgOutput("test", "null"),
        failureEvent,
        testFinishedOutput("test"),
        testRunFinishedOutput,
        done
    )
  }
}
