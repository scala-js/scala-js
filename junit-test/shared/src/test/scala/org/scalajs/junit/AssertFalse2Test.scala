package org.scalajs.junit

import org.junit.Assert._
import org.junit.Test

import org.scalajs.junit.utils._

class AssertFalse2Test {
  @Test def test(): Unit = {
    assertFalse("This is the message", true)
  }
}

class AssertFalse2TestAssertions extends JUnitTest with FailureFrameworkArgs {

  override val expectedFail: Int = 1
  override val expectedTotal: Int = 1

  protected def expectedOutput(context: OutputContext): List[Output] = {
    import context._
    List(
        testRunStartedOutput,
        testStartedOutput("test"),
        testAssertionErrorMsgOutput("test", "This is the message"),
        failureEvent,
        testFinishedOutput("test"),
        testRunFinishedOutput,
        done
    )
  }
}
