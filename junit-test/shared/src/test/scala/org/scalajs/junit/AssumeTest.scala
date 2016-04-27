package org.scalajs.junit

import org.junit.Assume._
import org.junit._

import org.scalajs.junit.utils._

class AssumeTest {
  @Test def assumeFail(): Unit = {
    assumeTrue("This assume should not pass", false)
  }
}

class AssumeTestAssertions extends JUnitTest with SuccessFrameworkArgs {

  override val expectedTotal: Int = 1

  protected def expectedOutput(context: OutputContext): List[Output] = {
    import context._
    List(
        testRunStartedOutput,
        testStartedOutput("assumeFail"),
        testAssumptionViolatedOutput("assumeFail"),
        skippedEvent,
        testFinishedOutput("assumeFail"),
        testRunFinishedOutput,
        done
    )
  }
}
