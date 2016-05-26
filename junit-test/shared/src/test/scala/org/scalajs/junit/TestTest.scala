package org.scalajs.junit

import org.junit.Test
import org.scalajs.junit.utils.{JUnitTest, SuccessFrameworkArgs}

class TestTest {
  @Test def onlyTest(): Unit = ()
}

class TestTestAssertions extends JUnitTest with SuccessFrameworkArgs {

  override val expectedTotal: Int = 1

  protected def expectedOutput(context: OutputContext): List[Output] = {
    import context._
    List(
        testRunStartedOutput,
        testStartedOutput("onlyTest"),
        testFinishedOutput("onlyTest"),
        successEvent,
        testRunFinishedOutput,
        done
    )
  }
}
