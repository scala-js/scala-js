package org.scalajs.junit

import org.junit.Test

import org.scalajs.junit.utils._

class Multi1Test {
  @Test def multiTest1(): Unit = ()
  @Test def multiTest2(): Unit = ()
}

class Multi1TestAssertions extends JUnitTest with SuccessFrameworkArgs {

  override val expectedTotal: Int = 2

  protected def expectedOutput(context: OutputContext): List[Output] = {
    import context._
    List(
        testRunStartedOutput,
        testStartedOutput("multiTest1"),
        testFinishedOutput("multiTest1"),
        successEvent,
        testStartedOutput("multiTest2"),
        testFinishedOutput("multiTest2"),
        successEvent,
        testRunFinishedOutput,
        done
    )
  }
}
