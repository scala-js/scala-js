package org.scalajs.junit

import org.junit.Test

import org.scalajs.junit.utils._

class Multi2Test {
  @Test def multiTest1(): Unit = ()
  @Test def multiTest2(): Unit = ()
  @Test def multiTest3(): Unit = ()
  @Test def multiTest4(): Unit = ()
  @Test def multiTest5(): Unit = ()
}

class Multi2TestAssertions extends JUnitTest with SuccessFrameworkArgs {

  override val expectedTotal: Int = 5

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
        testStartedOutput("multiTest3"),
        testFinishedOutput("multiTest3"),
        successEvent,
        testStartedOutput("multiTest4"),
        testFinishedOutput("multiTest4"),
        successEvent,
        testStartedOutput("multiTest5"),
        testFinishedOutput("multiTest5"),
        successEvent,
        testRunFinishedOutput,
        done
    )
  }
}
