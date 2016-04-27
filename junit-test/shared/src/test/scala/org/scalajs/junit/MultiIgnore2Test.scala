package org.scalajs.junit

import org.junit._
import org.scalajs.junit.utils.{JUnitTest, SuccessFrameworkArgs}

class MultiIgnore2Test {
  @Ignore @Test def multiTest1(): Unit = ()
  @Test def multiTest2(): Unit = ()
  @Test def multiTest3(): Unit = ()
  @Ignore @Test def multiTest4(): Unit = ()
  @Test def multiTest5(): Unit = ()
}

class MultiIgnore2TestAssertions extends JUnitTest with SuccessFrameworkArgs {

  override val expectedIgnored: Int = 2
  override val expectedTotal: Int = 3

  protected def expectedOutput(context: OutputContext): List[Output] = {
    import context._
    List(
        testRunStartedOutput,
        testIgnoredOutput("multiTest1"),
        skippedEvent,
        testStartedOutput("multiTest2"),
        testFinishedOutput("multiTest2"),
        successEvent,
        testStartedOutput("multiTest3"),
        testFinishedOutput("multiTest3"),
        successEvent,
        testIgnoredOutput("multiTest4"),
        skippedEvent,
        testStartedOutput("multiTest5"),
        testFinishedOutput("multiTest5"),
        successEvent,
        testRunFinishedOutput,
        done
    )
  }
}
