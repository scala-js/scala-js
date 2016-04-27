package org.scalajs.junit

import org.junit._
import org.scalajs.junit.utils.{JUnitTest, SuccessFrameworkArgs}

class MultiIgnoreAllTest {
  @Ignore @Test def multiTest1(): Unit = ()
  @Ignore @Test def multiTest2(): Unit = ()
  @Ignore @Test def multiTest3(): Unit = ()
  @Ignore @Test def multiTest4(): Unit = ()
  @Ignore @Test def multiTest5(): Unit = ()
}

class MultiIgnoreAllTestAssertions extends JUnitTest with SuccessFrameworkArgs {

  override val expectedIgnored: Int = 5

  protected def expectedOutput(context: OutputContext): List[Output] = {
    import context._
    List(
        testRunStartedOutput,
        testIgnoredOutput("multiTest1"),
        skippedEvent,
        testIgnoredOutput("multiTest2"),
        skippedEvent,
        testIgnoredOutput("multiTest3"),
        skippedEvent,
        testIgnoredOutput("multiTest4"),
        skippedEvent,
        testIgnoredOutput("multiTest5"),
        skippedEvent,
        testRunFinishedOutput,
        done
    )
  }
}
