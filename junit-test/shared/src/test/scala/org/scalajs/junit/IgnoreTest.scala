package org.scalajs.junit

import org.junit._
import org.scalajs.junit.utils.{JUnitTest, SuccessFrameworkArgs}

class IgnoreTest {
  @Ignore @Test def onlyTest(): Unit = ()
}

class IgnoreTestAssertions extends JUnitTest with SuccessFrameworkArgs {

  override val expectedIgnored: Int = 1

  protected def expectedOutput(context: OutputContext): List[Output] = {
    import context._
    List(
        testRunStartedOutput,
        testIgnoredOutput("onlyTest"),
        skippedEvent,
        testRunFinishedOutput,
        done
    )
  }
}
