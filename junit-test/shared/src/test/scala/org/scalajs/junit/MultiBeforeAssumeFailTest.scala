package org.scalajs.junit

import org.junit.Assume._
import org.junit._

import org.scalajs.junit.utils.{JUnitTest, SuccessFrameworkArgs}

object MultiBeforeAssumeFailTest {
  @BeforeClass def beforeClass(): Unit = {
    assumeTrue("This assume should not pass", false)
  }
}

class MultiBeforeAssumeFailTest {
  @Test def multiTest1(): Unit = ()
  @Test def multiTest2(): Unit = ()
  @Test def multiTest3(): Unit = ()
  @Test def multiTest4(): Unit = ()
  @Test def multiTest5(): Unit = ()
}

class MultiBeforeAssumeFailTestAssertions
    extends JUnitTest with SuccessFrameworkArgs {

  override val expectedIgnored: Int = 1

  protected def expectedOutput(context: OutputContext): List[Output] = {
    import context._
    List(
        testRunStartedOutput,
        testIgnoredClassOutput,
        skippedEvent,
        testRunFinishedOutput,
        done
    )
  }
}
