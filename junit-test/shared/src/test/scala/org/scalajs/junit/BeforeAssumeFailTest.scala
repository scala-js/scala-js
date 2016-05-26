package org.scalajs.junit

import org.junit.Assume._
import org.junit._

import org.scalajs.junit.utils._

object BeforeAssumeFailTest {
  @BeforeClass def beforeClass(): Unit = {
    assumeTrue("This assume should not pass", false)
  }
}

class BeforeAssumeFailTest {
  @Test def test(): Unit = ()
}

class BeforeAssumeFailTestAssertions extends JUnitTest with SuccessFrameworkArgs {

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
