package org.scalajs.junit

import org.junit.Assume._
import org.junit._

import org.scalajs.junit.utils._

class AssumeTest {
  @Test def assumeFail(): Unit = {
    assumeTrue("This assume should not pass", false)
  }
}

class AssumeTestAssertions extends JUnitTest {
  protected def expectedOutput(builder: OutputBuilder): OutputBuilder =
    builder.assumptionViolated("assumeFail")
}
