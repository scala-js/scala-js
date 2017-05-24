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

class BeforeAssumeFailTestAssertions extends JUnitTest {
  protected def expectedOutput(builder: OutputBuilder): OutputBuilder =
    builder.ignoredClass()
}
