package org.scalajs.junit

import org.junit._
import org.scalajs.junit.utils.JUnitTest

class IgnoreTest {
  @Ignore @Test def onlyTest(): Unit = ()
}

class IgnoreTestAssertions extends JUnitTest {
  protected def expectedOutput(builder: OutputBuilder): OutputBuilder =
    builder.ignored("onlyTest")
}
