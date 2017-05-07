package org.scalajs.junit

import org.junit.Test

import org.scalajs.junit.utils._

class Multi1Test {
  @Test def multiTest1(): Unit = ()
  @Test def multiTest2(): Unit = ()
}

class Multi1TestAssertions extends JUnitTest {
  protected def expectedOutput(builder: OutputBuilder): OutputBuilder = {
    builder
      .success("multiTest1")
      .success("multiTest2")
  }
}
