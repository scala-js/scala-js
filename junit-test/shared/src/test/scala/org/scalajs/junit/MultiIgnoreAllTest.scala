package org.scalajs.junit

import org.junit._
import org.scalajs.junit.utils.JUnitTest

class MultiIgnoreAllTest {
  @Ignore @Test def multiTest1(): Unit = ()
  @Ignore @Test def multiTest2(): Unit = ()
  @Ignore @Test def multiTest3(): Unit = ()
  @Ignore @Test def multiTest4(): Unit = ()
  @Ignore @Test def multiTest5(): Unit = ()
}

class MultiIgnoreAllTestAssertions extends JUnitTest {
  protected def expectedOutput(builder: OutputBuilder): OutputBuilder = {
    builder
      .ignored("multiTest1")
      .ignored("multiTest2")
      .ignored("multiTest3")
      .ignored("multiTest4")
      .ignored("multiTest5")
  }
}
