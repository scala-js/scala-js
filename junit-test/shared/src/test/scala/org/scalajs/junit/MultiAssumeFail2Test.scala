/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.junit

import org.junit.Assume._
import org.junit.Test

import org.scalajs.junit.utils.JUnitTest

class MultiAssumeFail2Test {
  @Test def multiTest1(): Unit = {
    assumeTrue("This assume should not pass", false)
  }
  @Test def multiTest2(): Unit = ()
  @Test def multiTest3(): Unit = ()
  @Test def multiTest4(): Unit = {
    assumeTrue("This assume should not pass", false)
  }
  @Test def multiTest5(): Unit = ()
}

class MultiAssumeFail2TestAssertions extends JUnitTest {
  protected def expectedOutput(builder: OutputBuilder): OutputBuilder = {
    builder
      .assumptionViolated("multiTest1")
      .success("multiTest2")
      .success("multiTest3")
      .assumptionViolated("multiTest4")
      .success("multiTest5")
  }
}
