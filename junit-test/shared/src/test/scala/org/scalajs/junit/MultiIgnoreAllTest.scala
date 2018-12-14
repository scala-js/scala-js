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

import org.junit._
import org.scalajs.junit.utils.JUnitTest

class MultiIgnoreAllTest {
  @Ignore @Test def multiTest1(): Unit = ()
  @Ignore @Test def multiTest2(): Unit = ()
  @Ignore @Test def multiTest3(): Unit = ()
  @Ignore @Test def multiTest4(): Unit = ()
  @Ignore @Test def multiTest5(): Unit = ()
}

class MultiIgnoreAllTestAssertions extends JUnitTest
