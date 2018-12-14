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

import org.junit.Test

import org.scalajs.junit.utils._

class Multi1Test {
  @Test def multiTest1(): Unit = ()
  @Test def multiTest2(): Unit = ()
}

class Multi1TestAssertions extends JUnitTest
