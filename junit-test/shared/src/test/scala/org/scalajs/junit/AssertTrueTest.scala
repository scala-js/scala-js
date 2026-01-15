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

import org.junit.Assert._
import org.junit.Test

import org.scalajs.junit.utils._

class AssertTrueTest {
  @Test def failTest(): Unit =
    assertTrue(false)

  @Test def successTest(): Unit =
    assertTrue(true)
}

class AssertTrueTestAssertions extends JUnitTest
