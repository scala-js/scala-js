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

package org.scalajs.testsuite.junit

import scala.scalajs.js

import org.junit.Test
import org.junit.Assert.assertTrue

class JUnitBootstrapTest {
  @Test def testClassBootstrap(): Unit = {
    // This tests that the Scala.js JUnit runtime is working
    assertTrue(true)
  }
}

object JUnitBootstrapTest extends js.JSApp {
  def main(): Unit = {
    // This should not fail
    JUnitUtil.loadBootstrapper("org.scalajs.testsuite.junit.JUnitBootstrapTest")
  }
}
