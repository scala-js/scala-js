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

package org.scalajs.testsuite.jsinterop

import org.junit.Test
import org.scalajs.testsuite.junit.JUnitUtil

class `1_TestName` { // scalastyle:ignore
  @Test def `a test with name 1_TestName`(): Unit = ()
}

class eval {
  @Test def `a test with name eval`(): Unit = ()
}

class `\u1f4a7` { // scalastyle:ignore
  @Test def `a test with name \u1f4a7`(): Unit = ()
}

class StrangeNamedTests {
  @Test def testName1(): Unit = {
    // This should not fail
    JUnitUtil.loadBootstrapper("org.scalajs.testsuite.jsinterop.1_TestName")
  }

  @Test def testName2(): Unit = {
    // This should not fail
    JUnitUtil.loadBootstrapper("org.scalajs.testsuite.jsinterop.eval")
  }

  @Test def testName3(): Unit = {
    // This should not fail
    JUnitUtil.loadBootstrapper("org.scalajs.testsuite.jsinterop.\u1f4a7")
  }
}
