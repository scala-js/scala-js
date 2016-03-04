/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.jsinterop

import org.junit.Test
import org.scalajs.testsuite.junit.JUnitUtil

class `1_TestName` { // scalastyle:ignore
  @Test def `a test with name 1_TestName`(): Unit = ()
}

class eval { // scalastyle:ignore
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
