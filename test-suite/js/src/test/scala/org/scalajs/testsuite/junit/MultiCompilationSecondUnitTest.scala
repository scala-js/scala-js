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

import org.junit.Test
import org.junit.Assert._

class MultiCompilationSecondUnitTest extends MultiCompilationTest

class MultiCompilationSecondUnitTestCheck {
  @Test def testInDifferentCompilationUnits(): Unit = {
    // Test for issue #2112
    val boot = JUnitUtil.loadBootstrapper(
        "org.scalajs.testsuite.junit.MultiCompilationSecondUnitTest")
    try {
      boot.invokeTest(boot.newInstance(), "testFromMultiCompilation")
    } catch {
      case e: Throwable =>
        fail(s"Could not invoke a test: ${e.getMessage}")
    }
  }
}
