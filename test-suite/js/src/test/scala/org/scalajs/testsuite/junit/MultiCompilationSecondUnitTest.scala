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
      boot.invoke(boot.newInstance(), "testFromMultiCompilation")
    } catch {
      case e: Throwable =>
        fail(s"Could not invoke a test: ${e.getMessage}")
    }
  }
}
