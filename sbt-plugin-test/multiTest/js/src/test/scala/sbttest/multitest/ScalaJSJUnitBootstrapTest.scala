package sbttest.multitest

import org.junit.Test
import org.junit.Assert.assertTrue

import org.scalajs.jasminetest.JasmineTest

class JUnitBootstrapTest {
  @Test def testClassBootstrap(): Unit = {
    // This tests that the Scala.js JUnit runtime is working
    assertTrue(true)
  }
}

object JUnitBootstrapTestFromJasmine extends JasmineTest {
  describe("org.scalajs.testsuite.junit.JUnitBootstrapTest") {
    it("should bootstrap JUnit test classes") {
      // This should not fail
      JUnitUtil.loadBootstrapper("sbttest.multitest.JUnitBootstrapTest")
    }
  }
}
