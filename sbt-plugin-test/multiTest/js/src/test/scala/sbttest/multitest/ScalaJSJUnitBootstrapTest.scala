package sbttest.multitest

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
    JUnitUtil.loadBootstrapper("sbttest.multitest.JUnitBootstrapTest")
  }
}
