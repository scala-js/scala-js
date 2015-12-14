package org.scalajs.testsuite.junit

import org.junit.Test

// This a base class that is extended in the tests by MultiCompilationB
abstract class MultiCompilationTest {
  @Test def testFromMultiCompilation(): Unit = ()
}
