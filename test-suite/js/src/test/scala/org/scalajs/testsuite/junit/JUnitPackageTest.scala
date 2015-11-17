package org.scalajs.testsuite.junit

import org.junit.Test
import org.junit.Assert._

package Outer {
  class JUnitPackageTestOuter {
    @Test def fun(): Unit = ()
  }

  package Inner {
    class JUnitPackageTestInner {
      @Test def fun(): Unit = ()
    }
  }
}

class JUnitPackageTest {
  @Test def testOuterTransformation(): Unit = {
    JUnitUtil.loadBootstrapper(
      "org.scalajs.testsuite.junit.Outer.JUnitPackageTestOuter")
  }

  @Test def testInnerTransformation(): Unit = {
    JUnitUtil.loadBootstrapper(
      "org.scalajs.testsuite.junit.Outer.Inner.JUnitPackageTestInner")
  }
}
