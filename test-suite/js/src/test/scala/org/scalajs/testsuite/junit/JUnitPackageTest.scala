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
