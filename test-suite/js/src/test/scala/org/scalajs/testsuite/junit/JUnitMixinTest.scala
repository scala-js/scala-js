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

import org.junit.Assert._
import org.junit.Test

trait JUnitMixinTestTrait {
  @Test def mixinTest(): Unit = ()
}

class JUnitMixinTest extends JUnitMixinTestTrait

class JUnitMixinTestCheck {
  @Test def jUnitMixinTest(): Unit = {
    val boot = JUnitUtil.loadBootstrapper(
        "org.scalajs.testsuite.junit.JUnitMixinTest")
    try {
      boot.invokeTest(boot.newInstance(), "mixinTest")
    } catch {
      case _: Throwable =>
        fail("Could not invoke JUnitMixinTest.mixinTest as a test.")
    }
  }
}
