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
      boot.invoke(boot.newInstance(), "mixinTest")
    } catch {
      case _: Throwable =>
        fail("Could not invoke JUnitMixinTest.mixinTest as a test.")
    }
  }
}
