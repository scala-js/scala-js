package org.scalajs.testsuite.junit

import org.junit.Assert._
import org.junit.Test

abstract class JUnitAbstractClassTest {
  @Test def test1(): Unit = ()
}

class JUnitAbstractClassExtended1Test extends JUnitAbstractClassTest

class JUnitAbstractClassExtended2Test extends JUnitAbstractClassTest {
  @Test def test2(): Unit = ()
}

class JUnitAbstractClassTestCheck {
  @Test def testAbstractClass1(): Unit = {
    val boot = JUnitUtil.loadBootstrapper(
        "org.scalajs.testsuite.junit.JUnitAbstractClassExtended1Test")
    try {
      boot.invoke(boot.newInstance(), "test1")
    } catch {
      case e: Throwable =>
        fail(s"Could not invoke a test: ${e.getMessage}")
    }
  }

  @Test def testAbstractClass2(): Unit = {
    val boot = JUnitUtil.loadBootstrapper(
        "org.scalajs.testsuite.junit.JUnitAbstractClassExtended2Test")
    try {
      boot.invoke(boot.newInstance(), "test1")
      boot.invoke(boot.newInstance(), "test2")
    } catch {
      case e: Throwable =>
        fail(s"Could not invoke a test: ${e.getMessage}")
    }
  }
}
