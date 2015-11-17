package org.scalajs.testsuite.junit

import org.junit.Assert._
import org.junit.Test

class JUnitSubClassTest {
  @Test def test1(): Unit = ()
}

class JUnitSubClassExtended1Test extends JUnitSubClassTest

class JUnitSubClassExtended2Test extends JUnitSubClassTest {
  @Test def test2(): Unit = ()
}

class JUnitSubClassTestCheck {

  @Test def testSubClass0(): Unit = {
    val boot = JUnitUtil.loadBootstrapper(
        "org.scalajs.testsuite.junit.JUnitSubClassTest")
    try {
      boot.invoke(boot.newInstance(), "test1")
    } catch {
      case e: Throwable =>
        fail(s"Could not invoke a test: ${e.getMessage}")
    }
  }

  @Test def testSubClass1(): Unit = {
    val boot = JUnitUtil.loadBootstrapper(
        "org.scalajs.testsuite.junit.JUnitSubClassExtended1Test")
    try {
      boot.invoke(boot.newInstance(), "test1")
    } catch {
      case e: Throwable =>
        fail(s"Could not invoke a test: ${e.getMessage}")
    }
  }

  @Test def testSubClass2(): Unit = {
    val boot = JUnitUtil.loadBootstrapper(
        "org.scalajs.testsuite.junit.JUnitSubClassExtended2Test")
    try {
      boot.invoke(boot.newInstance(), "test1")
      boot.invoke(boot.newInstance(), "test2")
    } catch {
      case e: Throwable =>
        fail(s"Could not invoke a test: ${e.getMessage}")
    }
  }
}
