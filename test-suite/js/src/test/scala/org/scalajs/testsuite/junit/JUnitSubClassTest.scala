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

class JUnitSubClassTest {
  @Test def test1(): Unit = ()
}

class JUnitSubClassExtended1Test extends JUnitSubClassTest

class JUnitSubClassExtended2Test extends JUnitSubClassTest {
  @Test def test2(): Unit = ()
}

class JUnitSubClassTestCheck {

  @Test def testSubClass0(): Unit = {
    val boot = JUnitUtil.loadBootstrapper("org.scalajs.testsuite.junit.JUnitSubClassTest")
    try {
      boot.invokeTest(boot.newInstance(), "test1")
    } catch {
      case e: Throwable =>
        fail(s"Could not invoke a test: ${e.getMessage}")
    }
  }

  @Test def testSubClass1(): Unit = {
    val boot = JUnitUtil.loadBootstrapper("org.scalajs.testsuite.junit.JUnitSubClassExtended1Test")
    try {
      boot.invokeTest(boot.newInstance(), "test1")
    } catch {
      case e: Throwable =>
        fail(s"Could not invoke a test: ${e.getMessage}")
    }
  }

  @Test def testSubClass2(): Unit = {
    val boot = JUnitUtil.loadBootstrapper("org.scalajs.testsuite.junit.JUnitSubClassExtended2Test")
    try {
      boot.invokeTest(boot.newInstance(), "test1")
      boot.invokeTest(boot.newInstance(), "test2")
    } catch {
      case e: Throwable =>
        fail(s"Could not invoke a test: ${e.getMessage}")
    }
  }
}
