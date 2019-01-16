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

class JUnitNamesTest {
  @Test def +(): Unit = ()
  @Test def `*`(): Unit = ()
  @Test def `∆ƒ`(): Unit = ()
  @Test def `3€`(): Unit = ()
}

class JUnitNamesTestCheck {
  @Test def jUnitNamesTest(): Unit = {
    val boot = JUnitUtil.loadBootstrapper(
        "org.scalajs.testsuite.junit.JUnitNamesTest")
    try {
      boot.invokeTest(boot.newInstance(), "$plus")
      boot.invokeTest(boot.newInstance(), "$times")
      boot.invokeTest(boot.newInstance(), "$u2206ƒ")
      boot.invokeTest(boot.newInstance(), "3€")
    } catch {
      case _: Throwable =>
        fail("Could not invoke method on JUnitNamesTest.")
    }
  }
}
