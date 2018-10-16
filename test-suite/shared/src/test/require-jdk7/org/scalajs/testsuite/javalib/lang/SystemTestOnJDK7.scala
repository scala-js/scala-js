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

package org.scalajs.testsuite.javalib.lang

import org.scalajs.testsuite.utils.Platform._

import org.junit.Test
import org.junit.Assert._

class SystemTestOnJDK7 {
  @Test def lineSeparator(): Unit = {
    val lineSep = System.lineSeparator()

    if (!executingInJVM)
      assertEquals("\n", lineSep)
    else
      assertTrue(Set("\n", "\r", "\r\n").contains(lineSep))
  }
}
