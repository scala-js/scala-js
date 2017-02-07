/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
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
