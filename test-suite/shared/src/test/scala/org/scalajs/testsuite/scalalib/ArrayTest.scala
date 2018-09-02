/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013--2018, LAMP/EPFL  **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.scalalib

import org.junit.Test
import org.junit.Assert._

class ArrayTest {
  @Test def unapplySeq_issue_3445(): Unit = {
    val args: Array[String] = Array("foo", "bar", "foobar")
    val Array(x, xs @ _*) = args
    assertEquals("foo", x)
    assertEquals(Seq("bar", "foobar"), xs)
  }
}
