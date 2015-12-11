/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.scalalib

import scala.reflect.NameTransformer

import org.junit.Test
import org.junit.Assert._

class NameTransformerTest {

  @Test def decode_issue_1602(): Unit = {
    /* Mostly to make sure it links.
     * We trust the Scala implementation for correctness. And if it isn't,
     * well, behaving the same as Scala is the correct thing do for us
     * anyway.
     */
    assertEquals("+", NameTransformer.decode("$plus"))
    assertEquals("ab+", NameTransformer.decode("ab$plus"))
    assertEquals("-", NameTransformer.decode("$minus"))
    assertEquals("+x-y", NameTransformer.decode("$plusx$minusy"))
    assertEquals("+-", NameTransformer.decode("$plus$minus"))
  }
}
