/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.io

import org.junit.Test
import org.junit.Assert._

import java.io.Closeable

class AutoCloseableTest {
  import AutoCloseableTest._

  private def close(x: AutoCloseable): Unit = x.close()

  @Test
  def closeableExtendsAutoCloseable(): Unit = {
    val x = new SomeCloseable
    assertFalse(x.closed)
    close(x)
    assertTrue(x.closed)
  }

  @Test
  def byteArrayOutputStreamIsAnAutoCloseable_issue_3107(): Unit = {
    val x = new java.io.ByteArrayOutputStream
    close(x)
  }
}

object AutoCloseableTest {
  class SomeCloseable extends Closeable {
    var closed: Boolean = false

    def close(): Unit = closed = true
  }
}
