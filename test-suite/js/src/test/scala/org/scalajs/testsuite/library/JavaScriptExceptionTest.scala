/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2016, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.library

import scala.scalajs.js

import org.junit.Assert._
import org.junit.Test

class JavaScriptExceptionTest {

  @Test def getMessageTest(): Unit = {
    val error = new js.TypeError("custom message")
    val jsException = js.JavaScriptException(error)
    assertEquals("TypeError: custom message", jsException.getMessage())
  }

  @Test def toStringTest(): Unit = {
    val error = new js.TypeError("custom message")
    val jsException = js.JavaScriptException(error)
    assertEquals(
        "scala.scalajs.js.JavaScriptException: TypeError: custom message",
        jsException.toString())
  }

}
