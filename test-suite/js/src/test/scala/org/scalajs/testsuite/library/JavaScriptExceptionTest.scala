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
    assertEquals("scala.scalajs.js.JavaScriptException: TypeError: custom message",
        jsException.toString())
  }

}
