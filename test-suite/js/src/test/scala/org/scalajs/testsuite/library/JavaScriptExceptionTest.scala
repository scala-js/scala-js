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
    assertEquals(
        "scala.scalajs.js.JavaScriptException: TypeError: custom message",
        jsException.toString())
  }

  @Test def caseClassAPI(): Unit = {
    /* This is mostly to ensure that the js.JavaScriptException from the
     * scalajs-library takes precedence over the one from the
     * linker-private-library.
     */

    val error = new js.TypeError("custom message")
    val jsException = js.JavaScriptException(error)
    val jsException2 = jsException.copy()
    assertNotSame(jsException, jsException2)
    assertSame(error, jsException2.exception)

    val product: Product = jsException
    assertEquals("JavaScriptException", product.productPrefix)
    assertSame(error, product.productElement(0))
  }

}
