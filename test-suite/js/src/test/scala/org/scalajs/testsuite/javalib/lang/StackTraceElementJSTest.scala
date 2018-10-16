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

import scala.language.reflectiveCalls

import scala.scalajs.js

import org.junit.Assert._
import org.junit.Test

class StackTraceElementJSTest {

  private type StackTraceElementEx = StackTraceElement {
    def getColumnNumber(): Int
    def setColumnNumber(columnNumber: Int): Unit
  }

  private def getColumnNumber(ste: StackTraceElement): Int =
    ste.asInstanceOf[StackTraceElementEx].getColumnNumber()

  private def setColumnNumber(ste: StackTraceElement, columnNumber: Int): Unit =
    ste.asInstanceOf[StackTraceElementEx].setColumnNumber(columnNumber)

  @Test def columnNumber(): Unit = {
    val ste = new StackTraceElement("MyClass", "myMethod", "myFile.scala", 1)
    assertEquals(-1, getColumnNumber(ste))
    setColumnNumber(ste, 5)
    assertEquals(5, getColumnNumber(ste))
  }

  @Test def should_use_the_additional_columnNumber_field_in_its_toString(): Unit = {
    val ste = new StackTraceElement("MyClass", "myMethod", "myFile.scala", 1)
    assertEquals("MyClass.myMethod(myFile.scala:1)", ste.toString)
    setColumnNumber(ste, 5)
    assertEquals("MyClass.myMethod(myFile.scala:1:5)", ste.toString)
  }
}
