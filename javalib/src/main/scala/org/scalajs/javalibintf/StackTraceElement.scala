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

package org.scalajs.javalibintf

import java.{lang => jl}

object StackTraceElement {
  def createWithColumnNumber(declaringClass: String, methodName: String,
      fileName: String, lineNumber: Int, columnNumber: Int): jl.StackTraceElement = {
    new jl.StackTraceElement(declaringClass, methodName, fileName,
        lineNumber, columnNumber)
  }

  def getColumnNumber(stackTraceElement: jl.StackTraceElement): Int =
    stackTraceElement.getColumnNumber()
}
