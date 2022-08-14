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

package java.lang

import scala.scalajs.js
import js.annotation.JSExport

/* The primary constructor, taking a `columnNumber`, is not part of the JDK
 * API. It is used internally in `java.lang.StackTrace`, and could be accessed
 * by third-party libraries with a bit of IR manipulation.
 */
final class StackTraceElement(declaringClass: String, methodName: String,
    fileName: String, lineNumber: Int, private[this] var columnNumber: Int)
    extends AnyRef with java.io.Serializable {

  def this(declaringClass: String, methodName: String, fileName: String, lineNumber: Int) =
    this(declaringClass, methodName, fileName, lineNumber, -1)

  def getFileName(): String = fileName
  def getLineNumber(): Int = lineNumber
  def getClassName(): String = declaringClass
  def getMethodName(): String = methodName
  def isNativeMethod(): scala.Boolean = false

  // Not part of the JDK API, accessible through reflection.
  def getColumnNumber(): Int = columnNumber

  // Not part of the JDK API, accessible through reflection.
  @deprecated("old internal API; use the constructor with a column number instead", "1.11.0")
  def setColumnNumber(columnNumber: Int): Unit =
    this.columnNumber = columnNumber

  override def equals(that: Any): scala.Boolean = that match {
    case that: StackTraceElement =>
      (getFileName() == that.getFileName()) &&
      (getLineNumber() == that.getLineNumber()) &&
      (getColumnNumber() == that.getColumnNumber()) &&
      (getClassName() == that.getClassName()) &&
      (getMethodName() == that.getMethodName())
    case _ =>
      false
  }

  override def toString(): String = {
    var result = ""
    if (declaringClass != "<jscode>")
      result += declaringClass + "."
    result += methodName
    if (fileName eq null) {
      if (isNativeMethod())
        result += "(Native Method)"
      else
        result += "(Unknown Source)"
    } else {
      result += "(" + fileName
      if (lineNumber >= 0) {
        result += ":" + lineNumber
        if (columnNumber >= 0)
          result += ":" + columnNumber
      }
      result += ")"
    }
    result
  }

  override def hashCode(): Int = {
    declaringClass.hashCode() ^
    methodName.hashCode() ^
    fileName.hashCode() ^
    lineNumber ^
    columnNumber
  }
}
