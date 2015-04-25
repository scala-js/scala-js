package java.lang

import scala.scalajs.js
import js.annotation.JSExport

final class StackTraceElement(declaringClass: String, methodName: String,
    fileName: String, lineNumber: Int) extends AnyRef with java.io.Serializable {

  private[this] var columnNumber: Int = -1

  def getFileName(): String = fileName
  def getLineNumber(): Int = lineNumber
  def getClassName(): String = declaringClass
  def getMethodName(): String = methodName
  def isNativeMethod(): scala.Boolean = false

  @JSExport
  def getColumnNumber(): Int = columnNumber

  @JSExport
  def setColumnNumber(columnNumber: Int): Unit =
    this.columnNumber = columnNumber

  override def equals(that: Any): scala.Boolean = that match {
    case that: StackTraceElement =>
      (getFileName == that.getFileName) &&
      (getLineNumber == that.getLineNumber) &&
      (getClassName == that.getClassName) &&
      (getMethodName == that.getMethodName)
    case _ =>
      false
  }

  override def toString(): String = {
    var result = ""
    if (declaringClass != "<jscode>")
      result += declaringClass + "."
    result += methodName
    if (fileName eq null) {
      if (isNativeMethod)
        result += "(Native Method)"
      else
        result += "(Unknown Source)"
    } else {
      result += s"($fileName"
      if (lineNumber >= 0) {
        result += s":$lineNumber"
        if (columnNumber >= 0)
          result += s":$columnNumber"
      }
      result += ")"
    }
    result
  }

  override def hashCode(): Int = {
    declaringClass.hashCode() ^ methodName.hashCode()
  }
}
