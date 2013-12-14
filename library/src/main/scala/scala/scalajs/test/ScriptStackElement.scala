package scala.scalajs.test

import scala.scalajs.js

class ScriptStackElement(
  val fileName: String,
  val functionName: String,
  val lineNumber: Int)

object ScriptStackElement {
  def apply(fileName: String, functionName: String, lineNumber: Int) =
    new ScriptStackElement(fileName, functionName, lineNumber)
}
