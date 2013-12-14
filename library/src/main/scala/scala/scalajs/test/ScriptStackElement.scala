/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


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
