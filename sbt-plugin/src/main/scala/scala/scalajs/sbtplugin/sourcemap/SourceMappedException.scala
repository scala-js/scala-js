/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.sbtplugin.sourcemap

import org.mozilla.javascript.ScriptStackElement
import org.mozilla.javascript.RhinoException

case class SourceMappedException(
    message: String,
    stack: Array[ScriptStackElement],
    cause: RhinoException) extends RuntimeException(message, cause) {

  def this(e: RhinoException) =
    this(e.getMessage, e.getScriptStack, e)

  def this(message: String, stack: Array[ScriptStackElement]) =
    this(message, stack, null)

  val javaStyleStack =
    stack.map { el =>
      new StackTraceElement(
        "<javascript code>",
        Option(el.functionName).getOrElse("<no function name>"),
        el.fileName,
        el.lineNumber)
    }

  setStackTrace(javaStyleStack)
}
