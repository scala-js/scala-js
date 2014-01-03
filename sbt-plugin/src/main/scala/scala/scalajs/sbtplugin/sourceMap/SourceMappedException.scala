package scala.scalajs.sbtplugin.sourceMap

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
