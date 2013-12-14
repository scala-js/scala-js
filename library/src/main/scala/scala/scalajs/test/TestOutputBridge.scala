package scala.scalajs.test

import scala.scalajs.js.Dynamic.newInstance
import scala.scalajs.js.Dynamic.global
import scala.scalajs.js
import scala.scalajs.js.JavaScriptException

class TestOutputBridge(eventProxy: EventProxy) extends TestOutput {

  val errorColor = "\033[31m"
  val successColor = "\033[32m"
  val infoColor = "\033[34m"
  val reset = "\033[0m"

  def color(message: String, color: String): String =
    message.split('\n').mkString(color, reset + '\n' + color, reset)

  def error(message: String, stack: Array[ScriptStackElement]): Unit =
    eventProxy.error(message, toJsArray(stack))

  def error(message: String): Unit =
    error(message, Array.empty)

  def failure(message: String, stack: Array[ScriptStackElement]): Unit =
    eventProxy.failure(message, toJsArray(stack))

  def failure(message: String): Unit =
    failure(message, Array.empty)

  def succeeded(message: String): Unit =
    eventProxy.succeeded(message)

  def skipped(message: String): Unit =
    eventProxy.skipped(message)

  def pending(message: String): Unit =
    eventProxy.pending(message)

  def ignored(message: String): Unit =
    eventProxy.ignored(message)

  def canceled(message: String): Unit =
    eventProxy.canceled(message)

  val log =
    new TestOutputLog {
      def info(message: String): Unit = eventProxy.info(message)
      def error(message: String): Unit = eventProxy.error(message)
    }

  def getCurrentStack(): Array[ScriptStackElement] = {
    val RhinoExceptionClass = global.Packages.org.mozilla.javascript.JavaScriptException.Packages.org.mozilla.javascript.JavaScriptException.Packages.org.mozilla.javascript.JavaScriptException.Packages.org.mozilla.javascript.JavaScriptException.Packages.org.mozilla.javascript.JavaScriptException
    val rhinoException = newInstance(RhinoExceptionClass)("stack creation")
    val rhinoStack = rhinoException.getScriptStack().asInstanceOf[js.Array[js.Dynamic]]
    val stack =
      for (i <- 0 until rhinoStack.length.asInstanceOf[js.Number].toInt) yield {
        val e = rhinoStack(i)
        ScriptStackElement(e.fileName.toString, e.functionName.toString, e.lineNumber.asInstanceOf[js.Number].toInt)
      }

    stack.toArray
  }

  private def stackElementToDictionary(s: ScriptStackElement): js.Dictionary =
    js.Dictionary(
      "fileName" -> (s.fileName: js.String),
      "functionName" -> (s.functionName: js.String),
      "lineNumber" -> (s.lineNumber: js.Number))

  private def elementToDictionary(a: Seq[ScriptStackElement]): Array[js.Dictionary] =
    (for (el <- a) yield stackElementToDictionary(el)).toArray

  private def toJsArray(a: Array[ScriptStackElement]): js.Array[js.Dictionary] =
    elementToDictionary(a)

}
