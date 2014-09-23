package scala.scalajs.testbridge.internal

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport

import scala.scalajs.testbridge._

import scala.scalajs.runtime.StackTrace.ColumnStackTraceElement

/** Implementation of TestOutput.
 *
 *  Attention: This class monkey-patches console.log. Make sure it is loaded
 *  before any output. It also should always be paired with a
 *  [[scala.scalajs.sbtplugin.testing.TestOutputConsole]] on the JVM side.
 */
@JSExport
protected object ConsoleTestOutput extends TestOutput {

  /** monkey-patches console.log when class is loaded */
  private val savedConsoleLog: js.Function1[String, Unit] = {
    import js.Dynamic.{ global => g }

    val console = g.console
    val savedLog = console.log

    val patch = (new MonkeyPatchConsole).asInstanceOf[js.Dynamic]

    // Need to write updateDynamic explicitly here. Since 2.10.x
    // chokes on this ("erroneous or inaccessible type")
    console.updateDynamic("log")(patch.log.bind(patch))

    savedLog.bind(console).asInstanceOf[js.Function1[String, Unit]]
  }

  type Color = String

  val errorColor = "\u001b[31m"
  val successColor = "\u001b[32m"
  val infoColor = "\u001b[34m"

  private val reset = "\u001b[0m"

  def color(message: String, color: String): String =
    message.split('\n').mkString(color, reset + '\n' + color, reset)

  def error(message: String,
    stack: Array[StackTraceElement]): Unit = {
    sendTrace(stack)
    send("error", message)
  }

  def error(message: String): Unit =
    error(message, Array.empty)

  def failure(message: String,
    stack: Array[StackTraceElement]): Unit = {
    sendTrace(stack)
    send("failure", message)
  }

  def failure(message: String): Unit =
    failure(message, Array.empty)

  def succeeded(message: String): Unit =
    send("succeeded", message)

  def skipped(message: String): Unit =
    send("skipped", message)

  def pending(message: String): Unit =
    send("pending", message)

  def ignored(message: String): Unit =
    send("ignored", message)

  def canceled(message: String): Unit =
    send("canceled", message)

  val log =
    new TestOutputLog {
      def info(message: String): Unit = send("info", message)
      def warn(message: String): Unit = send("warn", message)
      def error(message: String): Unit = send("error-log", message)
    }

  private def send(fct: String, msg: String) = {
    val escaped = msg
      .replace("\\", "\\\\")
      .replace("\n", "\\n")
      .replace("\r", "\\r")
    savedConsoleLog(fct + "|" + escaped)
  }

  private def sendTrace(stack: Array[StackTraceElement]) = for {
    el <- stack
  } send("trace", serializeStackElem(el))

  private def serializeStackElem(e: StackTraceElement) = {

    val flds = List(
      e.getClassName,
      e.getMethodName,
      e.getFileName,
      e.getLineNumber.toString,
      e.getColumnNumber.toString)

    flds.mkString("|")
  }

  /** used to monkey-patch console (only log will be used)
   *  we can't write a simple lambda, because we need varargs
   */
  private class MonkeyPatchConsole {
    @JSExport
    def log(msg: js.Any*): Unit = send("console-log", msg.mkString(" "))
  }

}
