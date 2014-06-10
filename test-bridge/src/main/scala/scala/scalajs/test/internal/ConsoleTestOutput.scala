package scala.scalajs.test.internal

import scala.scalajs.js.annotation.JSExport

import scala.scalajs.test._

import scala.scalajs.runtime.StackTrace.ColumnStackTraceElement

/** Implementation of TestOutput. DO NOT USE. This class is only public
 *  so it can be exported
 */
@JSExport("scala.scalajs.test.internal.ConsoleTestOutput")
object ConsoleTestOutput extends TestOutput {

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

  private val messagePrefix = "``|%^scala.js-test-comm&&"

  private def send(fct: String, msg: String) =
    println(messagePrefix + fct + "|" + msg)

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

}
