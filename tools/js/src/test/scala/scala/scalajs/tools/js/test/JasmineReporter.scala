package scala.scalajs.tools.js.test

import org.scalajs.jasmine.Suite

import scala.scalajs.js.annotation.JSExport

import scala.scalajs.test._

object JSConsoleTestOutput extends TestOutput {

  type Color = Null

  val errorColor: Color = null
  val successColor: Color = null
  val infoColor: Color = null

  def color(message: String, color: Color): String = message

  def error(message: String, stack: Array[StackTraceElement]): Unit =
    withStack(message, stack)

  def error(message: String): Unit = println(message)

  def failure(message: String, stack: Array[StackTraceElement]): Unit =
    withStack(message, stack)

  def failure(message: String): Unit = println(message)
  def succeeded(message: String): Unit = println(message)
  def skipped(message: String): Unit = println(message)
  def pending(message: String): Unit = println(message)
  def ignored(message: String): Unit = println(message)
  def canceled(message: String): Unit = println(message)

  object log extends TestOutputLog {
    def info(message: String): Unit = println(message)
    def warn(message: String): Unit = println(message)
    def error(message: String): Unit = println(message)
  }

  private def withStack(message: String, stack: Array[StackTraceElement]) =
    println(message + stack.mkString("\n", "\n", ""))

}

@JSExport("scalajs.JasmineConsoleReporter")
class JasmineConsoleReporter(throwOnFail: Boolean = false)
    extends JasmineTestReporter(JSConsoleTestOutput) {

  private var suiteFails: Int = 0
  private var suiteCount: Int = 0

  override def reportSuiteResults(suite: Suite): Unit = {
    super.reportSuiteResults(suite)
    if (suite.results().failedCount > 0)
      suiteFails += 1
    suiteCount += 1
  }

  override def reportRunnerResults(): Unit = {
    super.reportRunnerResults()
    val failed = suiteFails > 0
    val resStr = if (failed) "Failed" else "Passed"
    println(s"$resStr: Total $suiteCount, Failed $suiteFails")

    if (failed && throwOnFail)
      sys.error("Jasmine test suite failed.")
  }

}
