/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Framework    **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.test

import scala.scalajs.js
import scala.scalajs.js.Dynamic.global
import scala.scalajs.js.JavaScriptException
import scala.scalajs.js.annotation.JSExport

import org.scalajs.jasmine.ExpectationResult
import org.scalajs.jasmine.Result
import org.scalajs.jasmine.Spec
import org.scalajs.jasmine.Suite

object JasmineTestFramework extends TestFramework {

  /* Stub-out timer methods used by Jasmine and not provided by Rhino. */
  if (!global.setTimeout) {
    global.setTimeout = scalaJSStub("setTimeout")
    global.clearTimeout = scalaJSStub("clearTimeout")
    global.setInterval = scalaJSStub("setInterval")
    global.clearInterval = scalaJSStub("clearInterval")
  }

  def scalaJSStub(name: String): js.Function = { () =>
    global.console.log("Stub for " + name + " called")
  }

  // make sure jasmine is loaded
  global.importScripts("jasmine.js")

  def runTests(testOutput: TestOutput)(tests: => Unit): Unit = {
    val jasmine = global.jasmine
    val reporter = new JasmineTestFramework(testOutput)

    try {
      tests

      val jasmineEnv = jasmine.getEnv()
      jasmineEnv.addReporter(reporter.asInstanceOf[js.Any])
      jasmineEnv.updateInterval = 0
      jasmineEnv.execute()
    } catch {
      case JavaScriptException(exception) =>
        val stack = exception.asInstanceOf[js.Dynamic].stack
        testOutput.error("Problem executing code in tests: " + exception,
            reporter.getScriptStack(stack))
    }
  }
}

/** This class is passed to the actual jasmine framework as a reporter.
 *  TODO this should probably be renamed
 */
class JasmineTestFramework(testOutput: TestOutput) {
  private var currentSuite: Suite = _

  @JSExport
  def reportRunnerStarting(): Unit = {
    testOutput.log.info("")
  }

  @JSExport
  def reportSpecStarting(spec: Spec): Unit = {
    if (currentSuite != spec.suite) {
      currentSuite = spec.suite
      info(currentSuite.description)
    }
  }

  @JSExport
  def reportSpecResults(spec: Spec): Unit = {
    val results = spec.results()
    val description = spec.description

    if (results.passed) {
      testOutput.succeeded(s"  $success $description")
    } else {
      error(s" $failure $description")

      results.getItems foreach displayResult
    }
  }

  @JSExport
  def reportSuiteResults(suite: Suite): Unit = {
    var results = suite.results()

    info("")
    val title = "Total for suite " + suite.description
    val message =
      s"${results.totalCount} specs, ${results.failedCount} failure"

    if (results.passedCount != results.totalCount) {
      error(title)
      errorWithInfoColor(message)
    } else {
      info(title)
      infoWithInfoColor(message)
    }
    info("")
  }

  @JSExport
  def reportRunnerResults(): Unit = {
    // no need to report
  }

  private def info(str: String) =
    testOutput.log.info(str)

  private def infoWithInfoColor(str: String) =
    info(withColor(testOutput.infoColor, str))

  private def errorWithInfoColor(str: String) =
    error(withColor(testOutput.infoColor, str))

  private def error(msg: js.Any) =
    testOutput.log.error(msg.toString)

  private def withColor(color: String, message: String) =
    testOutput.color(message, color)

  private def sanitizeMessage(message: String) = {
    val FilePattern = """^(.+?) [^ ]+\.js \(line \d+\)\.*?$""".r
    val EvalPattern = """^(.+?) in eval.+\(eval\).+?\(line \d+\).*?$""".r

    message match {
      case FilePattern(originalMessage) => originalMessage
      case EvalPattern(originalMessage) => originalMessage
      case message => message
    }
  }

  private def failure = withColor(testOutput.errorColor, "x")
  private def success = withColor(testOutput.successColor, "+")

  private def displayResult(result: Result) = {
    (result.`type`: String) match {
      case "log" =>
        info(s"    ${result.toString}")
      case "expect" =>
        val r = result.asInstanceOf[ExpectationResult]
        if (!r.passed()) {
          val message = sanitizeMessage(r.message)
          val stack = getScriptStack(r.trace.stack)

          if (stack.isEmpty) testOutput.failure(s"    $message")
          else testOutput.error(s"    $message", stack)
        }
    }
  }

  private def getScriptStack(stack: js.Any): Array[ScriptStackElement] = {
    if (stack.isInstanceOf[js.String]) {
      val StackTracePattern = """^(.+?) ([^ ]+\.js):(\d+).*?$""".r
      val stackString: String = stack.asInstanceOf[js.String]

      stackString
        .split("\n")
        .map {
          case StackTracePattern(originalMessage, fileName, lineNumber) =>
            ScriptStackElement(fileName, "", lineNumber.toInt)
          case unknown =>
            throw JavaScriptException("Unknown stack element: " + unknown)
        }
        .takeWhile(e => !(e.fileName contains "jasmine.js"))
    } else Array.empty
  }
}
