/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Framework    **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.jasminetest

import sbt.testing._

import org.scalajs.jasmine.ExpectationResult
import org.scalajs.jasmine.Result
import org.scalajs.jasmine.Spec
import org.scalajs.jasmine.Suite

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.runtime.StackTrace

/** This class is passed to the actual jasmine framework as a reporter */
class JasmineTestReporter(taskDef: TaskDef, eventHandler: EventHandler,
    loggers: Array[Logger], runnerDone: () => Unit) {
  private var currentSuite: Suite = _

  @JSExport
  def reportRunnerStarting(): Unit = {
    info("")
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

    val selector =
      new NestedTestSelector(spec.suite.getFullName(), description)

    if (results.passed) {
      eventHandler.handle(new JasmineEvent(taskDef, Status.Success, selector))

      loggers foreach { log =>
        val success = color(log, SuccessColor, "+")
        log.info(s"  $success $description")
      }
    } else {
      eventHandler.handle(new JasmineEvent(taskDef, Status.Error, selector))

      loggers foreach { log =>
        val failure = color(log, ErrorColor, "x")
        log.error(s" $failure $description")
        results.getItems.foreach(displayResult(log))
      }
    }
  }

  @JSExport
  def reportSuiteResults(suite: Suite): Unit = {
    var results = suite.results()

    info("")
    val title = "Total for suite " + suite.description
    val message =
      s"${results.totalCount} specs, ${results.failedCount} failure"

    val selector = new NestedSuiteSelector(suite.getFullName())

    if (results.passedCount != results.totalCount) {
      eventHandler.handle(new JasmineEvent(taskDef, Status.Error, selector))

      loggers foreach { log =>
        log.error(title)
        log.error(color(log, InfoColor, message))
      }
    } else {
      loggers foreach { log =>
        log.info(title)
        log.info(color(log, InfoColor, message))
      }
    }
    info("")
  }

  @JSExport
  def reportRunnerResults(): Unit = {
    runnerDone()
  }

  private val ErrorColor = "\u001b[31m"
  private val SuccessColor = "\u001b[32m"
  private val InfoColor = "\u001b[34m"
  private val Reset = "\u001b[0m"

  private def info(str: String) =
    loggers.foreach(_.info(str))

  private def color(log: Logger, color: String, msg: String) =
    if (log.ansiCodesSupported) color + msg + Reset
    else msg

  private def sanitizeMessage(message: String) = {
    val FilePattern = """^(.+?) [^ ]+\.js \(line \d+\)\.*?$""".r
    val EvalPattern = """^(.+?) in eval.+\(eval\).+?\(line \d+\).*?$""".r

    message match {
      case FilePattern(originalMessage) => originalMessage
      case EvalPattern(originalMessage) => originalMessage
      case message => message
    }
  }

  private def displayResult(log: Logger)(result: Result) = {
    (result.`type`: String) match {
      case "log" =>
        log.info(s"    ${result.toString}")
      case "expect" =>
        val r = result.asInstanceOf[ExpectationResult]

        if (!r.passed()) {
          val message = sanitizeMessage(r.message)
          val stack = StackTrace.extract(r.trace).takeWhile { stackElem =>
            (stackElem.getFileName == null ||
                !stackElem.getFileName.endsWith("jasmine.js"))
          }

          if (stack.isEmpty)
            log.error(s"    $message")
          else
            log.trace(new JasmineTestException(message, stack))
        }
    }
  }

}
