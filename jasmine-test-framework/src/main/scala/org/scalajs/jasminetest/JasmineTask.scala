/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Framework    **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.jasminetest

import sbt.testing._

import scala.scalajs.js
import scala.scalajs.js.Dynamic.global
import scala.scalajs.js.JavaScriptException
import scala.scalajs.js.annotation.JSExport

import org.scalajs.testinterface.TestUtils

final class JasmineTask(private val runner: JasmineRunner,
    _taskDef: TaskDef) extends Task {

  def tags(): Array[String] = Array()

  def execute(eventHandler: EventHandler, loggers: Array[Logger]): Array[Task] =
    throw new UnsupportedOperationException("Jasmine only supports JavaScript")

  def execute(eventHandler: EventHandler, loggers: Array[Logger],
      continuation: Array[Task] => Unit): Unit = {
    val doneCont = () => continuation(Array())
    val jasmine = global.jasmine
    val reporter =
      new JasmineTestReporter(taskDef, eventHandler, loggers, doneCont)

    try {
      // Reset JasmineEnv
      jasmine.currentEnv_ = js.undefined

      // Load test
      TestUtils.loadModule(taskDef.fullyQualifiedName, runner.classLoader)

      // Setup JasmineEnv and run tests
      val jasmineEnv = jasmine.getEnv()
      jasmineEnv.addReporter(reporter.asInstanceOf[js.Any])
      jasmineEnv.updateInterval = 0
      jasmineEnv.execute()
    } catch {
      case t: Throwable =>
        // Jasmine itself failed. Issue a failure
        eventHandler.handle(new JasmineEvent(
            taskDef   = taskDef,
            status    = Status.Failure,
            selector  = new SuiteSelector,
            throwable = new OptionalThrowable(t)
        ))
        for (log <- loggers) {
          log.error("Problem executing code in tests: " + t.toString)
          log.trace(t)
        }

        // We are done
        doneCont()
    }
  }

  def taskDef(): TaskDef = _taskDef

}
