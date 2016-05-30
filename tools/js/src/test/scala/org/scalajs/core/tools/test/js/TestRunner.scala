package org.scalajs.core.tools.test.js

import scala.scalajs.js
import js.annotation.JSExport

import org.scalajs.testinterface.{ScalaJSClassLoader, TestDetector}

import sbt.testing._

@JSExport("scalajs.TestRunner")
object TestRunner {

  @JSExport
  def runTests(): Unit = {
    System.setProperty("scalajs.testsuite.testtag", "testtag.value")
    System.setProperty("scalajs.nodejs", "true")
    System.setProperty("scalajs.typedarray", "true")
    System.setProperty("scalajs.fastopt-stage", "true")

    val eventHandler = new SimpleEventHandler
    val loggers = Array[Logger](new SimpleLogger)

    def taskLoop(tasks: Iterable[Task]): Unit = {
      if (tasks.nonEmpty)
        tasks.head.execute(eventHandler, loggers,
            newTasks => taskLoop(tasks.tail ++ newTasks))
    }

    for {
      (framework, taskDefs) <- TestDetector.detectTests()
    } {
      val runner = framework.runner(Array(), Array(),
          new ScalaJSClassLoader(js.Dynamic.global))
      val tasks = runner.tasks(taskDefs.toArray)
      taskLoop(tasks)
    }

    if (eventHandler.hasFailed)
      sys.error("Some tests have failed")
  }

  private class SimpleEventHandler extends EventHandler {
    private[this] var failed = false

    def hasFailed: Boolean = failed

    def handle(ev: Event) = {
      if (ev.status == Status.Error || ev.status == Status.Failure)
        failed = true
    }
  }

  private class SimpleLogger extends Logger {
    def ansiCodesSupported(): Boolean = false
    def error(msg: String): Unit = println(msg)
    def warn(msg: String): Unit = println(msg)
    def info(msg: String): Unit = println(msg)
    def debug(msg: String): Unit = println(msg)
    def trace(t: Throwable): Unit = t.printStackTrace
  }

}
