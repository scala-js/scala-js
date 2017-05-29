package org.scalajs.testsuite.utils

import scala.collection.mutable

import scala.scalajs.js
import scala.scalajs.js.annotation._

import org.scalajs.testinterface.{ScalaJSClassLoader, TestDetector}

import sbt.testing._

@JSExportTopLevel("scalajs.ConsoleTestRunner")
object ConsoleTestRunner {

  @JSExport
  def runTests(): Unit = {
    try {
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
        val runner = framework.runner(Array(), Array(), new ScalaJSClassLoader())
        val tasks = runner.tasks(taskDefs.toArray)
        taskLoop(tasks)
      }

      val failedEvents = eventHandler.failedEvents
      if (failedEvents.nonEmpty) {
        System.err.println("The following tests failed:")
        for (event <- failedEvents) {
          System.err.println("* " + event.fullyQualifiedName())
          if (event.throwable().isDefined())
            event.throwable().get().printStackTrace()
        }
        throw new AssertionError("Some tests have failed")
      }
    } catch {
      case th: Throwable =>
        th.printStackTrace()
        throw th
    }
  }

  private class SimpleEventHandler extends EventHandler {
    private[this] val _failedEvents = new mutable.ListBuffer[Event]

    def failedEvents: List[Event] = _failedEvents.toList

    def handle(ev: Event) = {
      if (ev.status == Status.Error || ev.status == Status.Failure)
        _failedEvents += ev
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
