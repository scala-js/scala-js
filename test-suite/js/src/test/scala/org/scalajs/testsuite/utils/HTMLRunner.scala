/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.utils

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName

import scala.collection.mutable

import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global

import org.scalajs.testinterface.ScalaJSClassLoader

import sbt.testing._

object HTMLRunner extends js.JSApp {
  private val classLoader = new ScalaJSClassLoader(js.Dynamic.global)

  private val logger = new Logger {
    val ansiCodesSupported = false
    def error(msg: String): Unit = UI.log(msg, "error")
    def warn(msg: String): Unit = UI.log(msg, "warn")
    def info(msg: String): Unit = UI.log(msg, "info")
    def debug(msg: String): Unit = UI.log(msg, "debug")
    def trace(t: Throwable): Unit = UI.log(t.toString, "error")
  }

  private object EventCounter extends EventHandler {
    val counts = mutable.Map.empty[Status, Int].withDefaultValue(0)
    def handle(event: Event): Unit = counts(event.status) += 1
  }

  def main(): Unit = {
    val runResults = for {
      (framework, taskDefs) <- TestDetector.detectTests()
    } yield {
      logger.debug(s"Detected ${taskDefs.size} tests for ${framework.name}")
      val result = runTests(framework, taskDefs)

      // Report results.
      result.filter(_.nonEmpty).foreach(r =>
          logger.info(s"${framework.name()} reported $r"))

      result.map(_ => ())
    }

    // Report event counts.
    Future.sequence(runResults).foreach { (_: Seq[Unit]) =>
      import EventCounter.counts

      // Insert <hr>
      UI.hr()

      // Construct report string.
      val countStr = {
        val total = counts.values.sum
        val countStrs = s"Total: $total" +: Status.values.map(status =>
          s"$status: ${counts(status)}"
        )
        countStrs.mkString(", ")
      }

      if (counts(Status.Error) + counts(Status.Failure) > 0) {
        UI.log("Some tests failed!\n" + countStr, "had_failures")
      } else {
        UI.log("All tests passed!\n" + countStr, "all_passed")
      }
    }
  }

  private def runTests(framework: Framework,
      taskDefs: Seq[TaskDef]): Future[String] = {
    val runner = framework.runner(Array(), Array(), classLoader)
    val tasks = runner.tasks(taskDefs.toArray)

    def runAllTasks(tasks: Seq[Task]): Future[Unit] = {
      val units = tasks.map { task =>
        for {
          newTasks <- scheduleTask(task)
          allDone <- runAllTasks(newTasks)
        } yield allDone
      }

      Future.sequence(units).map((_: Seq[Unit]) => ())
    }

    runAllTasks(tasks).map((_: Unit) => runner.done())
  }

  private def scheduleTask(task: Task): Future[Array[Task]] = {
    val promise = Promise[Array[Task]]
    // Don't use a Future so we yield to the UI event thread.
    js.timers.setTimeout(0) {
      task.execute(EventCounter, Array(logger), promise.success)
    }
    promise.future
  }

  private object UI {
    def log(msg: String, clss: String): Unit = {
      val el = dom.document.createElement("pre")
      el.textContent = msg
      el.setAttribute("class", s"log $clss")
      dom.document.body.appendChild(el)
    }

    def hr(): Unit = {
      val elem = dom.document.createElement("hr")
      dom.document.body.appendChild(elem)
    }
  }

  // Mini dom facade.
  private object dom { // scalastyle:ignore
    @JSName("document")
    @js.native
    object document extends js.Object { // scalastyle:ignore
      def body: Element = js.native
      def createElement(tag: String): Element = js.native
    }

    @js.native
    trait Element extends js.Object {
      def appendChild(child: Element): Unit = js.native
      def setAttribute(name: String, value: String): Unit = js.native
      var textContent: String = js.native
    }
  }
}
