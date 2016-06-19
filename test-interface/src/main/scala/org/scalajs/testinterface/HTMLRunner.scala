/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testinterface

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName

import scala.collection.mutable

import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global

import sbt.testing._

protected[testinterface] object HTMLRunner extends js.JSApp {
  private val classLoader = new ScalaJSClassLoader(js.Dynamic.global)

  private object EventCounter {
    private val isErrorStatus = Set(Status.Error, Status.Failure)

    val counts = mutable.Map.empty[Status, Int].withDefaultValue(0)

    class Handler extends EventHandler {
      private[this] var _hasErrors = false

      def handle(event: Event): Unit = {
        val status = event.status
        _hasErrors ||= isErrorStatus(status)
        counts(status) += 1
      }

      def hasErrors: Boolean = _hasErrors
    }
  }

  def main(): Unit = {
    val allTests = TestDetector.detectTests()

    val ui = new UI(testCount = allTests.map(_._2.size).sum)

    val oks = for {
      (framework, taskDefs) <- allTests
    } yield {
      runTests(framework, taskDefs, ui)
    }

    // Report event counts.
    Future.sequence(oks).map(and).foreach(ui.done)
  }

  private def runTests(framework: Framework,
      taskDefs: Seq[TaskDef], ui: UI): Future[Boolean] = {
    def runAllTasks(tasks: Seq[Task]): Future[Boolean] = {
      val oks = tasks.map { task =>
        for {
          (ok, newTasks) <- scheduleTask(task, ui)
          newOk <- runAllTasks(newTasks)
        } yield ok && newOk
      }

      Future.sequence(oks).map(and)
    }

    val runner = framework.runner(Array(), Array(), classLoader)
    val tasks = runner.tasks(taskDefs.toArray)

    for (ok <- runAllTasks(tasks)) yield {
      val resultStr = runner.done()
      if (resultStr.nonEmpty)
        ui.reportFrameworkResult(ok, framework.name, resultStr)
      ok
    }
  }

  private def scheduleTask(task: Task, ui: UI): Future[(Boolean, Array[Task])] = {
    val promise = Promise[(Boolean, Array[Task])]

    val uiBox = new ui.TestTask(task.taskDef.fullyQualifiedName)
    val handler = new EventCounter.Handler

    // Don't use a Future so we yield to the UI event thread.
    js.timers.setTimeout(0) {
      task.execute(handler, Array(uiBox.logger), { newTasks =>
        val ok = !handler.hasErrors

        uiBox.done(ok)
        promise.success((ok, newTasks))
      })
    }

    promise.future
  }

  private class UI(testCount: Int) {
    // UI Elements.

    // Top level container. Mainly to prevent code under test from accidentally
    // modifying our UI.
    private val container = dom.document.body.newElement()
    private val rootBox = new RootBox(testCount)

    // State.
    private var _done = false

    updateCounts()

    def done(ok: Boolean): Unit = {
      _done = true
      rootBox.done(ok)
      updateCounts()
    }

    def reportFrameworkResult(ok: Boolean,
        framework: String, result: String): Unit = {
      rootBox.log(s"$framework reported $result", statusClass(ok))
    }

    private def updateCounts(): Unit = {
      import EventCounter.counts

      // Construct report string.
      val countStr = {
        val total = counts.values.sum
        val countStrs = {
          s"Total: $total" +:
          Status.values.map(status => s"$status: ${counts(status)}")
        }
        countStrs.mkString(", ")
      }

      if (_done) {
        rootBox.counterLineText = countStr
      } else {
        rootBox.counterLineText = "Running... " + countStr
      }
    }

    class TestTask(testName: String) {
      private val box = new TestBox(testName)

      def done(ok: Boolean): Unit = {
        updateCounts()
        box.done(ok)
        if (!ok)
          box.expand()
      }

      val logger: Logger = new Logger {
        val ansiCodesSupported = false

        def error(msg: String): Unit = {
          box.log(msg, "error")
          box.expand()
        }

        def warn(msg: String): Unit = box.log(msg, "warn")
        def info(msg: String): Unit = box.log(msg, "info")
        def debug(msg: String): Unit = box.log(msg, "debug")
        def trace(t: Throwable): Unit = error(t.toString)
      }
    }

    private class TestBox(caption: String) {
      private val box = container.newElement(clss = "test-box")

      private val header = box.newElement(clss = "test-box-header")

      private val expandLink = header.newElement(tpe = "a", text = "[+]")
      expandLink.setAttribute("href", "#")
      expandLink.onclick = { () => toggleExpand(); false }

      private val headerCaption = header.newTextNode(" " + caption)

      private val body = box.newElement(clss = "test-box-body")

      private[this] var expanded = false

      def done(ok: Boolean): Unit = {
        header.className += " " + statusClass(ok)
        headerCaption.textContent += (if (ok) " - Passed" else " - Failed")
      }

      def expand(): Unit = {
        if (!expanded)
          toggleExpand()
      }

      def log(msg: String, clss: String): dom.Element =
        body.newElement(clss = s"log $clss", text = msg, tpe = "pre")

      private def toggleExpand(): Unit = {
        expanded = !expanded
        expandLink.textContent = if (expanded) "[-]" else "[+]"
        body.style.display = if (expanded) "block" else "none"
      }
    }

    private class RootBox(testCount: Int) {
      private val box = new TestBox(s"Total Test Suites: $testCount")
      box.expand()

      private val counterLine = box.log("", "info")

      def counterLineText: String = counterLine.textContent
      def counterLineText_=(v: String): Unit = counterLine.textContent = v

      def done(ok: Boolean): Unit = {
        box.done(ok)
        counterLine.className = "log " + statusClass(ok)
      }

      def log(msg: String, clss: String): Unit = box.log(msg, clss)
    }

    private def statusClass(ok: Boolean): String =
      if (ok) "success" else "error"
  }

  // Mini dom facade.
  private object dom { // scalastyle:ignore
    @JSName("document")
    @js.native
    object document extends js.Object { // scalastyle:ignore
      def body: Element = js.native
      def createElement(tag: String): Element = js.native
      def createTextNode(tag: String): Node = js.native
    }

    @js.native
    trait Node extends js.Object {
      var textContent: String = js.native
    }

    @js.native
    trait Element extends Node {
      def appendChild(child: Node): Unit = js.native
      def setAttribute(name: String, value: String): Unit = js.native
      var className: String = js.native
      val style: Style = js.native
      var onclick: js.Function0[Boolean] = js.native
    }

    @js.native
    trait Style extends js.Object {
      var display: String = js.native
    }

    implicit class RichElement(val element: Element) extends AnyVal {
      def newElement(clss: String = "", text: String = "",
          tpe: String = "div"): dom.Element = {
        val el = document.createElement(tpe)
        if (clss.nonEmpty)
          el.className = clss
        if (text.nonEmpty)
          el.textContent = text
        element.appendChild(el)
        el
      }

      def newTextNode(text: String): Node = {
        val n = document.createTextNode(text)
        element.appendChild(n)
        n
      }
    }
  }

  private def and(xs: Seq[Boolean]): Boolean = xs.fold(true)(_ && _)
}
