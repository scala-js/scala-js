/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testinterface

import scala.scalajs.js
import scala.scalajs.concurrent.QueueExecutionContext
import scala.scalajs.js.annotation.JSName
import js.URIUtils.{decodeURIComponent, encodeURIComponent}

import scala.collection.mutable

import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global

import scala.util.Try

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
    /* Note: Test filtering is currently done based on the fully qualified name
     * of a test. While this is reasonable in most cases, there could be a test
     * that is run by multiple test frameworks.
     */
    val (testFilter, optExcludedHash): (TaskDef => Boolean, Option[Int])  = {
      val search = dom.document.location.search.stripPrefix("?")
      search.split("&").map(decodeURIComponent).toList match {
        case "i" :: excludedHash :: included =>
          val includeSet = included.toSet
          (t => includeSet.contains(t.fullyQualifiedName),
              Some(excludedHash.toInt))

        case "e" :: excluded =>
          val excludeSet = excluded.toSet
          (t => !excludeSet.contains(t.fullyQualifiedName), None)

        case _ =>
          // Invalid parameter. Run everything.
          (_ => true, None)
      }
    }

    val allTests = TestDetector.detectTests()

    val totalTestCount = allTests.map(_._2.size).sum
    val excludedTests = allTests.flatMap(_._2.filterNot(testFilter))

    val ui = new UI(excludedTests, totalTestCount)

    // Warn if test set changed.
    def excludedHash = excludedTests.map(_.fullyQualifiedName).toSet.##
    if (optExcludedHash.exists(_ != excludedHash)) {
      ui.warnTestSetChanged()
    }

    val oks = for {
      (framework, taskDefs) <- allTests
    } yield {
      runTests(framework, taskDefs.filter(testFilter), ui)
    }

    // Report event counts.
    Future.sequence(oks).map(and).onComplete(ui.done)
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
    val uiBox = ui.newTestTask(task.taskDef.fullyQualifiedName)
    val handler = new EventCounter.Handler

    // Schedule test via timeout so we yield to the UI event thread.
    val newTasks = Promise[Array[Task]]
    val invocation = Future(task.execute(handler, Array(uiBox.logger),
        newTasks.success))(QueueExecutionContext.timeouts())

    val result = for {
      _ <- invocation
      tasks <- newTasks.future
    } yield {
      (!handler.hasErrors, tasks)
    }

    result.map(_._1).onComplete(uiBox.done)

    result.recover {
      case _ => (false, Array[Task]())
    }
  }

  private class UI(excludedTaskDefs: Seq[TaskDef], totalTestCount: Int) {
    // State.
    private var _done = false

    private val runningTests = mutable.Buffer.empty[RunningTest]
    private val excludedTests = mutable.Buffer.empty[Test]

    // UI Elements.

    // Top level container. Mainly to prevent code under test from accidentally
    // modifying our UI.
    private val container = dom.document.body.newElement()

    private val rootBox = new RootBox(excludedTaskDefs.size, totalTestCount)

    private[this] var nextFailureLocation: MoveTarget =
      if (excludedTaskDefs.nonEmpty) new ExcludedTestBox()
      else rootBox

    updateCounts()

    trait TestTask {
      def logger: Logger
      def done(ok: Try[Boolean]): Unit
    }

    def newTestTask(testName: String): TestTask = {
      val task = new RunningTest(testName)
      runningTests += task
      task
    }

    def done(ok: Try[Boolean]): Unit = {
      _done = true

      ok.failed.foreach { t =>
        rootBox.log("Test framework crashed during execution:", "error")
        rootBox.log(t.toString, "error")
      }

      rootBox.done(ok.getOrElse(false))
      updateCounts()
    }

    def warnTestSetChanged(): Unit = {
      val line = rootBox.log("", "warn")

      // Note: The following is not entirely true. The warning will also appear
      // if tests have been removed.
      line.newTextNode("There are new excluded tests in your project. You " +
        "may wish to ")
      line.newLink("?", "Run all")
      line.newTextNode(" to rediscover all available tests.")
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

    private trait Test {
      def testName: String
      def selected: Boolean
      def selected_=(v: Boolean): Unit
      def failed: Boolean
    }

    private trait MoveTarget {
      def setNextSibling(that: TestBox): Unit
    }

    private class RunningTest(val testName: String) extends Test with TestTask {
      private val box = new TestBox(testName)
      box.checkbox.onclick = rootBox.updateCheckbox

      private var _ok = false

      def done(ok: Try[Boolean]): Unit = {
        ok.failed.foreach { t =>
          logger.error("Test framework crashed during test:")
          logger.trace(t)
        }

        _ok = ok.getOrElse(false)

        updateCounts()
        box.done(_ok)
        if (!_ok) {
          box.expand()
          nextFailureLocation.setNextSibling(box)
          nextFailureLocation = box
        }
      }

      def selected: Boolean = box.checkbox.checked
      def selected_=(v: Boolean): Unit = box.checkbox.checked = v

      def failed: Boolean = !_ok

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

    private class TestBox(caption: String) extends MoveTarget {
      private val box = container.newElement(clss = "test-box")

      private val header = box.newElement(clss = "test-box-header")

      private val expandLink = header.newLink(href = "#", text = "[+]")
      expandLink.onclick = { () => toggleExpand(); false }

      private val headerCaption = header.newTextNode(" " + caption)

      val checkbox = header.newCheckbox(checked = true)

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

      def setNextSibling(that: TestBox): Unit = {
        this.box.insertAdjacentElement("afterend", that.box)
      }

      private def toggleExpand(): Unit = {
        expanded = !expanded
        expandLink.textContent = if (expanded) "[-]" else "[+]"
        body.style.display = if (expanded) "block" else "none"
      }
    }

    private class RootBox(excludedTestCount: Int,
        totalTestCount: Int) extends MoveTarget {
      private val box = {
        val caption = {
          if (excludedTestCount == 0) {
            s"Total Test Suites: $totalTestCount"
          } else {
            val selectedCount = totalTestCount - excludedTestCount
            s"Selected Test Suites $selectedCount (Total: $totalTestCount)"
          }
        }
        new TestBox(caption)
      }

      box.expand()

      box.checkbox.onclick = testUpdater(runningTests, box.checkbox)

      private val counterLine = box.log("", "info")

      def counterLineText: String = counterLine.textContent
      def counterLineText_=(v: String): Unit = counterLine.textContent = v

      def done(ok: Boolean): Unit = {
        box.done(ok)
        counterLine.className = "log " + statusClass(ok)

        val rerunLine = box.log("Next: ", statusClass(ok))

        if (!ok) {
          rerunLine.newLink(runLink(_.failed), "Run failed")
          rerunLine.newTextNode(" | ")
        }

        rerunLine.newLink("#", "Run selected").onclick = { () =>
          dom.document.location.search = runLink(_.selected)
          false
        }

        rerunLine.newTextNode(" | ")
        rerunLine.newLink("?", "Run all")
      }

      val updateCheckbox: js.Function0[Boolean] =
        checkboxUpdater(runningTests, box.checkbox)

      def log(msg: String, clss: String): dom.Element = box.log(msg, clss)

      def setNextSibling(that: TestBox): Unit = box.setNextSibling(that)

      private def runLink(condition: Test => Boolean): String = {
        val (included, excluded) =
          (runningTests ++ excludedTests).partition(condition)

        val params = {
          if (included.size < excluded.size) {
            // Create an include list.
            val excludedHash = excluded.map(_.testName).toSet.##.toString
            Seq("i", excludedHash) ++ included.map(_.testName)
          } else {
            // Create an exclude list.
            "e" +: excluded.map(_.testName)
          }
        }

        params.map(encodeURIComponent).mkString("?", "&", "")
      }
    }

    private class ExcludedTestBox extends MoveTarget {
      private val box = {
        val count = excludedTaskDefs.size
        new TestBox(s"Excluded Test Suites ($count)")
      }

      private val updateCheckbox: js.Function0[Boolean] =
        checkboxUpdater(excludedTests, box.checkbox)

      box.checkbox.checked = false
      box.checkbox.onclick = testUpdater(excludedTests, box.checkbox)

      for (taskDef <- excludedTaskDefs) {
        excludedTests += new ExcludedTest(taskDef.fullyQualifiedName)
      }

      def setNextSibling(that: TestBox): Unit = box.setNextSibling(that)

      private class ExcludedTest(val testName: String) extends Test {
        private val logLine = box.log("", "info")
        private val checkbox = logLine.newCheckbox(checked = false)
        checkbox.onclick = updateCheckbox

        logLine.newTextNode(" " + testName)

        def selected: Boolean = checkbox.checked
        def selected_=(v: Boolean): Unit = checkbox.checked = v
        def failed: Boolean = false
      }
    }

    private def statusClass(ok: Boolean): String =
      if (ok) "success" else "error"

    private def checkboxUpdater(tests: Seq[Test],
        checkbox: dom.Checkbox): js.Function0[Boolean] = { () =>
      val all = tests.forall(_.selected)
      val indet = !all && tests.exists(_.selected)

      checkbox.indeterminate = indet
      if (!indet)
        checkbox.checked = all
      true
    }

    private def testUpdater(tests: Seq[Test],
        checkbox: dom.Checkbox): js.Function0[Boolean] = { () =>
      tests.foreach(_.selected = checkbox.checked)
      true
    }
  }

  // Mini dom facade.
  private object dom { // scalastyle:ignore
    @JSName("document")
    @js.native
    object document extends js.Object { // scalastyle:ignore
      def body: Element = js.native
      def createElement(tag: String): Element = js.native
      def createTextNode(tag: String): Node = js.native
      val location: Location = js.native
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
      def insertAdjacentElement(location: String, element: Element): Unit = js.native
    }

    @js.native
    trait Checkbox extends Element {
      var checked: Boolean = js.native
      var indeterminate: Boolean = js.native
    }

    @js.native
    trait Style extends js.Object {
      var display: String = js.native
    }

    @js.native
    trait Location extends js.Object {
      var search: String = js.native
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

      def newLink(href: String, text: String): dom.Element = {
        val el = newElement(tpe = "a", text = text)
        el.setAttribute("href", href)
        el
      }

      def newCheckbox(checked: Boolean): dom.Checkbox = {
        val el = newElement(tpe = "input").asInstanceOf[dom.Checkbox]
        el.setAttribute("type", "checkbox")
        el.checked = checked
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
