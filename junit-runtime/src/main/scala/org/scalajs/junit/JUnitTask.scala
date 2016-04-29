package org.scalajs.junit

import com.novocode.junit.{Ansi, RichLogger}
import Ansi._
import sbt.testing._
import org.scalajs.testinterface.TestUtils
import scala.util.{Try, Success, Failure}

final class JUnitTask(val taskDef: TaskDef, runner: JUnitBaseRunner)
    extends sbt.testing.Task {

  def tags: Array[String] = Array.empty

  def execute(eventHandler: EventHandler, loggers: Array[Logger],
      continuation: Array[Task] => Unit): Unit = {
    continuation(execute(eventHandler, loggers))
  }

  def execute(eventHandler: EventHandler, loggers: Array[Logger]): Array[Task] = {
    val fullClassName = taskDef.fullyQualifiedName
    val richLogger = new RichLogger(loggers, runner.runSettings, fullClassName)

    def infoOrDebug(msg: String): Unit = {
      if (runner.runSettings.verbose)
        richLogger.info(msg)
      else
        richLogger.debug(msg)
    }

    infoOrDebug(c("Test run started", INFO))

    val bootstrapperName = fullClassName + "$scalajs$junit$bootstrapper"

    val startTime = System.nanoTime

    def errorWhileLoadingClass(t: Throwable): Unit = {
      richLogger.error("Error while loading test class: " + fullClassName, t)
      val selector = new TestSelector(fullClassName)
      val optThrowable = new OptionalThrowable(t)
      val ev = new JUnitEvent(taskDef, Status.Failure, selector, optThrowable)
      eventHandler.handle(ev)
    }

    Try(TestUtils.loadModule(bootstrapperName, runner.testClassLoader)) match {
      case Success(classMetadata: JUnitTestBootstrapper) =>
        new JUnitExecuteTest(taskDef, runner, classMetadata,
            richLogger, eventHandler).executeTests()

      case Success(_) =>
        val msg = s"Expected $bootstrapperName to extend JUnitTestBootstrapper"
        errorWhileLoadingClass(new Exception(msg))

      case Failure(exception) =>
        errorWhileLoadingClass(exception)
    }

    runner.taskDone()

    val time = System.nanoTime - startTime
    val failed = runner.testFailedCount
    val ignored = runner.testIgnoredCount
    val total = runner.testTotalCount

    val msg = {
      c("Test run finished: ", INFO) +
      c(s"$failed failed", if (failed == 0) INFO else ERRCOUNT) +
      c(s", ", INFO) +
      c(s"$ignored ignored", if (ignored == 0) INFO else IGNCOUNT) +
      c(s", $total total, ${time.toDouble / 1000000000}s", INFO)
    }

    infoOrDebug(msg)
    runner.resetTestCounts()

    Array()
  }
}
