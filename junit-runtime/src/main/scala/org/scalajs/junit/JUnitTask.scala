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
    val richLogger = new RichLogger(loggers, runner.runSettings,
        taskDef.fullyQualifiedName)

    if (runner.runSettings.verbose)
      richLogger.info(c("Test run started", INFO))

    val bootstrapperName = taskDef.fullyQualifiedName + "$scalajs$junit$bootstrapper"

    val startTime = System.nanoTime

    Try(TestUtils.loadModule(bootstrapperName, runner.testClassLoader)) match {
      case Success(classMetadata: JUnitTestBootstrapper) =>
        new JUnitExecuteTest(taskDef, runner, classMetadata,
            richLogger, eventHandler).executeTests()

      case Success(_) =>
        richLogger.error("Error while loading test class: " +
            taskDef.fullyQualifiedName + ", expected " + bootstrapperName +
            " to extend JUnitTestBootstrapper")

      case Failure(exception) =>
        richLogger.error("Error while loading test class: " +
            taskDef.fullyQualifiedName, exception)
    }

    runner.taskDone()

    if (runner.runSettings.verbose) {
      val time = System.nanoTime - startTime
      val failed = runner.taskFailedCount
      val ignored = runner.taskIgnoredCount
      val total = runner.taskTotalCount
      val msg = Seq(
        c("Test run finished:", INFO),
        c(s"$failed failed,", if (failed == 0) INFO else ERRCOUNT),
        c(s"$ignored ignored,", if (ignored == 0) INFO else IGNCOUNT),
        c(s"$total total,", INFO),
        c(s"${time.toDouble / 1000000000}s", INFO))
      richLogger.info(msg.mkString(" "))
    }

    Array()
  }
}
