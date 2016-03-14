package org.scalajs.testsuite.utils

import com.novocode.junit.{RichLogger, RunSettings}
import org.scalajs.junit._
import org.scalajs.testinterface.ScalaJSClassLoader
import sbt.testing.{Event, EventHandler, Logger, TaskDef}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport

@JSExport
object JUnitTests {
  @JSExport
  def executeAll(): Unit = {
    val classLoader = new ScalaJSClassLoader(js.Dynamic.global)

    val runSettings = new RunSettings(color = false, decodeScalaNames = true,
        quiet = false, verbose = true, logAssert = true, ignoreRunners = "",
        logExceptionClass = true)

    val masterRunner = new JUnitMasterRunner(Array(), Array(), classLoader,
        runSettings)

    val loggers = Array[Logger](JSConsoleLogger)

    for (testDef <- TestDetector.getDetectedTests()) {
      testDef match {
        case testDef: JUnitTestBootstrapper =>
          val runner = new JUnitSlaveRunner(Array(), Array(), classLoader,
              (m: String) => masterRunner.receiveMessage(m), runSettings)
          val name = testDef.toString.split("\\$scalajs\\$junit\\$bootstrapper").head
          val eventHandler = new EventHandler {
            def handle(event: Event): Unit = ()
          }
          val richLogger = new RichLogger(loggers, runSettings, name)
          val taskDef = new TaskDef(name, JUnitFingerprint, false, Array())
          val executor = new JUnitExecuteTest(taskDef, runner, testDef,
            richLogger, eventHandler)
          JUnitTask.printStart(richLogger, runner)
          val startTime = System.nanoTime
          executor.executeTests()
          val time = System.nanoTime - startTime
          JUnitTask.printEnd(richLogger, runner, time)
          runner.done()

        case _ => // Ignore non JUnit tests
      }
    }

    val total = masterRunner.taskTotalCount
    val failed = masterRunner.taskFailedCount
    val passed = masterRunner.taskPassedCount
    val skipped = masterRunner.taskSkippedCount
    JSConsoleLogger.info(
        s"Passed: Total $total, Failed $failed, Passed $passed, Skipped $skipped")
  }
}
