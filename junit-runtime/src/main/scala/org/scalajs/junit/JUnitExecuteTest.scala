/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.junit

import java.io.ByteArrayOutputStream

import com.novocode.junit.Ansi._
import com.novocode.junit.RichLogger
import com.novocode.junit.RunSettings
import org.junit._
import sbt.testing._

import scala.util.matching.Regex

final class JUnitExecuteTest(task: JUnitTask, runSettings: RunSettings,
    bootstrapper: Bootstrapper, richLogger: RichLogger,
    eventHandler: EventHandler) {

  private val taskDef = task.taskDef

  def fullyQualifiedName: String = taskDef.fullyQualifiedName()

  def executeTests(): Unit = {
    val assumptionViolated = try {
      bootstrapper.beforeClass()
      false
    } catch {
      case _: AssumptionViolatedException | _:internal.AssumptionViolatedException =>
        true
    }

    if (assumptionViolated) {
      richLogger.info(s"Test $formattedTestClass ignored")
      ignoreTestClass()
    } else {
      def runWithOrWithoutQuietMode[T](block: => T): T = {
        if (runSettings.quiet) {
          scala.Console.withOut(new ByteArrayOutputStream()) {
            block
          }
        } else {
          block
        }
      }

      runWithOrWithoutQuietMode {
        for (method <- bootstrapper.tests) {
          if (method.ignored) {
            logTestInfo(_.info, method.name, "ignored")
            ignoreTest(method.name)
          } else {
            executeTestMethod(bootstrapper, method)
          }
        }
      }

      bootstrapper.afterClass()
    }
  }

  private[this] def executeTestMethod(bootstrapper: Bootstrapper,
      test: TestMetadata) = {
    val methodName = test.name
    val decodedMethodName = runSettings.decodeName(methodName)

    if (runSettings.verbose)
      logTestInfo(_.info, decodedMethodName, "started")
    else
      logTestInfo(_.debug, decodedMethodName, "started")

    val t0 = System.nanoTime
    def getTimeInSeconds(): Double = (System.nanoTime - t0).toDouble / 1000000000

    var eventAlreadyEmitted: Boolean = false

    def emitTestFailed(): Unit = {
      if (eventAlreadyEmitted) {
        // Only add to the failed test count, don't emit an event
        task.failed += 1
      } else {
        testFailed(methodName)
        eventAlreadyEmitted = true
      }
    }

    def execute(body: => Unit): Boolean = {
      try {
        body
        true
      } catch {
        case ex: Throwable =>
          val timeInSeconds = getTimeInSeconds()
          if (isAssumptionViolation(ex)) {
            logThrowable(_.warn, "Test assumption in test ", decodedMethodName, ex, timeInSeconds)
            testSkipped()
            false
          } else {
            logThrowable(_.error, "Test ", decodedMethodName, ex, timeInSeconds)
            if (!ex.isInstanceOf[AssertionError] || runSettings.logAssert) {
              richLogger.trace(ex)
            }
            emitTestFailed()
            false
          }
      }
    }

    def handleExpected(expectedException: Class[_ <: Throwable])(body: => Unit) = {
      val wantException = expectedException != classOf[org.junit.Test.None]
      val succeeded = try {
        body
        true
      } catch {
        case t if expectedException.isInstance(t) => false

        case t if wantException =>
          val expName = expectedException.getName
          val gotName = t.getClass.getName
          throw new Exception(
              s"Unexpected exception, expected<$expName> but was<$gotName>", t)
      }

      if (succeeded && wantException)
        throw new AssertionError("Expected exception: " + expectedException.getName)
    }

    var testClassInstance: AnyRef = null

    val instantiationSucceeded = execute {
      testClassInstance = bootstrapper.newInstance()
    }

    val success = if (!instantiationSucceeded) {
      false
    } else {
      val beforeSucceeded = execute {
        bootstrapper.before(testClassInstance)
      }

      val beforeAndTestSucceeded = if (!beforeSucceeded) {
        false
      } else {
        execute {
          handleExpected(test.annotation.expected) {
            bootstrapper.invokeTest(testClassInstance, test.name)
          }
        }
      }

      // Whether before and/or test succeeded or not, run the after methods
      val afterSucceeded = execute {
        bootstrapper.after(testClassInstance)
      }

      beforeAndTestSucceeded && afterSucceeded
    }

    logTestInfo(_.debug, decodedMethodName,
        s"finished, took ${getTimeInSeconds()} sec")

    // Scala.js-specific: timeouts are warnings only, after the fact
    val timeInSeconds = getTimeInSeconds()
    val timeout = test.annotation.timeout
    if (timeout != 0 && timeout <= timeInSeconds) {
      richLogger.warn("Timeout: took " + timeInSeconds + " sec, expected " +
          (timeout.toDouble / 1000) + " sec")
    }

    if (success)
      testPassed(methodName)

    task.total += 1
  }

  private def ignoreTest(methodName: String) = {
    task.ignored += 1
    val selector = new NestedTestSelector(fullyQualifiedName, methodName)
    eventHandler.handle(new JUnitEvent(taskDef, Status.Skipped, selector))
  }

  private def ignoreTestClass() = {
    task.ignored += 1
    val selector = new TestSelector(fullyQualifiedName)
    eventHandler.handle(new JUnitEvent(taskDef, Status.Skipped, selector))
  }

  private def testSkipped(): Unit = {
    val selector = new TestSelector(fullyQualifiedName)
    eventHandler.handle(new JUnitEvent(taskDef, Status.Skipped, selector))
  }

  private def testFailed(methodName: String): Unit = {
    task.failed += 1
    val selector = new NestedTestSelector(fullyQualifiedName, methodName)
    eventHandler.handle(new JUnitEvent(taskDef, Status.Failure, selector))
  }

  private def testPassed(methodName: String): Unit = {
    val selector = new NestedTestSelector(fullyQualifiedName, methodName)
    eventHandler.handle(new JUnitEvent(taskDef, Status.Success, selector))
  }

  private def logTestInfo(level: RichLogger => (String => Unit), method: String, msg: String): Unit =
    level(richLogger)(s"Test ${formatMethod(method, CYAN)} $msg")

  private def logThrowable(level: RichLogger => (String => Unit), prefix: String,
      method: String, ex: Throwable, timeInSeconds: Double): Unit = {
    val logException = {
      !runSettings.notLogExceptionClass &&
      (runSettings.logAssert || !ex.isInstanceOf[AssertionError])
    }

    val fmtName = if (logException) {
      val name =
        if (isAssumptionViolation(ex)) classOf[internal.AssumptionViolatedException].getName
        else ex.getClass.getName

      formatClass(name, RED) + ": "
    } else {
      ""
    }

    val m = formatMethod(method, RED)
    val msg = s"$prefix$m failed: $fmtName${ex.getMessage}, took $timeInSeconds sec"
    level(richLogger)(msg)
  }

  private def formatMethod(method: String, color: String): String =
    s"$formattedTestClass.${c(method, color)}"

  private lazy val formattedTestClass = formatClass(taskDef.fullyQualifiedName, YELLOW)

  private def formatClass(fullName: String, color: String): String = {
    val (prefix, name) = fullName.splitAt(fullName.lastIndexOf(".") + 1)
    prefix + c(name, color)
  }

  private def isAssumptionViolation(ex: Throwable): Boolean = {
    ex.isInstanceOf[AssumptionViolatedException] ||
    ex.isInstanceOf[internal.AssumptionViolatedException]
  }
}
