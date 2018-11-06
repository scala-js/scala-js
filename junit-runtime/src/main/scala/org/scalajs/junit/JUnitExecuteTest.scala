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

  lazy val packageName = fullyQualifiedName.split('.').init.mkString(".")
  lazy val className = fullyQualifiedName.split('.').last

  def fullyQualifiedName: String = taskDef.fullyQualifiedName()

  def executeTests(): Unit = {
    val assumptionViolated = try {
      bootstrapper.beforeClass()
      false
    } catch {
      case _: AssumptionViolatedException | _:internal.AssumptionViolatedException =>
        true
    }

    def logTestIgnored(name: String): Unit = {
      logFormattedInfo(name, "ignored")
    }

    if (assumptionViolated) {
      logTestIgnored(null)
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
            logTestIgnored(method.name)
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
      logFormattedInfo(decodedMethodName, "started")
    else
      logFormattedDebug(decodedMethodName, "started")

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
          if (ex.isInstanceOf[AssumptionViolatedException] ||
              ex.isInstanceOf[internal.AssumptionViolatedException]) {
            logAssertionWarning(decodedMethodName, ex, timeInSeconds)
            testSkipped()
            false
          } else {
            val isAssertion = ex.isInstanceOf[AssertionError]
            val failedMsg = new StringBuilder
            failedMsg ++= "failed: "
            if (!runSettings.notLogExceptionClass &&
                (!isAssertion || runSettings.logAssert)) {
              val classParts = ex.getClass.getName.split('.')
              failedMsg ++= classParts.init.mkString(".")
              failedMsg += '.'
              failedMsg ++= c(classParts.last, ENAME2)
              failedMsg ++= ": "
            }
            failedMsg ++= ex.getMessage
            failedMsg += ','
            val msg = s"$failedMsg took $timeInSeconds sec"
            val exOpt = {
              if (!isAssertion || runSettings.logAssert) Some(ex)
              else None
            }
            logFormattedError(decodedMethodName, msg, exOpt)
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

    logFormattedDebug(decodedMethodName,
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

  private[this] def logAssertionWarning(methodName: String, ex: Throwable,
      timeInSeconds: Double): Unit = {
    val exName =
      if (runSettings.notLogExceptionClass) ""
      else "org.junit.internal." + c("AssumptionViolatedException", ERRMSG) + ": "

    val msg = s"failed: $exName${ex.getMessage}, took $timeInSeconds sec"
    logFormattedWarn("Test assumption in test ", methodName, msg)
  }

  private[this] def logFormattedInfo(method: String, msg: String): Unit = {
    val fMethod = if (method != null) c(method, NNAME2) else null
    richLogger.info(
        formatLayout("Test ", packageName, c(className, NNAME1), fMethod, msg))
  }

  private[this] def logFormattedDebug(method: String, msg: String): Unit = {
    val fMethod = if (method != null) c(method, NNAME2) else null
    richLogger.debug(
        formatLayout("Test ", packageName, c(className, NNAME1), fMethod, msg))
  }

  private[this] def logFormattedWarn(prefix: String, method: String,
      msg: String): Unit = {
    val fMethod = if (method != null) c(method, ERRMSG) else null
    richLogger.warn(
        formatLayout(prefix, packageName, c(className, NNAME1), fMethod, msg))
  }

  private[this] def logFormattedError(method: String, msg: String,
      exOpt: Option[Throwable]): Unit = {
    val fMethod = if (method != null) c(method, ERRMSG) else null
    val formattedMsg = formatLayout("Test ", packageName, c(className, NNAME1),
        fMethod, msg)
    exOpt match {
      case Some(ex) => richLogger.error(formattedMsg, ex)
      case None     => richLogger.error(formattedMsg)
    }
  }

  private[this] def formatLayout(prefix: String, packageName: String,
      className: String, method: String, msg: String): String = {
    if (method != null) s"$prefix$packageName.$className.$method $msg"
    else s"$prefix$packageName.$className $msg"
  }
}
