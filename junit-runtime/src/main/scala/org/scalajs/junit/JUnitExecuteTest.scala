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

import org.junit._
import sbt.testing._

import scala.util.matching.Regex

private[junit] final class JUnitExecuteTest(taskDef: TaskDef,
    runSettings: RunSettings, bootstrapper: Bootstrapper,
    richLogger: RichLogger, eventHandler: EventHandler) {

  private[this] var failed = 0
  private[this] var ignored = 0
  private[this] var total = 0

  def executeTests(): Unit = {
    def runWithOrWithoutQuietMode[T](block: => T): T = {
      if (runSettings.quiet) {
        scala.Console.withOut(new ByteArrayOutputStream()) {
          block
        }
      } else {
        block
      }
    }

    richLogger.log(richLogger.infoOrDebug, Ansi.c("Test run started", Ansi.BLUE))

    val (_, timeInSeconds) = runTestLifecycle(None)(()) { _ =>
      bootstrapper.beforeClass()
      runWithOrWithoutQuietMode {
        for (method <- bootstrapper.tests) {
          if (method.ignored) {
            richLogger.logTestInfo(_.info, Some(method.name), "ignored")
            ignored += 1
            emitEvent(Some(method.name), Status.Skipped)
          } else {
            executeTestMethod(bootstrapper, method)
          }
        }
      }
    } { _ =>
      bootstrapper.afterClass()
    }

    val msg = {
      Ansi.c("Test run finished: ", Ansi.BLUE) +
      Ansi.c(s"$failed failed", if (failed == 0) Ansi.BLUE else Ansi.RED) +
      Ansi.c(s", ", Ansi.BLUE) +
      Ansi.c(s"$ignored ignored", if (ignored == 0) Ansi.BLUE else Ansi.YELLOW) +
      Ansi.c(s", $total total, ${timeInSeconds}s", Ansi.BLUE)
    }

    richLogger.log(richLogger.infoOrDebug, msg)
  }

  private[this] def executeTestMethod(bootstrapper: Bootstrapper,
      test: TestMetadata) = {
    richLogger.logTestInfo(richLogger.infoOrDebug, Some(test.name), "started")

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

    val (passed, timeInSeconds) = runTestLifecycle(Some(test.name)) {
      bootstrapper.newInstance()
    } { instance =>
      bootstrapper.before(instance)
      handleExpected(test.annotation.expected) {
        bootstrapper.invokeTest(instance, test.name)
      }
    } {
      bootstrapper.after(_)
    }

    richLogger.logTestInfo(_.debug, Some(test.name),
        s"finished, took $timeInSeconds sec")

    // Scala.js-specific: timeouts are warnings only, after the fact
    val timeout = test.annotation.timeout
    if (timeout != 0 && timeout <= timeInSeconds) {
      richLogger.log(_.warn, "Timeout: took " + timeInSeconds + " sec, expected " +
          (timeout.toDouble / 1000) + " sec")
    }

    if (passed)
      emitEvent(Some(test.name), Status.Success)

    total += 1
  }

  private def runTestLifecycle[T](method: Option[String])(build: => T)(
      body: T => Unit)(after: T => Unit): (Boolean, Double) = {
    val startTime = System.nanoTime

    var exceptions: List[Throwable] = Nil
    try {
      val x = build
      try {
        body(x)
      } catch {
        case t: Throwable => exceptions ::= t
      } finally {
        after(x)
      }
    } catch {
      case t: Throwable => exceptions ::= t
    }

    val timeInSeconds = (System.nanoTime - startTime).toDouble / 1000000000

    exceptions.reverse match {
      case Nil =>

      case e :: Nil if isAssumptionViolation(e) =>
        method.fold {
          richLogger.logTestInfo(_.info, None, "ignored")
          ignored += 1
        } { _ =>
          richLogger.logTestException(_.warn, "Test assumption in test ", method, e, timeInSeconds)
        }

        emitEvent(method, Status.Skipped)

      case e :: es =>
        def emit(t: Throwable) = {
          richLogger.logTestException(_.error, "Test ", method, t, timeInSeconds)
          richLogger.trace(t)
          failed += 1
        }

        emit(e)
        emitEvent(method, Status.Failure)
        es.foreach(emit)
    }

    (exceptions.isEmpty, timeInSeconds)
  }

  private def emitEvent(method: Option[String], status: Status): Unit = {
    val testName = method.fold(taskDef.fullyQualifiedName)(method =>
      taskDef.fullyQualifiedName + "." + runSettings.decodeName(method))
    val selector = new TestSelector(testName)
    eventHandler.handle(new JUnitEvent(taskDef, status, selector))
  }

  private def isAssumptionViolation(ex: Throwable): Boolean = {
    ex.isInstanceOf[AssumptionViolatedException] ||
    ex.isInstanceOf[internal.AssumptionViolatedException]
  }
}
