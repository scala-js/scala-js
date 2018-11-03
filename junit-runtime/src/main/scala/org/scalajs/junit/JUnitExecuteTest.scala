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

import org.junit._
import sbt.testing._

import scala.util.matching.Regex

private[junit] final class JUnitExecuteTest(taskDef: TaskDef,
    runSettings: RunSettings, bootstrapper: Bootstrapper,
    richLogger: RichLogger, eventHandler: EventHandler) {

  def executeTests(): Unit = {
    richLogger.log(richLogger.infoOrDebug, Ansi.c("Test run started", Ansi.BLUE))

    var failed = 0
    var ignored = 0
    var total = 0

    val (errors, timeInSeconds) = runTestLifecycle {
      ()
    } { _ =>
      bootstrapper.beforeClass()

      for (method <- bootstrapper.tests) {
        if (method.ignored) {
          richLogger.logTestInfo(_.info, Some(method.name), "ignored")
          ignored += 1
          emitEvent(Some(method.name), Status.Skipped)
        } else {
          failed += executeTestMethod(bootstrapper, method)
          total += 1
        }
      }
    } { _ =>
      bootstrapper.afterClass()
    }

    errors match {
      case e :: Nil if isAssumptionViolation(e) =>
        richLogger.logTestInfo(_.info, None, "ignored")
        ignored += 1
        emitEvent(None, Status.Skipped)

      case es =>
        failed += es.size
        reportErrors(None, timeInSeconds, es)
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
      test: TestMetadata): Int = {
    val method = Some(test.name)

    richLogger.logTestInfo(richLogger.infoOrDebug, method, "started")

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

    val (errors, timeInSeconds) = runTestLifecycle {
      bootstrapper.newInstance()
    } { instance =>
      bootstrapper.before(instance)
      handleExpected(test.annotation.expected) {
        bootstrapper.invokeTest(instance, test.name)
      }
    } {
      bootstrapper.after(_)
    }

    val failed = errors match {
      case e :: Nil if isAssumptionViolation(e) =>
        richLogger.logTestException(_.warn, "Test assumption in test ", method, e, timeInSeconds)
        emitEvent(method, Status.Skipped)
        0

      case es =>
        reportErrors(method, timeInSeconds, es)
        es.size
    }

    richLogger.logTestInfo(_.debug, method, s"finished, took $timeInSeconds sec")

    // Scala.js-specific: timeouts are warnings only, after the fact
    val timeout = test.annotation.timeout
    if (timeout != 0 && timeout <= timeInSeconds) {
      richLogger.log(_.warn, "Timeout: took " + timeInSeconds + " sec, expected " +
          (timeout.toDouble / 1000) + " sec")
    }

    if (errors.isEmpty)
      emitEvent(method, Status.Success)

    failed
  }

  private def runTestLifecycle[T](build: => T)(body: T => Unit)(
      after: T => Unit): (List[Throwable], Double) = {
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

    (exceptions.reverse, timeInSeconds)
  }

  private def reportErrors(method: Option[String], timeInSeconds: Double,
      errors: List[Throwable]): Unit = {
    def emit(t: Throwable) = {
      richLogger.logTestException(_.error, "Test ", method, t, timeInSeconds)
      richLogger.trace(t)
    }

    if (errors.nonEmpty) {
      emit(errors.head)
      emitEvent(method, Status.Failure)
      errors.tail.foreach(emit)
    }
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
