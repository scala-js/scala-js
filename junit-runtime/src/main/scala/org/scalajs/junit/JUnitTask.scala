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

import sbt.testing._
import scala.scalajs.reflect.Reflect
import scala.util.{Try, Success, Failure}

private[junit] final class JUnitTask(val taskDef: TaskDef,
    runSettings: RunSettings) extends Task {

  def tags: Array[String] = Array.empty

  def execute(eventHandler: EventHandler, loggers: Array[Logger],
      continuation: Array[Task] => Unit): Unit = {
    continuation(execute(eventHandler, loggers))
  }

  def execute(eventHandler: EventHandler, loggers: Array[Logger]): Array[Task] = {
    val reporter = new Reporter(eventHandler, loggers, runSettings, taskDef)

    for (bootstrapper <- loadBootstrapper(reporter)) {
      executeTests(bootstrapper, reporter)
    }

    Array()
  }

  private def executeTests(bootstrapper: Bootstrapper, reporter: Reporter): Unit = {
    reporter.reportRunStarted()

    var failed = 0
    var ignored = 0
    var total = 0

    val (errors, timeInSeconds) = runTestLifecycle {
      ()
    } { _ =>
      bootstrapper.beforeClass()

      for (method <- bootstrapper.tests) {
        if (method.ignored) {
          reporter.reportIgnored(Some(method.name))
          ignored += 1
        } else {
          failed += executeTestMethod(bootstrapper, method, reporter)
          total += 1
        }
      }
    } { _ =>
      bootstrapper.afterClass()
    }

    errors match {
      case e :: Nil if isAssumptionViolation(e) =>
        reporter.reportIgnored(None)
        ignored += 1

      case es =>
        failed += es.size
        reporter.reportErrors("Test ", None, timeInSeconds, es)
    }

    reporter.reportRunFinished(failed, ignored, total, timeInSeconds)
  }

  private[this] def executeTestMethod(bootstrapper: Bootstrapper, test: TestMetadata,
      reporter: Reporter): Int = {
    reporter.reportTestStarted(test.name)

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
        reporter.reportAssumptionViolation(test.name, timeInSeconds, e)
        0

      case es =>
        reporter.reportErrors("Test ", Some(test.name), timeInSeconds, es)
        es.size
    }

    reporter.reportTestFinished(test.name, errors.isEmpty, timeInSeconds)

    // Scala.js-specific: timeouts are warnings only, after the fact
    val timeout = test.annotation.timeout
    if (timeout != 0 && timeout <= timeInSeconds) {
      reporter.log(_.warn, "Timeout: took " + timeInSeconds + " sec, expected " +
          (timeout.toDouble / 1000) + " sec")
    }

    failed
  }

  private def loadBootstrapper(reporter: Reporter): Option[Bootstrapper] = {
    val bootstrapperName =
      taskDef.fullyQualifiedName + "$scalajs$junit$bootstrapper$"

    try {
      val b = Reflect
        .lookupLoadableModuleClass(bootstrapperName)
        .getOrElse(throw new ClassNotFoundException(s"Cannot find $bootstrapperName"))
        .loadModule()

      b match {
        case b: Bootstrapper => Some(b)

        case _ =>
          throw new ClassCastException(s"Expected $bootstrapperName to extend Bootstrapper")
      }
    } catch {
      case t: Throwable =>
        reporter.reportErrors("Error while loading test class ", None, 0, List(t))
        None
    }
  }

  private def handleExpected(expectedException: Class[_ <: Throwable])(body: => Unit) = {
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

  private def isAssumptionViolation(ex: Throwable): Boolean = {
    ex.isInstanceOf[org.junit.AssumptionViolatedException] ||
    ex.isInstanceOf[org.junit.internal.AssumptionViolatedException]
  }
}
