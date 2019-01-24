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

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import scala.util.{Try, Success, Failure}

import scala.scalajs.reflect.Reflect

import sbt.testing._

/* Implementation note: In JUnitTask we use Future[Try[Unit]] instead of simply
 * Future[Unit]. This is to prevent Scala's Future implementation to box/wrap
 * fatal errors (most importantly AssertionError) in ExecutionExceptions. We
 * need to prevent the wrapping in order to hide the fact that we use async
 * under the hood and stay consistent with JVM JUnit.
 */
private[junit] final class JUnitTask(val taskDef: TaskDef,
    runSettings: RunSettings) extends Task {

  def tags: Array[String] = Array.empty

  def execute(eventHandler: EventHandler, loggers: Array[Logger],
      continuation: Array[Task] => Unit): Unit = {
    val reporter = new Reporter(eventHandler, loggers, runSettings, taskDef)

    val result = loadBootstrapper(reporter).fold {
      Future.successful(())
    } { bootstrapper =>
      executeTests(bootstrapper, reporter)
    }

    result.foreach(_ => continuation(Array()))
  }

  private def executeTests(bootstrapper: Bootstrapper, reporter: Reporter): Future[Unit] = {
    reporter.reportRunStarted()

    var failed = 0
    var ignored = 0
    var total = 0

    def runTests(tests: List[TestMetadata]): Future[Try[Unit]] = {
      val (nextIgnored, other) = tests.span(_.ignored)

      nextIgnored.foreach(t => reporter.reportIgnored(Some(t.name)))
      ignored += nextIgnored.size

      other match {
        case t :: ts =>
          total += 1
          executeTestMethod(bootstrapper, t, reporter).flatMap { fc =>
            failed += fc
            runTests(ts)
          }

        case Nil =>
          Future.successful(Success(()))
      }
    }

    val result = runTestLifecycle {
      Success(())
    } { _ =>
      catchAll(bootstrapper.beforeClass())
    } { _ =>
      runTests(bootstrapper.tests.toList)
    } { _ =>
      catchAll(bootstrapper.afterClass())
    }

    for {
      (errors, timeInSeconds) <- result
    } yield {
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
  }

  private[this] def executeTestMethod(bootstrapper: Bootstrapper, test: TestMetadata,
      reporter: Reporter): Future[Int] = {
    reporter.reportTestStarted(test.name)

    val result = runTestLifecycle {
      catchAll(bootstrapper.newInstance())
    } { instance =>
      catchAll(bootstrapper.before(instance))
    } { instance =>
      handleExpected(test.annotation.expected) {
        catchAll(bootstrapper.invokeTest(instance, test.name)) match {
          case Success(f) => f.recover { case t => Failure(t) }
          case Failure(t) => Future.successful(Failure(t))
        }
      }
    } { instance =>
      catchAll(bootstrapper.after(instance))
    }

    for {
      (errors, timeInSeconds) <- result
    } yield {
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

  private def handleExpected(expectedException: Class[_ <: Throwable])(body: => Future[Try[Unit]]) = {
    val wantException = expectedException != classOf[org.junit.Test.None]

    if (wantException) {
      for (r <- body) yield {
        r match {
          case Success(_) =>
            Failure(new AssertionError("Expected exception: " + expectedException.getName))

          case Failure(t) if expectedException.isInstance(t) =>
            Success(())

          case Failure(t) =>
            val expName = expectedException.getName
            val gotName = t.getClass.getName
            Failure(new Exception(s"Unexpected exception, expected<$expName> but was<$gotName>", t))
        }
      }
    } else {
      body
    }
  }

  private def runTestLifecycle[T](build: => Try[T])(before: T => Try[Unit])(
      body: T => Future[Try[Unit]])(
      after: T => Try[Unit]): Future[(List[Throwable], Double)] = {
    val startTime = System.nanoTime

    val exceptions: Future[List[Throwable]] = build match {
      case Success(x) =>
        val bodyFuture = before(x) match {
          case Success(()) => body(x)
          case Failure(t)  => Future.successful(Failure(t))
        }

        for (bodyResult <- bodyFuture) yield {
          val afterException = after(x).failed.toOption
          bodyResult.failed.toOption.toList ++ afterException.toList
        }

      case Failure(t) =>
        Future.successful(List(t))
    }

    for (es <- exceptions) yield {
      val timeInSeconds = (System.nanoTime - startTime).toDouble / 1000000000
      (es, timeInSeconds)
    }
  }

  private def isAssumptionViolation(ex: Throwable): Boolean = {
    ex.isInstanceOf[org.junit.AssumptionViolatedException] ||
    ex.isInstanceOf[org.junit.internal.AssumptionViolatedException]
  }

  private def catchAll[T](body: => T): Try[T] = {
    try {
      Success(body)
    } catch {
      case t: Throwable => Failure(t)
    }
  }

  def execute(eventHandler: EventHandler, loggers: Array[Logger]): Array[Task] =
    throw new UnsupportedOperationException("Supports JS only")
}
