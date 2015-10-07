package org.scalajs.junit

import java.io.ByteArrayOutputStream

import com.novocode.junit.Ansi._
import com.novocode.junit.RichLogger
import org.junit._
import sbt.testing._

final class JUnitExecuteTest(taskDef: TaskDef, runner: JUnitBaseRunner,
    classMetadata: JUnitTestBootstrapper, richLogger: RichLogger,
    eventHandler: EventHandler) {

  lazy val packageName = fullyQualifiedName.split('.').init.mkString(".")
  lazy val className = fullyQualifiedName.split('.').last

  def fullyQualifiedName: String = taskDef.fullyQualifiedName()

  def executeTests(): Unit = {
    val jUnitMetadata = classMetadata.metadata()

    val assumptionViolated = try {
      for (method <- jUnitMetadata.beforeClassMethod)
        classMetadata.invoke(method.name)
      false
    } catch {
      case _: AssumptionViolatedException | _:internal.AssumptionViolatedException =>
        true
    }

    if (assumptionViolated) {
      logFormattedInfo(null, "ignored")
      taskSkipped()
    } else {
      def runWithOrWithoutQuietMode[T](block: => T): T = {
        if (runner.runSettings.quiet) {
          scala.Console.withOut(new ByteArrayOutputStream()) {
            block
          }
        } else {
          block
        }
      }

      runWithOrWithoutQuietMode {
        for (method <- jUnitMetadata.testMethods) {
          method.getIgnoreAnnotation match {
            case Some(ign) =>
              logFormattedInfo(method.name, "ignored")
              ignoreTest(method.name)

            case None =>
              executeTestMethod(classMetadata, method)
          }
        }
      }

      for (method <- jUnitMetadata.afterClassMethod)
        classMetadata.invoke(method.name)
    }
  }

  private[this] def executeTestMethod(classMetadata: JUnitTestBootstrapper,
      method: JUnitMethodMetadata) = {
    val jUnitMetadata = classMetadata.metadata()
    val testClassInstance = classMetadata.newInstance()

    val t0 = System.nanoTime
    def timeInSeconds() = (System.nanoTime - t0).toDouble / 1000000000

    val beforeMethodsAssertionFaild = {
      try {
        for (method <- jUnitMetadata.beforeMethod)
          classMetadata.invoke(testClassInstance, method.name)
        false
      } catch {
        case ex: AssumptionViolatedException =>
          logFormattedInfo(method.name, "started")
          logAssertionWarning(method.name, ex, timeInSeconds())
          true

        case ex: internal.AssumptionViolatedException =>
          logFormattedInfo(method.name, "started")
          logAssertionWarning(method.name, ex, timeInSeconds())
          true
      }
    }

    if (!beforeMethodsAssertionFaild) {
      val testAnnotation = method.getTestAnnotation.get
      val testMethodFailed = {
        try {
          classMetadata.invoke(testClassInstance, method.name)
          executedWithoutExceptions(method.name, testAnnotation, timeInSeconds())
          false
        } catch {
          case ex: Throwable =>
            executedWithExceptions(method.name, testAnnotation, timeInSeconds(), ex)
            true
        }
      }

      if (!testMethodFailed) {
        try {
          for (method <- jUnitMetadata.afterMethod)
            classMetadata.invoke(testClassInstance, method.name)

          if (testAnnotation.timeout != 0 && testAnnotation.timeout <= timeInSeconds) {
            richLogger.warn("Timeout: took " + timeInSeconds + " sec, expected " +
              (testAnnotation.timeout.toDouble / 1000) + " sec")
          }
        } catch {
          case ex: Throwable =>
            logFormattedError(method.name, "failed: on @AfterClass method", Some(ex))
            val selector = new NestedTestSelector(fullyQualifiedName, method.name)
            eventHandler.handle(new JUnitEvent(taskDef, Status.Failure, selector))
        }
      }
    }
  }

  private[this] def executedWithoutExceptions(methodName: String,
    testAnnotation: org.junit.Test, timeInSeconds: Double) = {
    if (testAnnotation.expected == classOf[org.junit.Test.None]) {
      if (runner.runSettings.verbose)
        logFormattedInfo(methodName, "started")
      taskPassed(methodName)
    } else {
      val msg = {
        s"failed: Expected exception: ${testAnnotation.expected} " +
        s"took $timeInSeconds sec"
      }
      logFormattedError(methodName, msg, None)
      taskFailed(methodName)
    }
    runner.taskRegisterTotal()
  }

  private[this] def executedWithExceptions(methodName: String,
      testAnnotation: org.junit.Test, timeInSeconds: Double, ex: Throwable) = {
    if (classOf[AssumptionViolatedException].isInstance(ex) ||
        classOf[internal.AssumptionViolatedException].isInstance(ex)) {
      logFormattedInfo(methodName, "started")
      logAssertionWarning(methodName, ex, timeInSeconds)
      taskSkipped()
    } else if (testAnnotation.expected.isInstance(ex)) {
      if (runner.runSettings.verbose)
        logFormattedInfo(methodName, "started")
      taskPassed(methodName)
    } else if (testAnnotation.expected == classOf[org.junit.Test.None]) {
      val failedMsg = new StringBuilder
      failedMsg ++= "failed: "
      if (ex.isInstanceOf[AssertionError] && runner.runSettings.logAssert) {
        failedMsg ++= "java.lang." ++= c("AssertionError", ERRMSG) ++= ": "
        failedMsg ++= ex.getMessage
      } else if (runner.runSettings.logExceptionClass) {
        failedMsg ++= ex.getMessage
      } else {
        failedMsg ++= ex.getClass.toString ++= " expected<"
        failedMsg ++= testAnnotation.expected.toString ++= "> but was<"
        failedMsg ++= ex.getClass.toString += '>'
      }
      failedMsg += ','
      val msg = s"$failedMsg took $timeInSeconds sec"
      val exOpt = {
        if (!ex.isInstanceOf[AssertionError] || runner.runSettings.logAssert) Some(ex)
        else None
      }
      logFormattedError(methodName, msg, exOpt)
      taskFailed(methodName)
    } else {
      val msg = s"failed: ${ex.getClass}, took $timeInSeconds sec"
      logFormattedError(methodName, msg, Some(ex))
      taskFailed(methodName)
    }
    runner.taskRegisterTotal()
  }

  private def ignoreTest(methodName: String) = {
    runner.taskIgnored()
    runner.taskRegisterTotal()
    val selector = new NestedTestSelector(fullyQualifiedName, methodName)
    eventHandler.handle(new JUnitEvent(taskDef, Status.Ignored, selector))
  }

  private def taskSkipped(): Unit = {
    runner.taskSkipped()
    val selector = new TestSelector(fullyQualifiedName)
    eventHandler.handle(new JUnitEvent(taskDef, Status.Skipped, selector))
  }

  private def taskFailed(methodName: String): Unit = {
    runner.taskFailed()
    val selector = new NestedTestSelector(fullyQualifiedName, methodName)
    eventHandler.handle(new JUnitEvent(taskDef, Status.Failure, selector))
  }

  private def taskPassed(methodName: String): Unit = {
    runner.taskPassed()
    val selector = new NestedTestSelector(fullyQualifiedName, methodName)
    eventHandler.handle(new JUnitEvent(taskDef, Status.Success, selector))
  }

  private[this] def logAssertionWarning(methodName: String, ex: Throwable,
      timeInSeconds: Double): Unit = {
    val msg = {
      "failed: org.junit." + c("AssumptionViolatedException", ERRMSG) +
        ": " + ex.getMessage + ", took " + timeInSeconds + " sec"
    }
    logFormattedWarn("Test assumption in test ", methodName, msg)
  }

  private[this] def logFormattedInfo(method: String, msg: String): Unit = {
    val fMethod = if (method != null) c(method, NNAME2) else null
    richLogger.info(
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
