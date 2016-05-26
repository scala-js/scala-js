package org.scalajs.junit

import java.io.ByteArrayOutputStream

import com.novocode.junit.Ansi._
import com.novocode.junit.RichLogger
import org.junit._
import sbt.testing._

import scala.util.matching.Regex

final class JUnitExecuteTest(taskDef: TaskDef, runner: JUnitBaseRunner,
    classMetadata: JUnitTestBootstrapper, richLogger: RichLogger,
    eventHandler: EventHandler) {

  private val verbose = runner.runSettings.verbose
  private val decodeScalaNames = runner.runSettings.decodeScalaNames

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

    def logTestIgnored(name: String): Unit = {
      logFormattedInfo(name, "ignored")
    }

    if (assumptionViolated) {
      logTestIgnored(null)
      ignoreTestClass()
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
              logTestIgnored(method.name)
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
    val methodName = method.name
    val decodedMethodName = {
      if (decodeScalaNames) runner.runSettings.decodeName(methodName)
      else methodName
    }
    val testAnnotation = method.getTestAnnotation.get

    val t0 = System.nanoTime
    def getTimeInSeconds(): Double = (System.nanoTime - t0).toDouble / 1000000000

    def executeTestMethods(): Unit = {
      val expectedException = testAnnotation.expected
      try {
        if (verbose) logFormattedInfo(decodedMethodName, "started")
        else logFormattedDebug(decodedMethodName, "started")

        classMetadata.invoke(testClassInstance, method.name)
        logFormattedDebug(decodedMethodName,
            s"finished, took ${getTimeInSeconds()} sec")

        if (expectedException == classOf[org.junit.Test.None]) {
          testPassed(methodName)
          executeAfterMethods()
        } else {
          val msg = {
            s"failed: Expected exception: " + expectedException +
            s"took ${getTimeInSeconds()} sec"
          }
          logFormattedError(decodedMethodName, msg, None)
          testFailed(methodName)
        }
      } catch {
        case ex: Throwable =>
          val timeInSeconds = getTimeInSeconds()
          if (ex.isInstanceOf[AssumptionViolatedException] ||
              ex.isInstanceOf[internal.AssumptionViolatedException]) {
            logAssertionWarning(decodedMethodName, ex, timeInSeconds)
            testSkipped()
          } else if (expectedException.isInstance(ex)) {
            testPassed(methodName)
            executeAfterMethods()
          } else if (expectedException == classOf[org.junit.Test.None]) {
            val isAssertion = ex.isInstanceOf[AssertionError]
            val failedMsg = new StringBuilder
            failedMsg ++= "failed: "
            if (!runner.runSettings.notLogExceptionClass &&
                (!isAssertion || runner.runSettings.logAssert)) {
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
              if (!isAssertion || runner.runSettings.logAssert) Some(ex)
              else None
            }
            logFormattedError(decodedMethodName, msg, exOpt)
            testFailed(methodName)
          } else {
            val msg = s"failed: ${ex.getClass}, took $timeInSeconds sec"
            logFormattedError(decodedMethodName, msg, Some(ex))
            testFailed(methodName)
          }
          logFormattedDebug(decodedMethodName,
              s"finished, took $timeInSeconds sec")
      }
      runner.testRegisterTotal()
    }

    def executeAfterMethods(): Unit = {
      try {
        for (method <- jUnitMetadata.afterMethod)
          classMetadata.invoke(testClassInstance, method.name)

        val timeInSeconds = getTimeInSeconds()
        if (testAnnotation.timeout != 0 && testAnnotation.timeout <= timeInSeconds) {
          richLogger.warn("Timeout: took " + timeInSeconds + " sec, expected " +
              (testAnnotation.timeout.toDouble / 1000) + " sec")
        }
      } catch {
        case ex: Throwable =>
          logFormattedError(methodName, "failed: on @AfterClass method", Some(ex))
          val selector = new NestedTestSelector(fullyQualifiedName, methodName)
          eventHandler.handle(new JUnitEvent(taskDef, Status.Failure, selector))
      }
    }

    try {
      for (method <- jUnitMetadata.beforeMethod) {
        classMetadata.invoke(testClassInstance, method.name)
      }
      executeTestMethods()
    } catch {
      case ex: AssumptionViolatedException =>
        logAssertionWarning(methodName, ex, getTimeInSeconds())

      case ex: internal.AssumptionViolatedException =>
        logAssertionWarning(methodName, ex, getTimeInSeconds())
    }
  }

  private def ignoreTest(methodName: String) = {
    runner.testIgnored()
    val selector = new NestedTestSelector(fullyQualifiedName, methodName)
    eventHandler.handle(new JUnitEvent(taskDef, Status.Skipped, selector))
  }

  private def ignoreTestClass() = {
    runner.testIgnored()
    val selector = new TestSelector(fullyQualifiedName)
    eventHandler.handle(new JUnitEvent(taskDef, Status.Skipped, selector))
  }

  private def testSkipped(): Unit = {
    runner.testSkipped()
    val selector = new TestSelector(fullyQualifiedName)
    eventHandler.handle(new JUnitEvent(taskDef, Status.Skipped, selector))
  }

  private def testFailed(methodName: String): Unit = {
    runner.testFailed()
    val selector = new NestedTestSelector(fullyQualifiedName, methodName)
    eventHandler.handle(new JUnitEvent(taskDef, Status.Failure, selector))
  }

  private def testPassed(methodName: String): Unit = {
    runner.testPassed()
    val selector = new NestedTestSelector(fullyQualifiedName, methodName)
    eventHandler.handle(new JUnitEvent(taskDef, Status.Success, selector))
  }

  private[this] def logAssertionWarning(methodName: String, ex: Throwable,
      timeInSeconds: Double): Unit = {
    val msg = {
      "failed: org.junit.internal." + c("AssumptionViolatedException", ERRMSG) +
      ": " + ex.getMessage + ", took " + timeInSeconds + " sec"
    }
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
