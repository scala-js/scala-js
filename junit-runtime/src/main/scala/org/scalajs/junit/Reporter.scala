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

private[junit] final class Reporter(eventHandler: EventHandler,
    loggers: Array[Logger], settings: RunSettings, taskDef: TaskDef) {

  def reportRunStarted(): Unit =
    log(infoOrDebug, Ansi.c("Test run started", Ansi.BLUE))

  def reportRunFinished(failed: Int, ignored: Int, total: Int,
      timeInSeconds: Double): Unit = {
    val msg = {
      Ansi.c("Test run finished: ", Ansi.BLUE) +
      Ansi.c(s"$failed failed", if (failed == 0) Ansi.BLUE else Ansi.RED) +
      Ansi.c(s", ", Ansi.BLUE) +
      Ansi.c(s"$ignored ignored", if (ignored == 0) Ansi.BLUE else Ansi.YELLOW) +
      Ansi.c(s", $total total, ${timeInSeconds}s", Ansi.BLUE)
    }

    log(infoOrDebug, msg)
  }

  def reportIgnored(method: Option[String]): Unit = {
    logTestInfo(_.info, method, "ignored")
    emitEvent(method, Status.Skipped)
  }

  def reportTestStarted(method: String): Unit =
    logTestInfo(infoOrDebug, Some(method), "started")

  def reportTestFinished(method: String, succeeded: Boolean, timeInSeconds: Double): Unit = {
    logTestInfo(_.debug, Some(method), s"finished, took $timeInSeconds sec")

    if (succeeded)
      emitEvent(Some(method), Status.Success)
  }

  def reportErrors(prefix: String, method: Option[String],
      timeInSeconds: Double, errors: List[Throwable]): Unit = {
    def emit(t: Throwable) = {
      logTestException(_.error, prefix, method, t, timeInSeconds)
      trace(t)
    }

    if (errors.nonEmpty) {
      emit(errors.head)
      emitEvent(method, Status.Failure)
      errors.tail.foreach(emit)
    }
  }

  def reportAssumptionViolation(method: String, timeInSeconds: Double, e: Throwable): Unit = {
    logTestException(_.warn, "Test assumption in test ", Some(method), e,
        timeInSeconds)
    emitEvent(Some(method), Status.Skipped)
  }

  private def logTestInfo(level: Reporter.Level, method: Option[String], msg: String): Unit =
    log(level, s"Test ${formatTest(method, Ansi.CYAN)} $msg")

  private def logTestException(level: Reporter.Level, prefix: String,
      method: Option[String], ex: Throwable, timeInSeconds: Double): Unit = {
    val logException = {
      !settings.notLogExceptionClass &&
      (settings.logAssert || !ex.isInstanceOf[AssertionError])
    }

    val fmtName = if (logException) {
      val name = {
        if (ex.isInstanceOf[AssumptionViolatedException])
          classOf[internal.AssumptionViolatedException].getName
        else
          ex.getClass.getName
      }

      formatClass(name, Ansi.RED) + ": "
    } else {
      ""
    }

    val m = formatTest(method, Ansi.RED)
    val msg = s"$prefix$m failed: $fmtName${ex.getMessage}, took $timeInSeconds sec"
    log(level, msg)
  }

  private def trace(t: Throwable): Unit = {
    if (!t.isInstanceOf[AssertionError] || settings.logAssert) {
      logTrace(t)
    }
  }

  private def infoOrDebug: Reporter.Level =
    if (settings.verbose) _.info
    else _.debug

  private def formatTest(method: Option[String], color: String): String = {
    method.fold(formattedTestClass) { method =>
      val fmtMethod = Ansi.c(settings.decodeName(method), color)
      s"$formattedTestClass.$fmtMethod"
    }
  }

  private lazy val formattedTestClass = formatClass(taskDef.fullyQualifiedName, Ansi.YELLOW)

  private def formatClass(fullName: String, color: String): String = {
    val (prefix, name) = fullName.splitAt(fullName.lastIndexOf(".") + 1)
    prefix + Ansi.c(name, color)
  }

  private def emitEvent(method: Option[String], status: Status): Unit = {
    val testName = method.fold(taskDef.fullyQualifiedName)(method =>
        taskDef.fullyQualifiedName + "." + settings.decodeName(method))
    val selector = new TestSelector(testName)
    eventHandler.handle(new JUnitEvent(taskDef, status, selector))
  }

  def log(level: Reporter.Level, s: String): Unit = {
    for (l <- loggers)
      level(l)(filterAnsiIfNeeded(l, s))
  }

  private def filterAnsiIfNeeded(l: Logger, s: String): String =
    if (l.ansiCodesSupported() && settings.color) s
    else Ansi.filterAnsi(s)

  private def logTrace(t: Throwable): Unit = {
    val trace = t.getStackTrace.dropWhile { p =>
      p.getFileName != null && {
        p.getFileName.contains("StackTrace.scala") ||
        p.getFileName.contains("Throwables.scala")
      }
    }
    val testFileName = {
      if (settings.color) findTestFileName(trace)
      else null
    }
    val i = trace.indexWhere {
      p => p.getFileName != null && p.getFileName.contains("JUnitExecuteTest.scala")
    } - 1
    val m = if (i > 0) i else trace.length - 1
    logStackTracePart(trace, m, trace.length - m - 1, t, testFileName)
  }

  private def logStackTracePart(trace: Array[StackTraceElement], m: Int,
      framesInCommon: Int, t: Throwable, testFileName: String): Unit = {
    val m0 = m
    var m2 = m
    var top = 0
    var i = top
    while (i <= m2) {
      if (trace(i).toString.startsWith("org.junit.") ||
          trace(i).toString.startsWith("org.hamcrest.")) {
        if (i == top) {
          top += 1
        } else {
          m2 = i - 1
          var break = false
          while (m2 > top && !break) {
            val s = trace(m2).toString
            if (!s.startsWith("java.lang.reflect.") &&
                !s.startsWith("sun.reflect.")) {
              break = true
            } else {
              m2 -= 1
            }
          }
          i = m2 // break
        }
      }
      i += 1
    }

    for (i <- top to m2) {
      log(_.error, "    at " +
        stackTraceElementToString(trace(i), testFileName))
    }
    if (m0 != m2) {
      // skip junit-related frames
      log(_.error, "    ...")
    } else if (framesInCommon != 0) {
      // skip frames that were in the previous trace too
      log(_.error, "    ... " + framesInCommon + " more")
    }
    logStackTraceAsCause(trace, t.getCause, testFileName)
  }

  private def logStackTraceAsCause(causedTrace: Array[StackTraceElement],
      t: Throwable, testFileName: String): Unit = {
    if (t != null) {
      val trace = t.getStackTrace
      var m = trace.length - 1
      var n = causedTrace.length - 1
      while (m >= 0 && n >= 0 && trace(m) == causedTrace(n)) {
        m -= 1
        n -= 1
      }
      log(_.error, "Caused by: " + t)
      logStackTracePart(trace, m, trace.length - 1 - m, t, testFileName)
    }
  }

  private def findTestFileName(trace: Array[StackTraceElement]): String =
    trace.find(_.getClassName == taskDef.fullyQualifiedName).map(_.getFileName).orNull

  private def stackTraceElementToString(e: StackTraceElement, testFileName: String): String = {
    val highlight = settings.color && {
      taskDef.fullyQualifiedName == e.getClassName ||
      (testFileName != null && testFileName == e.getFileName)
    }
    var r = ""
    r += settings.decodeName(e.getClassName + '.' + e.getMethodName)
    r += '('

    if (e.isNativeMethod) {
      r += Ansi.c("Native Method", if (highlight) Ansi.YELLOW else null)
    } else if (e.getFileName == null) {
      r += Ansi.c("Unknown Source", if (highlight) Ansi.YELLOW else null)
    } else {
      r += Ansi.c(e.getFileName, if (highlight) Ansi.MAGENTA else null)
      if (e.getLineNumber >= 0) {
        r += ':'
        r += Ansi.c(String.valueOf(e.getLineNumber), if (highlight) Ansi.YELLOW else null)
      }
    }
    r += ')'
    r
  }
}

private[junit] object Reporter {
  type Level = Logger => (String => Unit)
}
