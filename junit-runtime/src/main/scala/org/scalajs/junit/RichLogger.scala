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

import sbt.testing.Logger

private[junit] final class RichLogger(loggers: Array[Logger],
    settings: RunSettings, testClassName: String) {

  def debug(s: String): Unit = {
    for (l <- loggers)
      l.debug(filterAnsiIfNeeded(l, s))
  }

  def error(s: String): Unit = {
    for (l <- loggers)
      l.error(filterAnsiIfNeeded(l, s))
  }

  def info(s: String): Unit = {
    for (l <- loggers)
      l.info(filterAnsiIfNeeded(l, s))
  }

  def warn(s: String): Unit = {
    for (l <- loggers)
      l.warn(filterAnsiIfNeeded(l, s))
  }

  private def filterAnsiIfNeeded(l: Logger, s: String): String =
    if (l.ansiCodesSupported() && settings.color) s
    else Ansi.filterAnsi(s)

  def trace(t: Throwable): Unit = {
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
    logStackTracePart(trace, m, trace.length - m - 1, t, testClassName, testFileName)
  }

  private def logStackTracePart(trace: Array[StackTraceElement], m: Int,
      framesInCommon: Int, t: Throwable, testClassName: String,
      testFileName: String): Unit = {
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
      error("    at " +
        stackTraceElementToString(trace(i), testClassName, testFileName))
    }
    if (m0 != m2) {
      // skip junit-related frames
      error("    ...")
    } else if (framesInCommon != 0) {
      // skip frames that were in the previous trace too
      error("    ... " + framesInCommon + " more")
    }
    logStackTraceAsCause(trace, t.getCause, testClassName, testFileName)
  }

  private def logStackTraceAsCause(causedTrace: Array[StackTraceElement],
      t: Throwable, testClassName: String, testFileName: String): Unit = {
    if (t != null) {
      val trace = t.getStackTrace
      var m = trace.length - 1
      var n = causedTrace.length - 1
      while (m >= 0 && n >= 0 && trace(m) == causedTrace(n)) {
        m -= 1
        n -= 1
      }
      error("Caused by: " + t)
      logStackTracePart(trace, m, trace.length - 1 - m, t, testClassName, testFileName)
    }
  }

  private def findTestFileName(trace: Array[StackTraceElement]): String = {
    trace.collectFirst {
      case e if testClassName.equals(e.getClassName) => e.getFileName
    }.orNull
  }

  private def stackTraceElementToString(e: StackTraceElement,
      testClassName: String, testFileName: String): String = {
    val highlight = settings.color && {
      testClassName == e.getClassName ||
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
