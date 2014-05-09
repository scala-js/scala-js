/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.sbtplugin.testing

import sbt.testing.Logger
import sbt.testing.EventHandler

import scala.scalajs.tools.env.JSConsole
import scala.scalajs.tools.sourcemap.SourceMapper
import scala.scalajs.tools.classpath.JSClasspath

import scala.collection.mutable.ArrayBuffer

import scala.util.Try

class TestOutputConsole(
    base: JSConsole,
    handler: EventHandler,
    loggers: Array[Logger],
    events: Events,
    classpath: JSClasspath,
    noSourceMap: Boolean) extends JSConsole {

  import events._

  private val messagePrefix = "``|%^scala.js-test-comm&&"

  private val traceBuf = new ArrayBuffer[StackTraceElement]

  private lazy val sourceMapper = new SourceMapper(classpath)

  override def log(msg: Any): Unit = {
    val msgStr = msg.toString
    if (msgStr.startsWith(messagePrefix)) {
      val data = msgStr.stripPrefix(messagePrefix)

      val sepPos = data.indexOf("|")

      if (sepPos == -1)
        log(_.error, s"Malformed message: $msgStr")
      else {
        val op = data.substring(0, sepPos)
        val message = data.substring(sepPos + 1)

        op match {
          case "error" =>
            val trace = getTrace()
            logWithEvent(_.error,
              messageWithStack(message, trace),
              Error(new TestException(message, trace))
            )
          case "failure" =>
            val trace = getTrace()
            logWithEvent(_.error,
              messageWithStack(message, trace),
              Failure(new TestException(message, trace))
            )
          case "succeeded" =>
            noTrace()
            logWithEvent(_.info, message, Succeeded)
          case "skipped" =>
            noTrace()
            logWithEvent(_.info, message, Skipped)
          case "pending" =>
            noTrace()
            logWithEvent(_.info, message, Pending)
          case "ignored" =>
            noTrace()
            logWithEvent(_.info, message, Ignored)
          case "canceled" =>
            noTrace()
            logWithEvent(_.info, message, Canceled)
          case "info" =>
            noTrace()
            log(_.info, message)
          case "warn" =>
            noTrace()
            log(_.warn, message)
          case "trace" =>
            val Array(className, methodName, fileName,
                lineNumberStr, columnNumberStr) = message.split('|')

            def tryParse(num: String, name: String) = Try(num.toInt).getOrElse {
              log(_.warn, s"Couldn't parse $name number in StackTrace: $num")
              -1
            }

            val lineNumber   = tryParse(lineNumberStr, "line")
            val columnNumber = tryParse(columnNumberStr, "column")

            val ste =
              new StackTraceElement(className, methodName, fileName, lineNumber)

            if (noSourceMap)
              traceBuf += ste
            else
              traceBuf += sourceMapper.map(ste, columnNumber)
          case _ =>
            noTrace()
            log(_.error, s"Unknown op: $op. Originating log message: $msgStr")
        }
      }
    } else base.log(msg)
  }

  private def noTrace() = {
    if (traceBuf.nonEmpty)
      log(_.warn, s"Discarding ${traceBuf.size} stack elements")
    traceBuf.clear()
  }

  private def getTrace() = {
    val res = traceBuf.toArray
    traceBuf.clear()
    res
  }

  private def messageWithStack(message: String, stack: Array[StackTraceElement]): String =
    message + stack.mkString("\n", "\n", "")

  private def log(method: Logger => (String => Unit), message: String): Unit = {
    for (logger <- loggers) {
      method(logger) {
        if (logger.ansiCodesSupported) message
        else removeColors(message)
      }
    }
  }

  private def logWithEvent(method: Logger => (String => Unit),
      message: String, event: Event): Unit = {
    handler handle event
    log(method, message)
  }

  private val colorPattern = raw"\033\[\d{1,2}m"

  private def removeColors(message: String): String =
    message.replaceAll(colorPattern, "")
}
