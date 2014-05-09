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
import scala.scalajs.tools.classpath.{CompleteClasspath, CompleteIRClasspath}

import scala.collection.mutable.ArrayBuffer

import scala.util.Try

import java.util.regex._

/** This parses the messages sent from the test bridge and forwards
 *  the calls to SBT. It also buffers all log messages and allows to
 *  pipe them to multiple loggers in a synchronized fashion. This
 *  ensures that log messages aren't interleaved due to parallelism.
 */
class TestOutputConsole(
    base: JSConsole,
    handler: EventHandler,
    events: Events,
    classpath: CompleteClasspath,
    noSourceMap: Boolean) extends JSConsole {

  import TestOutputConsole._
  import events._

  private val traceBuf = new ArrayBuffer[StackTraceElement]
  private val logBuffer = new ArrayBuffer[LogElement]

  /* See #727: source mapping does not work with CompleteIRClasspath, so
   * don't bother to try.
   */
  private val ignoreSourceMapping =
    noSourceMap || classpath.isInstanceOf[CompleteIRClasspath]

  private lazy val sourceMapper = new SourceMapper(classpath)

  override def log(msg: Any): Unit = {
    val data = msg.toString
    val sepPos = data.indexOf("|")

    if (sepPos == -1)
      log(_.error, s"Malformed message: $data")
    else {
      val op = data.substring(0, sepPos)
      val message = unescape(data.substring(sepPos + 1))

      op match {
        case "console-log" =>
          base.log(message)
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
        case "error-log" =>
          noTrace()
          log(_.error, message)
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

          if (ignoreSourceMapping)
            traceBuf += ste
          else
            traceBuf += sourceMapper.map(ste, columnNumber)
        case _ =>
          noTrace()
          log(_.error, s"Unknown op: $op. Originating log message: $data")
      }
    }
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

  private def log(method: LogMethod, message: String): Unit =
    logBuffer.append(LogElement(method, message))

  private def logWithEvent(method: LogMethod,
      message: String, event: Event): Unit = {
    handler handle event
    log(method, message)
  }

  def pipeLogsTo(loggers: Array[Logger]): Unit = {
    TestOutputConsole.synchronized {
      for {
        LogElement(method, message) <- logBuffer
        logger <- loggers
      } method(logger) {
        if (logger.ansiCodesSupported) message
        else removeColors(message)
      }
    }
  }

  def allLogs: List[LogElement] = logBuffer.toList

  private val colorPattern = raw"\033\[\d{1,2}m"

  private def removeColors(message: String): String =
    message.replaceAll(colorPattern, "")

  private val unEscPat = Pattern.compile("(\\\\\\\\|\\\\n|\\\\r)")
  private def unescape(message: String): String = {
    val m = unEscPat.matcher(message)
    val res = new StringBuffer()
    while (m.find()) {
      val repl = m.group() match {
        case "\\\\" => "\\\\"
        case "\\n" => "\n"
        case "\\r" => "\r"
      }
      m.appendReplacement(res, repl);
    }
    m.appendTail(res);
    res.toString
  }

}

object TestOutputConsole {
  type LogMethod = Logger => (String => Unit)
  case class LogElement(method: LogMethod, message: String)
}
