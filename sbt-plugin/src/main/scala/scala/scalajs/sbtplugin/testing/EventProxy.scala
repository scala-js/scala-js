/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.sbtplugin.testing

import org.mozilla.javascript.ScriptStackElement
import org.mozilla.javascript.NativeArray
import sbt.testing.Logger
import sbt.testing.EventHandler
import org.mozilla.javascript.NativeObject
import scala.scalajs.sbtplugin.sourcemap.SourceMapper
import scala.scalajs.sbtplugin.sourcemap.SourceMappedException

case class EventProxy(handler: EventHandler, loggers: Array[Logger], events: Events) {

  import events._

  private[testing] def error(message: String, stack: Array[ScriptStackElement]): Unit = {
    val sourceMappedStack = SourceMapper.map(stack)
    logWithEvent(_.error,
        messageWithStack(message, sourceMappedStack),
        Error(new SourceMappedException(message, sourceMappedStack)))
  }

  def error(message: String, stack: NativeArray): Unit =
    error(message, fromNativeArray(stack))

  def failure(message: String, stack: NativeArray): Unit =
    failure(message, fromNativeArray(stack))

  private[testing] def failure(message: String, stack: Array[ScriptStackElement]) = {
    logWithEvent(_.error,
        messageWithStack(message, stack),
        Failure(new SourceMappedException(message, stack)))
  }

  def succeeded(message: String): Unit =
    logWithEvent(_.info, message, Succeeded)

  def skipped(message: String): Unit =
    logWithEvent(_.info, message, Skipped)

  def pending(message: String): Unit =
    logWithEvent(_.info, message, Pending)

  def ignored(message: String): Unit =
    logWithEvent(_.info, message, Ignored)

  def canceled(message: String): Unit =
    logWithEvent(_.info, message, Canceled)

  def info(message: String): Unit =
    log(_.info, message)

  def error(message: String): Unit =
    log(_.error, message)

  private def messageWithStack(message: String, stack: Array[ScriptStackElement]): String =
    message + stack.mkString("\n", "\n", "")

  private def log(method: Logger => (String => Unit), message: String): Unit = {
    for (logger <- loggers) {
      var loggedMessage = message
      if (!logger.ansiCodesSupported) removeColors(loggedMessage)
      method(logger)(loggedMessage)
    }
  }

  private def logWithEvent(method: Logger => (String => Unit), message: String, event: Event): Unit = {
    handler handle event
    log(method, message)
  }

  private def fromNativeArray(stack: NativeArray) = {
    stack.toArray.map {
      case o: NativeObject =>
        new ScriptStackElement(
          o.get("fileName").asInstanceOf[String],
          o.get("functioName").asInstanceOf[String],
          o.get("lineNumber").asInstanceOf[Double].toInt)
    }
  }

  private val colorPattern = raw"\033\[\d{1, 2}m"

  private def removeColors(message: String): String =
    message.replaceAll(colorPattern, "")
}
