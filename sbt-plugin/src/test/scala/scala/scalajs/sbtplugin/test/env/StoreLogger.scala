package scala.scalajs.sbtplugin.test.env

import scala.scalajs.tools.logging._

import scala.collection.mutable.ListBuffer

class StoreLogger extends Logger {
  import StoreLogger._

  private[this] val buf = new ListBuffer[LogElem]

  def log(level: Level, message: => String): Unit =
    buf += Log(level, message)
  def success(message: => String): Unit =
    buf += Success(message)
  def trace(t: => Throwable): Unit =
    buf += Trace(t)

  def getLog: List[LogElem] = buf.toList
}

object StoreLogger {

  abstract class LogElem
  case class Log(level: Level, message: String) extends LogElem
  case class Success(message: String) extends LogElem
  case class Trace(t: Throwable) extends LogElem

}
