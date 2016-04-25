package org.scalajs.jsenv.test

import org.scalajs.core.tools.logging._

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

  sealed trait LogElem
  final case class Log(level: Level, message: String) extends LogElem
  final case class Success(message: String) extends LogElem
  final case class Trace(t: Throwable) extends LogElem

}
