package scala.scalajs.tools.logging

object NullLogger extends Logger {
  def log(level: Level, message: => String): Unit = {}
  def success(message: => String): Unit = {}
  def trace(t: => Throwable): Unit = {}
}
