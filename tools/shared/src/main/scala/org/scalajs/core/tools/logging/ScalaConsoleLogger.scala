package org.scalajs.core.tools.logging

class ScalaConsoleLogger(minLevel: Level = Level.Debug) extends Logger {

  def log(level: Level, message: =>String): Unit = if (level >= minLevel) {
    if (level == Level.Warn || level == Level.Error)
      scala.Console.err.println(message)
    else
      scala.Console.out.println(message)
  }
  def success(message: => String): Unit = info(message)
  def trace(t: => Throwable): Unit =
    // This is error level, so no checking
    t.printStackTrace()
}
