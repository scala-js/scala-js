/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.logging

/** Abstract logger for our tools. Designed after sbt's Loggers. */
trait Logger {
  def log(level: Level, message: => String): Unit
  def success(message: => String): Unit
  def trace(t: => Throwable): Unit

  def error(message: => String): Unit = log(Level.Error, message)
  def warn(message: => String): Unit = log(Level.Warn, message)
  def info(message: => String): Unit = log(Level.Info, message)
  def debug(message: => String): Unit = log(Level.Debug, message)

  def time(title: String, nanos: Long): Unit =
    debug(s"$title: ${nanos / 1000} us")

  final def time[A](title: String)(body: => A): A = {
    val startTime = System.nanoTime()
    val result = body
    val endTime = System.nanoTime()
    val elapsedTime = endTime - startTime
    time(title, elapsedTime)
    result
  }
}
