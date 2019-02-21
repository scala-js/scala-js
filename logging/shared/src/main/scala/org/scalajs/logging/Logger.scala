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

package org.scalajs.logging

import scala.concurrent._

/** Abstract logger for our tools. Designed after sbt's Loggers. */
trait Logger {
  def log(level: Level, message: => String): Unit
  def trace(t: => Throwable): Unit

  final def error(message: => String): Unit = log(Level.Error, message)
  final def warn(message: => String): Unit = log(Level.Warn, message)
  final def info(message: => String): Unit = log(Level.Info, message)
  final def debug(message: => String): Unit = log(Level.Debug, message)

  def time(title: String, nanos: Long): Unit =
    debug(s"$title: ${nanos / 1000} us")

  final def timeFuture[A](title: String)(body: => Future[A])(
      implicit ex: ExecutionContext): Future[A] = {
    val startTime = System.nanoTime()
    body.andThen { case t =>
      val endTime = System.nanoTime()
      val elapsedTime = endTime - startTime
      time(title, elapsedTime)
    }
  }

  final def time[A](title: String)(body: => A): A = {
    val startTime = System.nanoTime()
    val result = body
    val endTime = System.nanoTime()
    val elapsedTime = endTime - startTime
    time(title, elapsedTime)
    result
  }
}
