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

class ScalaConsoleLogger(minLevel: Level = Level.Debug) extends Logger {
  def log(level: Level, message: => String): Unit = level match {
    case _ if level < minLevel    =>
    case Level.Warn | Level.Error => scala.Console.err.println(message)
    case Level.Info | Level.Debug => scala.Console.out.println(message)
  }

  def trace(t: => Throwable): Unit = {
    // This is error level, so no checking
    t.printStackTrace()
  }
}
