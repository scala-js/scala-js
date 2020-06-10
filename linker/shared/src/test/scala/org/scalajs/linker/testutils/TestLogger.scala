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

package org.scalajs.linker.testutils

import scala.collection.mutable

import org.scalajs.logging._

import org.junit.Assert._

final class TestLogger extends Logger {
  private val logLines = mutable.ListBuffer.empty[String]

  def log(level: Level, message: => String): Unit = {
    if (level == Level.Error)
      logLines += message
  }

  def trace(t: => Throwable): Unit =
    logLines += t.toString()

  def assertContainsLogLine(expected: String): Unit = {
    if (!containsLogLine(expected))
      fail(s"expected a log line containing '$expected', but got\n$show")
  }

  def containsLogLine(expected: String): Boolean =
    logLines.exists(_.contains(expected))

  def show: String =
    logLines.mkString("  ", "\n  ", "")
}
