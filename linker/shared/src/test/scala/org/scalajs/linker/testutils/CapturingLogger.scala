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

final class CapturingLogger extends Logger {
  import CapturingLogger._

  val lines = mutable.ListBuffer.empty[LogLine]

  def log(level: Level, message: => String): Unit =
    lines += new LogLine(level, message)

  def trace(t: => Throwable): Unit =
    lines += new LogLine(Level.Error, t.toString())

  def allLogLines: LogLines = new LogLines(lines.toList)
}

object CapturingLogger {
  final class LogLine(val level: Level, val message: String) {
    def contains(messagePart: String): Boolean =
      message.contains(messagePart)

    def contains(level: Level, messagePart: String): Boolean =
      this.level == level && contains(messagePart)

    override def toString(): String =
      s"[$level] $message"
  }

  final class LogLines(lines: List[LogLine]) {
    def contains(messagePart: String): Boolean =
      lines.exists(_.contains(messagePart))

    def contains(level: Level, messagePart: String): Boolean =
      lines.exists(_.contains(level, messagePart))

    def assertContains(messagePart: String): Unit = {
      assertTrue(
          s"expected a log line containing '$messagePart', but got \n${this}",
          contains(messagePart))
    }

    def assertNotContains(messagePart: String): Unit = {
      assertFalse(
          s"did not expect a log line containing '$messagePart', but got \n${this}",
          contains(messagePart))
    }

    def assertContains(level: Level, messagePart: String): Unit = {
      assertTrue(
          s"expected a [$level] line containing '$messagePart', but got \n${this}",
          contains(level, messagePart))
    }

    def assertNotContains(level: Level, messagePart: String): Unit = {
      assertFalse(
          s"did not expect a [$level] line containing '$messagePart', but got \n${this}",
          contains(level, messagePart))
    }

    def assertContainsError(messagePart: String): Unit =
      assertContains(Level.Error, messagePart)

    override def toString(): String =
      lines.mkString("  ", "\n  ", "")
  }
}
