package org.scalajs.testsuite.utils

import sbt.testing.Logger

import scala.scalajs.js

object JSConsoleLogger extends Logger {
  import js.Dynamic.global

  def warn(msg: String): Unit = {
    if (js.isUndefined(global.console.warn))
      println("Warn: " + msg)
    else
      global.console.warn(msg)
  }

  def error(msg: String): Unit = {
    if (js.isUndefined(global.console.error))
      println("Error: " + msg)
    else
      global.console.error(msg)
  }

  def ansiCodesSupported(): Boolean = false

  def debug(msg: String): Unit = {
    if (js.isUndefined(global.console.debug))
      println("Debug: " + msg)
    else
      global.console.debug(msg)
  }

  def trace(t: Throwable): Unit = t.printStackTrace()

  def info(msg: String): Unit = {
    if (js.isUndefined(global.console.info))
      println("Info: " + msg)
    else
      global.console.info(msg)
  }
}
