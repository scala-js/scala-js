package org.scalajs.sbtplugin

import org.scalajs.core.tools.logging._
import sbt.{Level => SbtLevel, Logger => SbtLogger}

object Loggers {
  private class SbtLoggerWrapper(underlying: SbtLogger) extends Logger {
    def log(level: Level, message: => String): Unit =
      underlying.log(toolsLevel2sbtLevel(level), message)
    def success(message: => String): Unit =
      underlying.success(message)
    def trace(t: => Throwable): Unit =
      underlying.trace(t)
  }

  def sbtLogger2ToolsLogger(logger: SbtLogger): Logger =
    new SbtLoggerWrapper(logger)

  def sbtLevel2ToolsLevel(level: SbtLevel.Value): Level = level match {
    case SbtLevel.Error => Level.Error
    case SbtLevel.Warn  => Level.Warn
    case SbtLevel.Info  => Level.Info
    case SbtLevel.Debug => Level.Debug
  }

  def toolsLevel2sbtLevel(level: Level): SbtLevel.Value = level match {
    case Level.Error => SbtLevel.Error
    case Level.Warn  => SbtLevel.Warn
    case Level.Info  => SbtLevel.Info
    case Level.Debug => SbtLevel.Debug
  }
}
