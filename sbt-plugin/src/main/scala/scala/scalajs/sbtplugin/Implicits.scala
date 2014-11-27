package org.scalajs.sbtplugin

import scala.language.implicitConversions

import org.scalajs.core.tools.logging._
import sbt.{Logger => SbtLogger, Level => SbtLevel}

object Implicits {
  private class SbtLoggerWrapper(underlying: SbtLogger) extends Logger {
    def log(level: Level, message: => String): Unit =
      underlying.log(level, message)
    def success(message: => String): Unit =
      underlying.success(message)
    def trace(t: => Throwable): Unit =
      underlying.trace(t)
  }

  implicit def sbtLogger2ToolsLogger(logger: SbtLogger): Logger =
    new SbtLoggerWrapper(logger)

  implicit def sbtLevel2ToolsLevel(level: SbtLevel.Value): Level = level match {
    case SbtLevel.Error => Level.Error
    case SbtLevel.Warn  => Level.Warn
    case SbtLevel.Info  => Level.Info
    case SbtLevel.Debug => Level.Debug
  }

  implicit def toolsLevel2sbtLevel(level: Level): SbtLevel.Value = level match {
    case Level.Error => SbtLevel.Error
    case Level.Warn  => SbtLevel.Warn
    case Level.Info  => SbtLevel.Info
    case Level.Debug => SbtLevel.Debug
  }
}
