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

package org.scalajs.sbtplugin

import scala.language.implicitConversions

import org.scalajs.core.tools.logging._
import sbt.{Logger => SbtLogger, Level => SbtLevel}

@deprecated("Use sbtplugin.Loggers explicitly instead.", "0.6.10")
object Implicits {

  implicit def sbtLogger2ToolsLogger(logger: SbtLogger): Logger =
    Loggers.sbtLogger2ToolsLogger(logger)

  implicit def sbtLevel2ToolsLevel(level: SbtLevel.Value): Level =
    Loggers.sbtLevel2ToolsLevel(level)

  implicit def toolsLevel2sbtLevel(level: Level): SbtLevel.Value =
    Loggers.toolsLevel2sbtLevel(level)
}
