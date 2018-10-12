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

import sbt.Logger
import org.scalajs.jsenv.JSConsole

/** A proxy for a Logger that looks like a Mozilla console object */
@deprecated(
    "Use org.scalajs.jsenv.ConsoleJSConsole or define your own instead.",
    "0.6.10")
class LoggerJSConsole(logger: Logger) extends JSConsole {
  def log(msg: Any): Unit = logger.info(msg.toString)
}
