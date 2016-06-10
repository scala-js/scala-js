/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


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
