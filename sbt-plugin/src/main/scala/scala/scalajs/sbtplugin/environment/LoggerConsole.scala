/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.sbtplugin.environment

import sbt.Logger

/** A proxy for a Logger that looks like a Mozilla console object */
class LoggerConsole(logger: Logger) extends Console {
  def log(msg: Any): Unit = logger.info(msg.toString)
  def info(msg: Any): Unit = logger.info(msg.toString)
  def warn(msg: Any): Unit = logger.warn(msg.toString)
  def error(msg: Any): Unit = logger.error(msg.toString)
}
