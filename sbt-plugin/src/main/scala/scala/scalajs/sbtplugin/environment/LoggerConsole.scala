package scala.scalajs.sbtplugin.environment

import sbt.Logger

/** A proxy for a Logger that looks like a Mozilla console object */
class LoggerConsole(logger: Logger) extends Console {
  def log(msg: Any): Unit = logger.info(msg.toString)
  def info(msg: Any): Unit = logger.info(msg.toString)
  def warn(msg: Any): Unit = logger.warn(msg.toString)
  def error(msg: Any): Unit = logger.error(msg.toString)
}
