package scala.scalajs.sbtplugin.testing

import scala.scalajs.tools.logging._
import sbt.testing.{ Logger => SbtTestLogger }

class SbtTestLoggerAccWrapper(logger: Seq[SbtTestLogger]) extends Logger {

  import scala.scalajs.sbtplugin.Implicits._
  import Level._

  def log(level: Level, message: => String): Unit = level match {
    case Error => logger.foreach(_.error(message))
    case Warn  => logger.foreach(_.warn(message))
    case Info  => logger.foreach(_.info(message))
    case Debug => logger.foreach(_.debug(message))
  }

  def success(message: => String): Unit = logger.foreach(_.info(message))

  def trace(t: => Throwable): Unit = logger.foreach(_.trace(t))

}
