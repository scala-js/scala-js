package scala.scalajs.sbtplugin.environment

import sbt.Logger

trait Console {
  def log(msg: Any): Unit
}
