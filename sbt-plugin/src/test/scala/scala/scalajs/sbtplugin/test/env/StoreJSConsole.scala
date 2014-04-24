package scala.scalajs.sbtplugin.test.env

import scala.scalajs.tools.env._

class StoreJSConsole extends JSConsole {
  private[this] val buf = new StringBuilder()

  def log(msg: Any): Unit = {
    buf.append(msg.toString)
    buf.append('\n')
  }

  def getLog: String = buf.toString
}
