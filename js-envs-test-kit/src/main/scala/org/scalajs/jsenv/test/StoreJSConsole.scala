package org.scalajs.jsenv.test

import org.scalajs.jsenv._

class StoreJSConsole extends JSConsole {
  private[this] val buf = new StringBuilder()

  def log(msg: Any): Unit = {
    buf.append(msg.toString)
    buf.append('\n')
  }

  def getLog: String = buf.toString
}
