package org.scalajs.jsenv

object NullJSConsole extends JSConsole {
  def log(msg: Any): Unit = {}
}
