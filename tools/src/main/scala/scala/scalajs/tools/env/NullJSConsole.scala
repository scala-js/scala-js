package scala.scalajs.tools.env

object NullJSConsole extends JSConsole {
  def log(msg: Any): Unit = {}
}
