package scala.scalajs.sbtplugin.env.phantomjs

private[phantomjs] trait WebsocketListener {
  def onRunning(): Unit
  def onOpen(): Unit
  def onClose(): Unit
  def onMessage(msg: String): Unit

  def log(msg: String): Unit
}
