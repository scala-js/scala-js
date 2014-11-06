package scala.scalajs.sbtplugin.env.phantomjs

private[phantomjs] trait WebsocketManager {
  def start(): Unit
  def stop(): Unit
  def sendMessage(msg: String): Unit
  def localPort: Int
  def isConnected: Boolean
  def isClosed: Boolean
}
