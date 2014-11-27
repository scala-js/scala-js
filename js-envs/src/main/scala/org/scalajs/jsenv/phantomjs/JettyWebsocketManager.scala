package org.scalajs.jsenv.phantomjs

import javax.servlet.http.HttpServletRequest

import org.eclipse.jetty.server.Server
import org.eclipse.jetty.server.nio.SelectChannelConnector
import org.eclipse.jetty.websocket.{WebSocket, WebSocketHandler}
import org.eclipse.jetty.util.component.{LifeCycle, AbstractLifeCycle}
import org.eclipse.jetty.util.log

final class JettyWebsocketManager(
    wsListener: WebsocketListener) extends WebsocketManager { thisMgr =>

  private[this] var webSocketConn: WebSocket.Connection = null
  private[this] var closed = false

  // We can just set the logger here, since we are supposed to be protected by
  // the private ClassLoader that loads us reflectively.
  log.Log.setLog(new WSLogger("root"))

  private[this] val connector = new SelectChannelConnector

  connector.setHost("localhost")
  connector.setPort(0)

  private[this] val server = new Server()

  server.addConnector(connector)
  server.setHandler(new WebSocketHandler {
    // Support Hixie 76 for Phantom.js
    getWebSocketFactory().setMinVersion(-1)

    override def doWebSocketConnect(
        request: HttpServletRequest, protocol: String): WebSocket =
      new ComWebSocketListener
  })

  server.addLifeCycleListener(new AbstractLifeCycle.AbstractLifeCycleListener {
    override def lifeCycleStarted(event: LifeCycle): Unit = {
      if (event.isRunning())
        wsListener.onRunning()
    }
  })

  private class ComWebSocketListener extends WebSocket.OnTextMessage {
    override def onOpen(connection: WebSocket.Connection): Unit = {
      thisMgr.synchronized {
        if (isConnected)
          throw new IllegalStateException("Client connected twice")
        webSocketConn = connection
      }
      wsListener.onOpen()
    }

    override def onClose(statusCode: Int, reason: String): Unit = {
      thisMgr.synchronized {
        webSocketConn = null
        closed = true
      }
      wsListener.onClose()
      server.stop()

      if (statusCode != 1000) {
        throw new Exception("Abnormal closing of connection. " +
            s"Code: $statusCode, Reason: $reason")
      }
    }

    override def onMessage(message: String): Unit =
      wsListener.onMessage(message)
  }

  private class WSLogger(fullName: String) extends log.AbstractLogger {
    private[this] var debugEnabled = false

    def debug(msg: String, args: Object*): Unit =
      if (debugEnabled) log("DEBUG", msg, args)

    def debug(msg: String, thrown: Throwable): Unit =
      if (debugEnabled) log("DEBUG", msg, thrown)

    def debug(thrown: Throwable): Unit =
      if (debugEnabled) log("DEBUG", thrown)

    def getName(): String = fullName

    def ignore(ignored: Throwable): Unit = ()

    def info(msg: String, args: Object*): Unit = log("INFO", msg, args)
    def info(msg: String, thrown: Throwable): Unit = log("INFO", msg, thrown)
    def info(thrown: Throwable): Unit = log("INFO", thrown)

    def warn(msg: String, args: Object*): Unit = log("WARN", msg, args)
    def warn(msg: String, thrown: Throwable): Unit = log("WARN", msg, thrown)
    def warn(thrown: Throwable): Unit = log("WARN", thrown)

    def isDebugEnabled(): Boolean = debugEnabled
    def setDebugEnabled(enabled: Boolean): Unit = debugEnabled = enabled

    private def log(lvl: String, msg: String, args: Object*): Unit =
      wsListener.log(s"$lvl: $msg " + args.mkString(", "))

    private def log(lvl: String, msg: String, thrown: Throwable): Unit =
      wsListener.log(s"$lvl: $msg $thrown\n{$thrown.getStackStrace}")

    private def log(lvl: String, thrown: Throwable): Unit =
      wsListener.log(s"$lvl: $thrown\n{$thrown.getStackStrace}")

    protected def newLogger(fullName: String) = new WSLogger(fullName)
  }

  def start(): Unit = server.start()

  def stop(): Unit = server.stop()

  def isConnected: Boolean = webSocketConn != null && !closed
  def isClosed: Boolean = closed

  def localPort: Int = connector.getLocalPort()

  def sendMessage(msg: String) = synchronized {
    if (webSocketConn != null)
      webSocketConn.sendMessage(msg)
  }

}
