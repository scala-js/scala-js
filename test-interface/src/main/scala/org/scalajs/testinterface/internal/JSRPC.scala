package org.scalajs.testinterface.internal

import scala.scalajs.js
import scala.scalajs.js.annotation._

import scala.concurrent.duration._

import org.scalajs.testcommon.RPCCore

/** JS RPC Core. Uses `scalajsCom`. */
private[internal] final object JSRPC extends RPCCore {
  Com.init(handleMessage _)

  override protected def send(msg: String): Unit = Com.send(msg)

  override def close(): Unit = {
    super.close()
    Com.close()
  }

  @js.native
  @JSGlobal("scalajsCom")
  private object Com extends js.Object {
    def init(onReceive: js.Function1[String, Unit]): Unit = js.native
    def send(msg: String): Unit = js.native
    def close(): Unit = js.native
  }
}
