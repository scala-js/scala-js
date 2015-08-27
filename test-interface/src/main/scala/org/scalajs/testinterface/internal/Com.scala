package org.scalajs.testinterface.internal

import scala.scalajs.js
import js.annotation.JSName

@js.native
@JSName("scalajsCom")
object Com extends js.Object {
  def init(onReceive: js.Function1[String, Unit]): Unit = js.native
  def send(msg: String): Unit = js.native
  def close(): Unit = js.native
}
