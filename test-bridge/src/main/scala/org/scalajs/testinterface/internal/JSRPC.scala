/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.testinterface.internal

import scala.scalajs.js
import scala.scalajs.js.annotation._

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import org.scalajs.testcommon.RPCCore

/** JS RPC Core. Uses `scalajsCom`. */
private[internal] final object JSRPC extends RPCCore {
  Com.init(handleMessage _)

  override protected def send(msg: String): Unit = Com.send(msg)

  @js.native
  @JSGlobal("scalajsCom")
  private object Com extends js.Object {
    def init(onReceive: js.Function1[String, Unit]): Unit = js.native
    def send(msg: String): Unit = js.native
    // We support close, but do not use it. The JS side just terminates.
    // def close(): Unit = js.native
  }
}
