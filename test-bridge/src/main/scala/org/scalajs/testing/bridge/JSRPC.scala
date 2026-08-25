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

package org.scalajs.testing.bridge

import scala.scalajs.js
import scala.scalajs.js.annotation._

/* Use the queue execution context (based on JS promises) explicitly:
 * We do not have anything better at our disposal and it is accceptable in
 * terms of fairness: JSRPC only handles in-between test communcation, so any
 * future chain will "yield" to I/O (waiting for a message) or an RPC handler in
 * a finite number of steps.
 */
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import scala.concurrent.duration._

import org.scalajs.testing.common.RPCCore

/** JS RPC Core. Uses `scalajsCom`. */
private[bridge] final object JSRPC extends RPCCore {
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
