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
import scala.scalajs.LinkingInfo._
import scala.scalajs.LinkingInfo.ModuleKind.MinimalWasmModule
import scala.scalajs.wasm.annotation._

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

import org.scalajs.testing.common.RPCCore

/** JS RPC Core. Uses `scalajsCom`. */
private[bridge] final object JSRPC extends RPCCore {
  linkTimeIf(moduleKind == MinimalWasmModule) {
    ()
  } {
    /* Use the queue execution context (based on JS promises) explicitly:
     * We do not have anything better at our disposal and it is accceptable in
     * terms of fairness: JSRPC only handles in-between test communication, so any
     * future chain will "yield" to I/O (waiting for a message) or an RPC handler in
     * a finite number of steps.
     */
    implicit val ec = scala.scalajs.concurrent.JSExecutionContext.queue
    Com.init(handleMessage _)
  }

  override protected def send(msg: String): Unit = {
    linkTimeIf(moduleKind == MinimalWasmModule) {
      val codeUnits = new Array[Short](msg.length)
      var i = 0
      while (i != codeUnits.length) {
        codeUnits(i) = msg.charAt(i).toShort
        i += 1
      }
      WasmCom.send(codeUnits)
    } {
      Com.send(msg)
    }
  }

  @WasmExport("scalajs:testing/com/receive")
  def receive(msg: Array[Short]): Unit = {
    // TODO Why is this function even *linked* when we're not in MinimalWasmModule?
    linkTimeIf(moduleKind == MinimalWasmModule) {
      val chars = new Array[Char](msg.length)
      var i = 0
      while (i != chars.length) {
        chars(i) = msg(i).toChar
        i += 1
      }

      implicit val ec = ComLoopExecutionContext
      handleMessage(new String(chars))
      ec.runLoop()
    } {
      throw new AssertionError("receive should only be called for MinimalWasmModule")
    }
  }

  @js.native
  @JSGlobal("scalajsCom")
  private object Com extends js.Object {
    def init(onReceive: js.Function1[String, Unit]): Unit = js.native
    def send(msg: String): Unit = js.native
    // We support close, but do not use it. The JS side just terminates.
    // def close(): Unit = js.native
  }

  private object WasmCom {
    @WasmImport("scalajs:testing/com", "send")
    def send(msg: Array[Short]): Unit = scala.scalajs.wasm.native

    @WasmImport("scalajs:core", "doWriteLine")
    def doWriteLine(isErr: scala.Boolean, line: Array[scala.Byte]): Unit =
      scala.scalajs.wasm.native
  }

  private object ComLoopExecutionContext extends ExecutionContext {
    private val tasks = mutable.ListBuffer.empty[Runnable]
    private var inLoop: Boolean = false

    def execute(runnable: Runnable): Unit =
      tasks += runnable

    def runLoop(): Unit = {
      if (inLoop) {
        // Reentrency into a loop; don't start a new one
      } else {
        inLoop = true
        try {
          while (tasks.nonEmpty) {
            val task = tasks.remove(0)
            try {
              task.run()
            } catch {
              case t: Throwable => reportFailure(t)
            }
          }
        } finally {
          inLoop = false
        }
      }
    }

    def reportFailure(t: Throwable): Unit =
      WasmCom.doWriteLine(true, t.toString().getBytes())
  }
}
