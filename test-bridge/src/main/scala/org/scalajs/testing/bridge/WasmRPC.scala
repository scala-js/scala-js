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

import scala.scalajs.wasm.minimal.annotation._

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

import org.scalajs.testing.common.RPCCore

/** Wasm RPC Core. Uses the `scalajs:testing/com/` Wasm API. */
private[bridge] object WasmRPC extends RPCCore {
  override protected def send(msg: String): Unit =
    WasmCom.send(stringToUTF16CodeUnits(msg))

  @WasmExport("scalajs:testing/com/receive")
  def receive(msg: Array[Short]): Unit = {
    implicit val ec = ComLoopExecutionContext
    handleMessage(utf16CodeUnitsToString(msg))
    ec.runLoop()
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
      WasmCom.reportTopLevelError(stringToUTF16CodeUnits(t.toString()))
  }

  private def stringToUTF16CodeUnits(s: String): Array[Short] = {
    val len = s.length()
    val codeUnits = new Array[Short](len)
    var i = 0
    while (i != len) {
      codeUnits(i) = s.charAt(i).toShort
      i += 1
    }
    codeUnits
  }

  private def utf16CodeUnitsToString(codeUnits: Array[Short]): String = {
    var result = ""
    val len = codeUnits.length
    var i = 0
    while (i != len) {
      result += codeUnits(i).toChar
      i += 1
    }
    result
  }

  private object WasmCom {
    @WasmImport("scalajs:testing/com", "send")
    def send(msg: Array[Short]): Unit = scala.scalajs.wasm.native

    @WasmImport("scalajs:testing/com", "reportTopLevelError")
    def reportTopLevelError(message: Array[Short]): Unit = scala.scalajs.wasm.native
  }
}
