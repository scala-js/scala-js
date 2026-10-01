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

import scala.scalajs.wasm.annotation._

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

import org.scalajs.testing.common.RPCCore

/** Wasm RPC Core. Uses the `scalajs:com` Wasm API. */
private[bridge] object WasmRPC extends RPCCore {
  override protected def send(msg: String): Unit =
    WasmCom.send(stringToUTF16CodeUnits(msg))

  @WasmExport("scalajs:com/receive")
  def receive(msg: Array[Short]): Unit =
    handleMessage(utf16CodeUnitsToString(msg))

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
    @WasmImport("scalajs:com", "send")
    def send(msg: Array[Short]): Unit = scala.scalajs.wasm.native
  }
}
