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
import scala.scalajs.js.annotation.{JSGlobalScope, JSName}
import scala.scalajs.LinkingInfo.{linkTimeIf, moduleKind}
import scala.scalajs.LinkingInfo.ModuleKind.MinimalWasmModule

import org.scalajs.testing.common._

private[bridge] object Bridge {
  // Called via org.scalajs.testing.adapter.testAdapterInitializer
  def start(): Unit = mode match {
    case TestBridgeMode.FullBridge =>
      TestAdapterBridge.start()

    case TestBridgeMode.HTMLRunner(tests) =>
      linkTimeIf(moduleKind == MinimalWasmModule) {
        throw new AssertionError("The HTML runner is not supported in Wasm-without-JS.")
      } {
        HTMLRunner.start(tests)
      }
  }

  private def mode: TestBridgeMode = {
    linkTimeIf[TestBridgeMode](moduleKind == MinimalWasmModule) {
      TestBridgeMode.FullBridge
    } {
      if (js.typeOf(js.Dynamic.global.__ScalaJSTestBridgeMode) == "undefined") {
        TestBridgeMode.FullBridge
      } else {
        val modeStr =
          js.Dynamic.global.__ScalaJSTestBridgeMode.asInstanceOf[String]
        Serializer.deserialize[TestBridgeMode](modeStr)
      }
    }
  }
}
