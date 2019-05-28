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

import org.scalajs.testing.common._

private[bridge] object Bridge {
  // Called via org.scalajs.testing.adapter.testAdapterInitializer
  def start(): Unit = mode match {
    case TestInterfaceMode.FullBridge        => TestAdapterBridge.start()
    case TestInterfaceMode.HTMLRunner(tests) => HTMLRunner.start(tests)
  }

  private def mode = {
    if (js.typeOf(js.Dynamic.global.__ScalaJSTestInterfaceMode) == "undefined") {
      TestInterfaceMode.FullBridge
    } else {
      val modeStr =
        js.Dynamic.global.__ScalaJSTestInterfaceMode.asInstanceOf[String]
      Serializer.deserialize[TestInterfaceMode](modeStr)
    }
  }
}
