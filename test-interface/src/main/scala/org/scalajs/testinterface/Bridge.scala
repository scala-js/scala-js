package org.scalajs.testinterface

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSName}

import org.scalajs.testcommon._

private[testinterface] object Bridge {
  // Called via org.scalajs.testadapter.testAdapterInitializer
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
