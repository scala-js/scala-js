package org.scalajs.testinterface.internal

import scala.scalajs.js
import js.Dynamic.{literal => lit}
import js.JSConverters._
import js.annotation.JSExport

@JSExport
final class InfoSender(frameworkName: String) {

  @JSExport
  def initAndSend(): Unit = {
    Com.init((_: String) => ())
    sendFrameworkInfo()
    Com.close()
  }

  private def sendFrameworkInfo(): Unit = {
    val framework = FrameworkLoader.loadFramework(frameworkName)
    val fingerprints =
      framework.fingerprints.map(FingerprintSerializer.serialize).toJSArray
    val data = lit(
        name = framework.name,
        fingerprints = fingerprints)
    Com.send(js.JSON.stringify(data))
  }

}
