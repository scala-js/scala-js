package org.scalajs.testinterface.internal

import scala.scalajs.js.annotation._

import java.io._

import org.scalajs.testcommon._

@JSExportTopLevel("org.scalajs.testinterface.internal.InfoSender")
final class InfoSender(frameworkName: String) {
  @JSExport
  def initAndSend(): Unit = {
    Com.init((_: String) => ())
    sendFrameworkInfo()
    Com.close()
  }

  private def sendFrameworkInfo(): Unit = {
    val framework = FrameworkLoader.loadFramework(frameworkName)
    val info = new FrameworkInfo(framework.name, framework.fingerprints.toList)
    Com.send(Serializer.serialize(info))
  }
}
