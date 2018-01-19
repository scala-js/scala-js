package org.scalajs.linker.testutils

import org.scalajs.linker.irio._

object Platform {
  def loadJar(path: String): ScalaJSIRContainer =
    new NodeVirtualJarScalaJSIRContainer(path)
}
