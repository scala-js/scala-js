package org.scalajs.core.tools.linker.testutils

import org.scalajs.core.tools.linker.irio._

object Platform {
  def loadJar(path: String): ScalaJSIRContainer =
    new NodeVirtualJarScalaJSIRContainer(path)
}
