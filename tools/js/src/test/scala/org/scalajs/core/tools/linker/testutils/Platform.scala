package org.scalajs.core.tools.linker.testutils

import org.scalajs.core.tools.io._

object Platform {
  def loadJar(path: String): ScalaJSIRContainer =
    new NodeVirtualJarFile(path)
}
