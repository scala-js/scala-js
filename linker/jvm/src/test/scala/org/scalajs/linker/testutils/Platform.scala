package org.scalajs.linker.testutils

import java.io.File

import org.scalajs.linker.irio._

object Platform {
  def loadJar(path: String): ScalaJSIRContainer =
    new FileVirtualJarScalaJSIRContainer(new File(path))
}
