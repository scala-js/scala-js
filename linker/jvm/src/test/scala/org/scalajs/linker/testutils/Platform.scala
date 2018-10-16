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

package org.scalajs.linker.testutils

import java.io.File

import org.scalajs.linker.irio._

object Platform {
  def loadJar(path: String): ScalaJSIRContainer =
    new FileVirtualJarScalaJSIRContainer(new File(path))
}
