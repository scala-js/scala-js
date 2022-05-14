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

package org.scalajs.linker.backend.emitter

import org.scalajs.linker.interface.IRFile
import org.scalajs.linker.standard.MemIRFileImpl

object PrivateLibHolder {
  val files: Seq[IRFile] = {
    for ((name, contentBase64) <- PrivateLibData.pathsAndContents) yield {
      new MemIRFileImpl(
          path = "org/scalajs/linker/runtime/" + name,
          version = Some(""), // this indicates that the file never changes
          content = java.util.Base64.getDecoder().decode(contentBase64)
      )
    }
  }
}
