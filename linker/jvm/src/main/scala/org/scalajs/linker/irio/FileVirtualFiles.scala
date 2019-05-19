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

package org.scalajs.linker.irio

import java.io._
import java.nio.file._

class WritableFileVirtualBinaryFile(file: Path) extends WritableVirtualBinaryFile {
  final def outputStream: OutputStream = {
    import StandardOpenOption._

    new BufferedOutputStream(Files.newOutputStream(file, WRITE, CREATE, TRUNCATE_EXISTING))
  }
}
