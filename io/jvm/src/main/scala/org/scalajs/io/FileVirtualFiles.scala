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

package org.scalajs.io

import java.io._

/** A [[VirtualBinaryFile]] implemented by an actual file on the file system. */
class FileVirtualBinaryFile(val file: File) extends VirtualBinaryFile {
  final def path: String = file.getPath

  final def inputStream: InputStream =
    new BufferedInputStream(new FileInputStream(file))
}

class WritableFileVirtualBinaryFile(file: File) extends WritableVirtualBinaryFile {
  final def outputStream: OutputStream =
    new BufferedOutputStream(new FileOutputStream(file))
}
