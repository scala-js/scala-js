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
import java.net.URI

/** A [[VirtualFile]] implemented by an actual file on the file system. */
class FileVirtualFile(val file: File) extends VirtualFile {
  final def path: String = file.getPath

  final override def version: Option[String] = {
    if (!file.isFile) None
    else Some(file.lastModified.toString)
  }
}

/** A [[VirtualBinaryFile]] implemented by an actual file on the file system. */
class FileVirtualBinaryFile(f: File) extends FileVirtualFile(f)
                                        with VirtualBinaryFile {
  final def inputStream: InputStream =
    new BufferedInputStream(new FileInputStream(file))
}

class WritableFileVirtualBinaryFile(f: File) extends FileVirtualBinaryFile(f)
                                                with WritableVirtualBinaryFile {
  final def outputStream: OutputStream =
    new BufferedOutputStream(new FileOutputStream(file))
}
