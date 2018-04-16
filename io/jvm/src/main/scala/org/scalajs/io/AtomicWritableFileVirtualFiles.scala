package org.scalajs.io

import java.io._
import java.net.URI

trait AtomicWritableFileVirtualBinaryFile extends WritableFileVirtualBinaryFile {
  override def outputStream: OutputStream =
    new BufferedOutputStream(new AtomicFileOutputStream(file))
}

object AtomicWritableFileVirtualBinaryFile {
  def apply(f: File): AtomicWritableFileVirtualBinaryFile =
    new FileVirtualBinaryFile(f) with AtomicWritableFileVirtualBinaryFile
}
