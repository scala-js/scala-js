package org.scalajs.io

import java.io._

final class AtomicWritableFileVirtualBinaryFile(f: File)
    extends FileVirtualFile(f) with WritableVirtualBinaryFile {
  override def outputStream: OutputStream =
    new BufferedOutputStream(new AtomicFileOutputStream(file))
}
