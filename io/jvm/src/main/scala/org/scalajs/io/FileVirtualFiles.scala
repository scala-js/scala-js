package org.scalajs.io

import java.io._
import java.net.URI

/** A [[VirtualFile]] implemented by an actual file on the file system. */
class FileVirtualFile(val file: File) extends VirtualFile {
  import FileVirtualFile._

  override def path: String = file.getPath

  override def version: Option[String] = {
    if (!file.isFile) None
    else Some(file.lastModified.toString)
  }
}

object FileVirtualFile extends (File => FileVirtualFile) {
  def apply(f: File): FileVirtualFile =
    new FileVirtualFile(f)
}

/** A [[VirtualBinaryFile]] implemented by an actual file on the file system. */
class FileVirtualBinaryFile(f: File) extends FileVirtualFile(f)
                                        with VirtualBinaryFile {
  import FileVirtualBinaryFile._

  override def inputStream: InputStream =
    new BufferedInputStream(new FileInputStream(file))
}

object FileVirtualBinaryFile extends (File => FileVirtualBinaryFile) {
  def apply(f: File): FileVirtualBinaryFile =
    new FileVirtualBinaryFile(f)
}

trait WritableFileVirtualBinaryFile extends FileVirtualBinaryFile
                                       with WritableVirtualBinaryFile {
  override def outputStream: OutputStream =
    new BufferedOutputStream(new FileOutputStream(file))
}

object WritableFileVirtualBinaryFile {
  def apply(f: File): WritableFileVirtualBinaryFile =
    new FileVirtualBinaryFile(f) with WritableFileVirtualBinaryFile
}
