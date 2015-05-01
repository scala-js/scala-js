package org.scalajs.core.tools.io

import java.io._
import java.net.URI

trait AtomicWritableFileVirtualTextFile extends WritableFileVirtualTextFile {
  override def contentWriter: Writer = {
    new BufferedWriter(new OutputStreamWriter(
      new AtomicFileOutputStream(file), "UTF-8"))
  }
}

object AtomicWritableFileVirtualTextFile {
  def apply(f: File): AtomicWritableFileVirtualTextFile =
    new FileVirtualTextFile(f) with AtomicWritableFileVirtualTextFile
}

trait AtomicWritableFileVirtualBinaryFile extends WritableFileVirtualBinaryFile {
  override def outputStream: OutputStream =
    new BufferedOutputStream(new AtomicFileOutputStream(file))
}

object AtomicWritableFileVirtualBinaryFile {
  def apply(f: File): AtomicWritableFileVirtualBinaryFile =
    new FileVirtualBinaryFile(f) with AtomicWritableFileVirtualBinaryFile
}

trait AtomicWritableFileVirtualJSFile extends WritableFileVirtualJSFile
                                         with AtomicWritableFileVirtualTextFile {
  override def sourceMapWriter: Writer = {
    new BufferedWriter(new OutputStreamWriter(
        new AtomicFileOutputStream(sourceMapFile), "UTF-8"))
  }
}

object AtomicWritableFileVirtualJSFile {
  def apply(f: File): AtomicWritableFileVirtualJSFile =
    new FileVirtualJSFile(f) with AtomicWritableFileVirtualJSFile
}
