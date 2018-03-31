package org.scalajs.io

import java.io._
import java.util.zip.{ZipInputStream, ZipEntry}

/** A virtual jar file. */
trait VirtualJarFile extends VirtualFileContainer with VirtualBinaryFile {
  import VirtualJarFile._

  def listEntries[T](p: String => Boolean)(
      makeResult: (String, InputStream) => T): List[T] = {
    val stream = new ZipInputStream(inputStream)
    try {
      val streamIgnoreClose = new IgnoreCloseFilterInputStream(stream)
      Iterator.continually(stream.getNextEntry())
        .takeWhile(_ != null)
        .filter(entry => p(entry.getName))
        .map(entry => makeResult(entry.getName, streamIgnoreClose))
        .toList
    } finally {
      stream.close()
    }
  }
}

private object VirtualJarFile {
  private final class IgnoreCloseFilterInputStream(in: InputStream)
      extends FilterInputStream(in) {

    override def close(): Unit = ()
  }
}
