package org.scalajs.linker.testutils

import java.io.InputStream

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.typedarray._

import org.scalajs.io._

import org.scalajs.linker.irio._

class NodeVirtualJarFile(file: String)
    extends NodeVirtualBinaryFile(file) with VirtualFileContainer {

  import NodeVirtualJarFile._

  def listEntries[T](p: String => Boolean)(
      makeResult: (String, InputStream) => T): List[T] = {
    import js.Dynamic.{global => g}

    val stream = inputStream
    try {
      /* Build a Uint8Array with the content of this jar file.
       * We know that in practice, NodeVirtualBinaryFile#inputStream returns
       * an ArrayBufferInputStream, so we just fetch its internal ArrayBuffer
       * rather than copying.
       *
       * Since we have NodeVirtualBinaryFile under our control, in the same
       * repository, we can make this assumption. Should we change
       * NodeVirtualBinaryFile, this test will immediately fail, and we can
       * adapt it.
       */
      val data = stream match {
        case stream: ArrayBufferInputStream =>
          // Simulate reading all the data
          while (stream.skip(stream.available()) > 0) {}
          new Uint8Array(stream.buffer, stream.offset, stream.length)
        case _ =>
          throw new AssertionError(
              s"Uh! '$file' was not read as an ArrayBufferInputStream")
      }

      val zip = new JSZip(data)

      for ((name, entry) <- zip.files.toList if p(name)) yield {
        val entryStream = new ArrayBufferInputStream(entry.asArrayBuffer())
        try {
          makeResult(name, entryStream)
        } finally {
          entryStream.close()
        }
      }
    } finally {
      stream.close()
    }
  }
}

object NodeVirtualJarFile {
  @js.native
  @JSImport("jszip", JSImport.Default)
  private class JSZip(data: Uint8Array) extends js.Object {
    def files: js.Dictionary[JSZipEntry] = js.native
  }

  private trait JSZipEntry extends js.Object {
    def asArrayBuffer(): ArrayBuffer
  }
}

class NodeVirtualJarScalaJSIRContainer(file: String)
    extends NodeVirtualJarFile(file) with ScalaJSIRContainer {

  def sjsirFiles: List[VirtualScalaJSIRFile] =
    ScalaJSIRContainer.sjsirFilesIn(this)
}
