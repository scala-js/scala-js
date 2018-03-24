package org.scalajs.io

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray._

import java.io._
import java.net.URI

class NodeVirtualFile(override val path: String) extends VirtualFile {
  override def version: Option[String] =
    NodeFS.statSync(path).mtime.map(_.getTime.toString).toOption
}

class NodeVirtualTextFile(p: String) extends NodeVirtualFile(p)
                                        with VirtualTextFile {
  override def content: String = NodeFS.readFileSync(path, NodeSupport.utf8enc)
}

class NodeVirtualBinaryFile(p: String) extends NodeVirtualFile(p)
                                          with VirtualBinaryFile {
  private def buf: ArrayBuffer =
    new Uint8Array(NodeFS.readFileSync(path)).buffer

  override def content: Array[Byte] = new Int8Array(buf).toArray
  override def inputStream: InputStream = new ArrayBufferInputStream(buf)
}

class NodeVirtualJSFile(p: String) extends NodeVirtualTextFile(p)
                                      with VirtualJSFile {

  /** Always returns None. We can't read them on JS anyway */
  override def sourceMap: Option[String] = None
}

private[io] object NodeSupport {
  val utf8enc: NodeFS.Enc = new NodeFS.Enc { val encoding = "UTF-8" }
}

@JSImport("fs", JSImport.Namespace)
@js.native
private[io] object NodeFS extends js.Object {
  trait Enc extends js.Object {
    val encoding: String
  }

  trait Stat extends js.Object {
    val mtime: js.UndefOr[js.Date]
  }

  def readFileSync(path: String): js.Array[Int] = js.native
  def readFileSync(path: String, enc: Enc): String = js.native
  def statSync(path: String): Stat = js.native
}

private[scalajs] class NodeVirtualJarFile(file: String)
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

private object NodeVirtualJarFile {
  @js.native
  @JSImport("jszip", JSImport.Default)
  private class JSZip(data: Uint8Array) extends js.Object {
    def files: js.Dictionary[JSZipEntry] = js.native
  }

  private trait JSZipEntry extends js.Object {
    def asArrayBuffer(): ArrayBuffer
  }
}
