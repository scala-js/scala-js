package org.scalajs.io

import scala.annotation.tailrec

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

trait WritableNodeVirtualBinaryFile extends NodeVirtualBinaryFile
                                       with WritableVirtualBinaryFile {
  def outputStream: OutputStream = new NodeOutputStream(path)
}

object WritableNodeVirtualBinaryFile {
  def apply(path: String): WritableNodeVirtualBinaryFile =
    new NodeVirtualBinaryFile(path) with WritableNodeVirtualBinaryFile
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
private[scalajs] object NodeFS extends js.Object {
  trait Enc extends js.Object {
    val encoding: String
  }

  trait Stat extends js.Object {
    val mtime: js.UndefOr[js.Date]
  }

  def readFileSync(path: String): js.Array[Int] = js.native
  def readFileSync(path: String, enc: Enc): String = js.native
  def statSync(path: String): Stat = js.native

  def openSync(path: String, flags: String): Int = js.native
  def writeSync(fd: Int, buffer: Uint8Array): Int = js.native
  def closeSync(fd: Int): Unit = js.native
}

private[io] final class NodeOutputStream(path: String) extends OutputStream {
  private[this] val bufsize = 4096
  private[this] val fd = NodeFS.openSync(path, "w")
  private[this] val arrBuf = new ArrayBuffer(bufsize)
  private[this] val buf = TypedArrayBuffer.wrap(arrBuf)

  @tailrec
  override def write(b: Array[Byte], off: Int, len: Int): Unit = {
    ensureSpace()

    val ilen = Math.min(len, buf.remaining())
    buf.put(b, off, ilen)

    if (len > ilen)
      write(b, off + ilen, ilen - len)
  }

  def write(b: Int): Unit = {
    ensureSpace()
    buf.put(b.toByte)
  }

  override def flush(): Unit = performWrite(0)

  private def ensureSpace(): Unit = {
    if (!buf.hasRemaining())
      performWrite(bufsize / 4)
  }

  private def performWrite(limit: Int): Unit = {
    buf.flip()
    while (buf.remaining() > limit) {
      val pos = buf.position()
      val written = NodeFS.writeSync(fd, new Uint8Array(arrBuf, pos, buf.limit() - pos))
      buf.position(pos + written)
    }
    buf.compact()
  }

  override def close(): Unit = {
    flush()
    NodeFS.closeSync(fd)
  }
}
