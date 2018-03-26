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

trait WritableNodeVirtualTextFile extends NodeVirtualTextFile
                                     with WritableVirtualTextFile {
  def contentWriter: Writer = new NodeWriter(path)
}

object WritableNodeVirtualTextFile {
  def apply(path: String): WritableNodeVirtualTextFile =
    new NodeVirtualTextFile(path) with WritableNodeVirtualTextFile
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

trait WritableNodeVirtualJSFile extends NodeVirtualJSFile
                                   with WritableVirtualJSFile
                                   with WritableNodeVirtualTextFile {
  def sourceMapWriter: Writer = new NodeWriter(path + ".map")
}

object WritableNodeVirtualJSFile {
  def apply(path: String): WritableNodeVirtualJSFile =
    new NodeVirtualJSFile(path) with WritableNodeVirtualJSFile
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
  def writeFileSync(path: String, data: String, enc: Enc): Unit = js.native
}

private[io] class NodeWriter(path: String) extends StringWriter {
  override def close(): Unit = {
    super.close()
    NodeFS.writeFileSync(path, this.toString, NodeSupport.utf8enc)
  }
}
