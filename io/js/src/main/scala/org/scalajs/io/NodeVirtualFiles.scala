package org.scalajs.io

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray._

import java.io._
import java.net.URI

class NodeVirtualFile(override val path: String) extends VirtualFile {
  override def version: Option[String] =
    NodeFS.statSync(path).mtime.map(_.getTime.toString).toOption

  override def exists: Boolean = NodeFS.existsSync(path)

  override def toURI: URI = {
    val abspath = NodeFS.realpathSync(path)
    new URI("file", abspath, null)
  }
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

  def existsSync(path: String): Boolean = js.native
  def readFileSync(path: String): js.Array[Int] = js.native
  def readFileSync(path: String, enc: Enc): String = js.native
  def realpathSync(path: String): String = js.native
  def statSync(path: String): Stat = js.native
}
