package org.scalajs.core.tools.io

import scala.scalajs.js
import scala.scalajs.js.typedarray._

import java.io._
import java.net.URI

class NodeVirtualFile(override val path: String) extends VirtualFile {
  import NodeFS.fs

  override def version: Option[String] = {
    val stat = fs.statSync(path)
    if (js.isUndefined(stat.mtime))
      None
    else
      Some(stat.mtime.asInstanceOf[js.Date].getTime.toString)
  }

  override def exists: Boolean =
    fs.existsSync(path).asInstanceOf[Boolean]

  override def toURI: URI = {
    val abspath = fs.realpathSync(path).asInstanceOf[String]
    new URI("file", abspath, null)
  }
}

class NodeVirtualTextFile(p: String) extends NodeVirtualFile(p)
                                        with VirtualTextFile {
  import NodeFS.fs

  override def content: String = {
    val options = js.Dynamic.literal(encoding = "UTF-8")
    fs.readFileSync(path, options).asInstanceOf[String]
  }
}

class NodeVirtualBinaryFile(p: String) extends NodeVirtualFile(p)
                                          with VirtualBinaryFile {
  import NodeFS.fs

  private def buf: ArrayBuffer =
    new Uint8Array(fs.readFileSync(path).asInstanceOf[js.Array[Int]]).buffer

  override def content: Array[Byte] = new Int8Array(buf).toArray
  override def inputStream: InputStream = new ArrayBufferInputStream(buf)
}

class NodeVirtualJSFile(p: String) extends NodeVirtualTextFile(p)
                                      with VirtualJSFile {

  /** Always returns None. We can't read them on JS anyway */
  override def sourceMap: Option[String] = None
}

class NodeVirtualScalaJSIRFile(p: String)
    extends NodeVirtualBinaryFile(p) with VirtualSerializedScalaJSIRFile

private[io] object NodeFS {
  val fs = js.Dynamic.global.require("fs")
}
