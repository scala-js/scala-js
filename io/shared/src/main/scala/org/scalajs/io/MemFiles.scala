/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.io

import java.io._

import java.nio.charset.StandardCharsets

/** A base class for simple in-memory mutable virtual files. */
class MemVirtualFile(val path: String) extends VirtualFile {
  private[this] var _version: Option[String] = None

  override def version: Option[String] = _version
  def version_=(v: Option[String]): Unit = _version = v

  final def withVersion(v: Option[String]): this.type = {
    version = v
    this
  }
}

/** A simple in-memory mutable virtual binary file. */
class MemVirtualBinaryFile(p: String) extends MemVirtualFile(p)
                                         with VirtualBinaryFile {
  private[this] var _content: Array[Byte] = new Array[Byte](0)

  def content: Array[Byte] = _content
  def content_=(v: Array[Byte]): Unit = _content = v

  def inputStream: InputStream = new ByteArrayInputStream(content)

  final def withContent(v: Array[Byte]): this.type = {
    content = v
    this
  }

  final def withStringUTF8(v: String): this.type =
    withContent(v.getBytes(StandardCharsets.UTF_8))
}

trait WritableMemVirtualBinaryFile extends MemVirtualBinaryFile
                                      with WritableVirtualBinaryFile {
  def outputStream: OutputStream = new ByteArrayOutputStream {
    override def close(): Unit = {
      super.close()
      WritableMemVirtualBinaryFile.this.content = this.toByteArray
    }
  }
}

object WritableMemVirtualBinaryFile {
  def apply(path: String): WritableMemVirtualBinaryFile =
    new MemVirtualBinaryFile(path) with WritableMemVirtualBinaryFile
}
