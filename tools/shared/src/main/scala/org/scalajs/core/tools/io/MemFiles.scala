/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.io

import java.io._

/** A base class for simple in-memory mutable virtual files. */
class MemVirtualFile(val path: String) extends VirtualFile {
  private[this] var _version: Option[String] = None

  override def version: Option[String] = _version
  def version_=(v: Option[String]): Unit = _version = v

  final def withVersion(v: Option[String]): this.type = {
    version = v
    this
  }

  override def exists: Boolean = true
}

/** A simple in-memory mutable virtual text file. */
class MemVirtualTextFile(p: String) extends MemVirtualFile(p)
                                       with VirtualTextFile {
  private[this] var _content: String = ""

  override def content: String = _content
  def content_=(v: String): Unit = _content = v

  final def withContent(v: String): this.type = {
    content = v
    this
  }
}

trait WritableMemVirtualTextFile extends MemVirtualTextFile
                                    with WritableVirtualTextFile {
  def contentWriter: Writer = new StringWriter {
    override def close(): Unit = {
      super.close()
      WritableMemVirtualTextFile.this.content = this.toString
    }
  }
}

object WritableMemVirtualTextFile {
  def apply(path: String): WritableMemVirtualTextFile =
    new MemVirtualTextFile(path) with WritableMemVirtualTextFile
}

/** A simple in-memory mutable virtual binary file. */
class MemVirtualBinaryFile(p: String) extends MemVirtualFile(p)
                                         with VirtualBinaryFile {
  private[this] var _content: Array[Byte] = new Array[Byte](0)

  override def content: Array[Byte] = _content
  def content_=(v: Array[Byte]): Unit = _content = v

  final def withContent(v: Array[Byte]): this.type = {
    content = v
    this
  }
}

/** A simple in-memory mutable virtual JS file. */
class MemVirtualJSFile(p: String) extends MemVirtualTextFile(p)
                                     with VirtualJSFile {
  private[this] var _sourceMap: Option[String] = None

  override def sourceMap: Option[String] = _sourceMap
  def sourceMap_=(v: Option[String]): Unit = _sourceMap = v

  final def withSourceMap(v: Option[String]): this.type = {
    sourceMap = v
    this
  }
}

trait WritableMemVirtualJSFile extends MemVirtualJSFile
                                  with WritableVirtualJSFile
                                  with WritableMemVirtualTextFile {

  def sourceMapWriter: Writer = new StringWriter {
    override def close(): Unit = {
      super.close()
      WritableMemVirtualJSFile.this.sourceMap = Some(this.toString)
    }
  }
}

object WritableMemVirtualJSFile {
  def apply(path: String): WritableMemVirtualJSFile =
    new MemVirtualJSFile(path) with WritableMemVirtualJSFile
}

/** A simple in-memory mutable virtual serialized Scala.js IR file. */
class MemVirtualSerializedScalaJSIRFile(p: String) extends MemVirtualBinaryFile(p)
                                                      with VirtualSerializedScalaJSIRFile
