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

/** A simple in-memory immutable virtual binary file.
 *
 *  Modifying the passed `content` after creation of a [[MemVirtualBinaryFile]]
 *  yields undefined behavior.
 */
class MemVirtualBinaryFile(
    final val path: String,
    content: Array[Byte],
    final override val version: Option[String]
) extends VirtualBinaryFile {
  final def inputStream: InputStream = new ByteArrayInputStream(content)
}

object MemVirtualBinaryFile {
  def apply(path: String, content: Array[Byte]): MemVirtualBinaryFile =
    apply(path, content, None)

  def apply(path: String, content: Array[Byte],
      version: Option[String]): MemVirtualBinaryFile = {
    new MemVirtualBinaryFile(path, content, version)
  }

  def fromStringUTF8(path: String, content: String): MemVirtualBinaryFile =
    fromStringUTF8(path, content, None)

  def fromStringUTF8(path: String, content: String,
      version: Option[String]): MemVirtualBinaryFile = {
    apply(path, content.getBytes(StandardCharsets.UTF_8), version)
  }
}

final class WritableMemVirtualBinaryFile extends WritableVirtualBinaryFile {
  private var _content: Array[Byte] = _

  def content: Array[Byte] = _content

  def outputStream: OutputStream = new ByteArrayOutputStream {
    override def close(): Unit = {
      super.close()
      _content = this.toByteArray
    }
  }

  def toReadable(path: String): MemVirtualBinaryFile = toReadable(path, None)

  def toReadable(path: String, version: Option[String]): MemVirtualBinaryFile =
    new MemVirtualBinaryFile(path, content, version)
}
