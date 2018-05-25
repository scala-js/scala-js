package org.scalajs.io

import java.io._
import java.net.URI

/** A virtual input file.
 */
trait VirtualFile {
  /** Path of the file, including everything.
   *  Unique if possible (used for lookup). */
  def path: String

  /** Optionally returns an implementation-dependent "version" token.
   *  Versions are compared with ==.
   *  If non-empty, a different version must be returned when the content
   *  changes. It should be equal if the content has not changed, but it is
   *  not mandatory.
   *  Such a token can be used by caches: the file need not be read and
   *  processed again if its version has not changed.
   */
  def version: Option[String] = None

  override def toString(): String = {
    val className = getClass.getName
    val shortClassName = className.substring(className.lastIndexOf('.') + 1)
    shortClassName + "(" + path + ")"
  }
}

/** A virtual binary input file.
 */
trait VirtualBinaryFile extends VirtualFile {
  /** Returns a new InputStream of the file. */
  def inputStream: InputStream
}

/** A writable virtual binary file.
 *
 *  @note This does **not** extend [[VirtualBinaryFile]] nor [[VirtualFile]],
 *      mainly because the Scala.js stack never needs these two things together.
 *      Implementations are free to provide both interfaces.
 */
trait WritableVirtualBinaryFile {
  def outputStream: OutputStream
}
