/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.io

import java.io._
import java.net.URI

/** A virtual binary input file. */
trait VirtualBinaryFile {
  def path: String

  /** Returns a new InputStream of the file. */
  def inputStream: InputStream

  override def toString(): String = {
    val className = getClass.getName
    val shortClassName = className.substring(className.lastIndexOf('.') + 1)
    shortClassName + "(" + path + ")"
  }
}

/** A writable virtual binary file.
 *
 *  @note This does **not** extend [[VirtualBinaryFile]], mainly because the
 *      Scala.js stack never needs these two things together.
 *      Implementations are free to provide both interfaces.
 */
trait WritableVirtualBinaryFile {
  def outputStream: OutputStream
}
