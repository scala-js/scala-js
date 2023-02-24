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

package org.scalajs.linker.backend

import java.io.OutputStream
import java.nio.ByteBuffer

import org.scalajs.ir.UTF8String

/** Like a `java.io.ByteArrayOutputStream` but with more control. */
private[backend] final class ByteArrayWriter extends OutputStream {
  private var buffer: Array[Byte] = new Array[Byte](1024)
  private var size: Int = 0

  def currentSize: Int = size

  def sizeHint(capacity: Int): Unit =
    ensureCapacity(capacity)

  private def ensureCapacity(capacity: Int): Unit = {
    if (buffer.length < capacity)
      buffer = java.util.Arrays.copyOf(buffer, powerOfTwoAtLeast(capacity))
  }

  private def powerOfTwoAtLeast(capacity: Int): Int =
    java.lang.Integer.highestOneBit(capacity - 1) << 1

  private def grow(): Unit =
    buffer = java.util.Arrays.copyOf(buffer, buffer.length * 2)

  def write(b: Int): Unit = {
    if (size == buffer.length)
      grow()
    buffer(size) = b.toByte
    size += 1
  }

  override def write(bs: Array[Byte]): Unit =
    write(bs, 0, bs.length)

  override def write(bs: Array[Byte], start: Int, len: Int): Unit = {
    val newSize = size + len
    ensureCapacity(newSize)
    System.arraycopy(bs, start, buffer, size, len)
    size = newSize
  }

  def writeASCIIString(str: String): Unit = {
    val len = str.length()
    val oldSize = size
    val newSize = oldSize + len
    ensureCapacity(newSize)

    val buffer = this.buffer // local copy -- after ensureCapacity!
    var i = 0
    while (i != len) {
      buffer(oldSize + i) = str.charAt(i).toByte
      i += 1
    }

    size = newSize
  }

  def result(): ByteBuffer =
    ByteBuffer.wrap(buffer, 0, size)
}
