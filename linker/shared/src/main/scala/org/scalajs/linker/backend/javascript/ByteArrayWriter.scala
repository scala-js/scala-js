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

package org.scalajs.linker.backend.javascript

import java.io.OutputStream
import java.nio.ByteBuffer

/** Like a `java.io.ByteArrayOutputStream` but with more control. */
private[backend] final class ByteArrayWriter extends OutputStream {
  private var buffer: Array[Byte] = new Array[Byte](1024)
  private var size: Int = 0

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

  /** Writes an ASCII-escaped JavaScript string to the buffer.
   *
   *  @return
   *    the number of ASCII chars (i.e., bytes) that were written
   */
  def writeASCIIEscapedJSString(str: String): Int = {
    // scalastyle:off return

    /* Note that Java and JavaScript happen to use the same encoding for
     * Unicode, namely UTF-16, which means that 1 char from Java always equals
     * 1 char in JavaScript.
     */

    // First, a fast path for cases where we do not need to escape anything.

    val oldSize = size
    val len = str.length()
    ensureCapacity(oldSize + len)

    val buffer = this.buffer // local copy -- after ensureCapacity!
    var i = 0
    while (i != len) {
      val c = str.charAt(i).toInt

      if (c >= 32 && c <= 126 && c != '\"' && c != '\\') {
        buffer(oldSize + i) = c.toByte
        i += 1
      } else {
        return writeASCIIEscapedJSStringSlowPath(str, i)
      }
    }

    size = oldSize + len
    len // number of bytes written

    // scalastyle:on return
  }

  /** Slow path when we encounter at least one char needing an escape.
   *
   *  When calling this method, the first `start` chars of `str` have already
   *  been written in the buffer from offset `size` onwards, and there are
   *  still at least `str.length() - start` bytes available in the buffer.
   *
   *  @return
   *    the number of ASCII chars (i.e., bytes) that were written in total,
   *    including the first `start` bytes.
   */
  private def writeASCIIEscapedJSStringSlowPath(str: String, start: Int): Int = {
    val oldSize = size

    var offset = oldSize + start
    val len = str.length()
    var i = start

    // Loop invariant: there is at least `len - i` bytes available in the buffer
    while (i != len) {
      val c = str.charAt(i).toInt
      i += 1

      if (c >= 32 && c <= 126 && c != '\"' && c != '\\') {
        buffer(offset) = c.toByte
        offset += 1
      } else {
        // Grow if needed: at most 6 bytes for the escape + room to maintain the invariant
        ensureCapacity(offset + 6 + (len - i))

        buffer(offset) = '\\'

        if (8 <= c && c < 14) {
          buffer(offset + 1) = ByteArrayWriter.EscapeJSBytes(c)
          offset += 2
        } else if (c == '\"') {
          buffer(offset + 1) = '\"'
          offset += 2
        } else if (c == '\\') {
          buffer(offset + 1) = '\\'
          offset += 2
        } else {
          def hexDigit(x: Int): Byte =
            if (x < 10) (x + '0').toByte else (x + ('a' - 10)).toByte

          buffer(offset + 1) = 'u'
          buffer(offset + 2) = hexDigit(c >> 12)
          buffer(offset + 3) = hexDigit((c >> 8) & 0x0f)
          buffer(offset + 4) = hexDigit((c >> 4) & 0x0f)
          buffer(offset + 5) = hexDigit(c & 0x0f)

          offset += 6
        }
      }
    }

    size = offset
    offset - oldSize // number of bytes written in total
  }

  def toByteBuffer(): ByteBuffer =
    ByteBuffer.wrap(buffer, 0, size).asReadOnlyBuffer()

  def toByteArray(): Array[Byte] =
    java.util.Arrays.copyOf(buffer, size)
}

private object ByteArrayWriter {
  private final val EscapeJSBytes: Array[Byte] =
    "01234567btnvfr".toArray.map(_.toByte) // offsets 0 to 7 are unused
}
