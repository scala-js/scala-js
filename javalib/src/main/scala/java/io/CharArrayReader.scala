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

package java.io

class CharArrayReader(protected var buf: Array[Char], offset: Int, length: Int) extends Reader {
  if (offset < 0 || offset > buf.length || length < 0 || offset + length < 0)
    throw new IllegalArgumentException

  protected var pos: Int = offset
  protected var markedPos: Int = offset

  // count is actually the "end" index
  protected var count: Int = Math.min(offset + length, buf.length)

  def this(buf: Array[Char]) = this(buf, 0, buf.length)

  override def close(): Unit = this.buf = null

  override def mark(readAheadLimit: Int): Unit = {
    ensureOpen()

    // The parameter readAheadLimit is ignored for CharArrayReaders
    this.markedPos = this.pos
  }

  override def markSupported(): Boolean = true

  override def read(): Int = {
    ensureOpen()

    if (this.pos == this.count) {
      -1
    } else {
      this.pos += 1
      buf(this.pos - 1)
    }
  }

  override def read(buffer: Array[Char], offset: Int, len: Int): Int = {
    if (offset < 0 || offset > buffer.length)
      throw new ArrayIndexOutOfBoundsException("Offset out of bounds : " + offset)

    if (len < 0 || len > buffer.length - offset)
      throw new ArrayIndexOutOfBoundsException("Length out of bounds : " + len)

    ensureOpen()

    if (this.pos < this.count) {
      val bytesRead = Math.min(len, this.count - this.pos)
      System.arraycopy(this.buf, this.pos, buffer, offset, bytesRead)
      this.pos += bytesRead
      bytesRead
    } else {
      -1
    }
  }

  override def ready(): Boolean = {
    ensureOpen()

    /* JDK spec says "Character-array readers are always ready to be read."
     * However, testing shows it returns false when pos == count
     */
    this.pos != this.count
  }

  override def reset(): Unit = {
    ensureOpen()

    this.pos = this.markedPos
  }

  override def skip(n: Long): Long = {
    ensureOpen()

    val available: Long = (this.count - this.pos).toLong
    val skipped: Long = Math.max(0L, Math.min(n, available))
    this.pos += skipped.toInt
    skipped
  }

  private def ensureOpen(): Unit = {
    if (this.buf == null)
      throw new IOException("CharArrayReader is closed.")
  }
}
