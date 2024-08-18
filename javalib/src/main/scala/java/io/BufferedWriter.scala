// Ported from Scala-native

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

class BufferedWriter(out: Writer, sz: Int) extends Writer {

  if (sz <= 0) throw new IllegalArgumentException("Buffer size <= 0")

  def this(out: Writer) = this(out, 4096)

  private[this] val buffer: Array[Char] = new Array[Char](sz)
  private[this] var pos: Int = 0
  private[this] var closed: Boolean = false

  def close(): Unit = if (!closed) {
    flush()
    out.close()
    closed = true
  }

  def flush(): Unit = {
    ensureOpen()
    if (pos > 0) {
      out.write(buffer, 0, pos)
      out.flush()
      pos = 0
    }
  }

  def newLine(): Unit =
    write(System.lineSeparator())

  override def write(c: Int): Unit = {
    buffer.update(pos, c.toChar)
    pos += 1
    if (pos == sz)
      flush()
  }

  override def write(s: String, off: Int, len: Int): Unit =
    write(s.toCharArray, off, len)

  def write(cbuf: Array[Char], off: Int, len: Int): Unit = {
    ensureOpen()
    if (len > 0 || off + len > 0) {
      val available = sz - pos
      if (available > len) {
        System.arraycopy(cbuf, off, buffer, pos, len)
        pos += len
      } else {
        flush()
        out.write(cbuf, off, len)
        out.flush()
      }
    }
  }

  private def ensureOpen(): Unit =
    if (closed) throw new IOException("Stream closed")
}
