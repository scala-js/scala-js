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

class ByteArrayInputStream(
    protected val buf: Array[Byte],
    offset: Int, length: Int) extends InputStream {

  protected val count: Int = offset + length
  protected var mark: Int = offset
  protected var pos: Int = offset

  def this(buf: Array[Byte]) = this(buf, 0, buf.length)

  override def read(): Int = {
    if (pos >= count)
      -1
    else {
      val res = buf(pos) & 0xff // convert to unsigned int
      pos += 1
      res
    }
  }

  override def read(b: Array[Byte], off: Int, reqLen: Int): Int = {
    if (off < 0 || reqLen < 0 || reqLen > b.length - off)
      throw new IndexOutOfBoundsException

    if (pos == count) {
      /* There is nothing left to read.
       * #3913: return -1 even if reqLen == 0.
       */
      -1
    } else {
      val len = Math.min(reqLen, count - pos)
      System.arraycopy(buf, pos, b, off, len)
      pos += len
      len
    }
  }

  override def skip(n: Long): Long = {
    val k = Math.max(0, Math.min(n, count - pos))
    pos += k.toInt
    k.toLong
  }

  override def available(): Int = count - pos

  override def markSupported(): Boolean = true

  override def mark(readlimit: Int): Unit =
    mark = pos

  override def reset(): Unit =
    pos = mark

  override def close(): Unit = ()

}
