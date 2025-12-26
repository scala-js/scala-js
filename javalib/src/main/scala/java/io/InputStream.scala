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

import java.util.Arrays

abstract class InputStream extends Closeable {
  def read(): Int

  def read(b: Array[Byte]): Int = read(b, 0, b.length)

  def read(b: Array[Byte], off: Int, len: Int): Int = {
    if (off < 0 || len < 0 || len > b.length - off)
      throw new IndexOutOfBoundsException

    if (len == 0) 0
    else {
      var bytesWritten = 0
      var next = 0

      while (bytesWritten < len && next != -1) {
        next = {
          if (bytesWritten == 0) read()
          else {
            try read()
            catch { case _: IOException => -1 }
          }
        }
        if (next != -1) {
          b(off + bytesWritten) = next.toByte
          bytesWritten += 1
        }
      }

      if (bytesWritten <= 0) -1
      else bytesWritten
    }
  }

  def readAllBytes(): Array[Byte] =
    readNBytes(Integer.MAX_VALUE)

  def readNBytes(len: Int): Array[Byte] = {
    if (len < 0) {
      throw new IllegalArgumentException
    } else if (len == 0) {
      new Array[Byte](0)
    } else {
      var bytesRead = 0

      /* Allocate a buffer.
       *
       * Note that the implementation is required to grow memory proportional to
       * the amount read, not the amount requested. Therefore, we cannot simply
       * allocate an array of length len.
       */
      var buf = new Array[Byte](Math.min(len, 1024))

      var lastRead = 0

      while (bytesRead < len && lastRead != -1) {
        if (buf.length == bytesRead) {
          /* Note that buf.length < Integer.MAX_VALUE, because:
           * - bytesRead < len (loop condition)
           * - len <= Integer.MAX_VALUE (because of its type)
           */
          val newLen =
            if (Integer.MAX_VALUE / 2 > buf.length) Integer.MAX_VALUE
            else buf.length * 2
          buf = Arrays.copyOf(buf, Math.min(len, newLen))
        }

        lastRead = read(buf, bytesRead, buf.length - bytesRead)
        if (lastRead > 0)
          bytesRead += lastRead
      }

      if (buf.length > bytesRead)
        Arrays.copyOf(buf, bytesRead)
      else
        buf
    }
  }

  def readNBytes(b: Array[Byte], off: Int, len: Int): Int = {
    if (off < 0 || len < 0 || len > b.length - off)
      throw new IndexOutOfBoundsException

    var bytesRead = 0
    var lastRead = 0
    while (bytesRead < len && lastRead != -1) {
      lastRead = read(b, off + bytesRead, len - bytesRead)
      if (lastRead > 0) {
        bytesRead += lastRead
      }
    }

    bytesRead
  }

  def skip(n: Long): Long = {
    var skipped = 0
    while (skipped < n && read() != -1)
      skipped += 1
    skipped
  }

  def skipNBytes(n: Long): Unit = {
    var remaining = n
    while (remaining > 0) {
      val skipped = skip(remaining)
      if (skipped < 0 || skipped > remaining) {
        throw new IOException
      } else if (skipped == 0) {
        if (read() == -1)
          throw new EOFException
        remaining -= 1
      } else {
        remaining -= skipped
      }
    }
  }

  def available(): Int = 0

  def close(): Unit = ()

  def mark(readlimit: Int): Unit = ()

  def reset(): Unit =
    throw new IOException("Reset not supported")

  def markSupported(): Boolean = false

  def transferTo(out: OutputStream): Long = {
    out.getClass() // Trigger NPE (if enabled).

    var transferred = 0L
    val buf = new Array[Byte](4096)
    var bytesRead = 0

    while (bytesRead != -1) {
      bytesRead = read(buf)
      if (bytesRead != -1) {
        out.write(buf, 0, bytesRead)
        transferred += bytesRead
      }
    }

    transferred
  }
}

object InputStream {
  def nullInputStream(): InputStream = new InputStream {
    private[this] var closed = false

    @inline
    private def ensureOpen(): Unit = {
      if (closed)
        throw new IOException
    }

    override def available(): Int = {
      ensureOpen()
      0
    }

    def read(): Int = {
      ensureOpen()
      -1
    }

    override def readNBytes(n: Int): Array[Byte] = {
      ensureOpen()
      super.readNBytes(n)
    }

    override def readNBytes(b: Array[Byte], off: Int, len: Int): Int = {
      ensureOpen()
      super.readNBytes(b, off, len)
    }

    override def skip(n: Long): Long = {
      ensureOpen()
      0L
    }

    override def skipNBytes(n: Long): Unit = {
      ensureOpen()
      super.skipNBytes(n)
    }

    override def close(): Unit =
      closed = true
  }
}
