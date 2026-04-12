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

abstract class OutputStream extends Object with Closeable with Flushable {
  def write(b: Int): Unit

  def write(b: Array[Byte]): Unit =
    write(b, 0, b.length)

  def write(b: Array[Byte], off: Int, len: Int): Unit = {
    val stop = BoundsChecks.checkOffsetCount(off, len, b.length)
    var n = off
    while (n < stop) {
      write(b(n))
      n += 1
    }
  }

  def flush(): Unit = ()

  def close(): Unit = ()

}

object OutputStream {
  def nullOutputStream(): OutputStream = new OutputStream {
    private[this] var closed = false

    private def ensureOpen(): Unit = {
      if (closed)
        throw new IOException
    }

    def write(b: Int): Unit = ensureOpen()

    override def write(b: Array[Byte]): Unit = {
      ensureOpen()

      b.length // Null check
    }

    override def write(b: Array[Byte], off: Int, len: Int): Unit = {
      ensureOpen()

      BoundsChecks.checkOffsetCount(off, len, b.length)
    }

    override def close(): Unit =
      closed = true
  }
}
