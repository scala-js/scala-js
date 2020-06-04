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

import java.nio.CharBuffer

import scala.annotation.tailrec

abstract class Reader private[this] (_lock: Option[Object])
    extends Readable with Closeable {

  protected val lock = _lock.getOrElse(this)

  protected def this(lock: Object) = this(Some(lock))
  protected def this() = this(None)

  def read(target: CharBuffer): Int = {
    if (!target.hasRemaining()) 0
    else if (target.hasArray()) {
      val charsRead = read(target.array(),
          target.position() + target.arrayOffset(), target.remaining())
      if (charsRead != -1)
        target.position(target.position() + charsRead)
      charsRead
    } else {
      val buf = new Array[Char](target.remaining())
      val charsRead = read(buf)
      if (charsRead != -1)
        target.put(buf, 0, charsRead)
      charsRead
    }
  }

  def read(): Int = {
    val buf = new Array[Char](1)
    if (read(buf) == -1) -1
    else buf(0).toInt
  }

  def read(cbuf: Array[Char]): Int =
    read(cbuf, 0, cbuf.length)

  def read(cbuf: Array[Char], off: Int, len: Int): Int

  def skip(n: Long): Long = {
    if (n < 0)
      throw new IllegalArgumentException("Cannot skip negative amount")

    val buffer = new Array[Char](8192)
    @tailrec
    def loop(m: Long, lastSkipped: Long): Long = {
      if (m <= 0) {
        lastSkipped
      } else {
        val mMin = Math.min(m, 8192).toInt
        val skipped = read(buffer, 0, mMin)
        if (skipped < 0) {
          lastSkipped
        } else {
          val totalSkipped = lastSkipped + skipped
          loop(m - mMin, totalSkipped)
        }
      }
    }
    loop(n, 0)
  }

  def ready(): Boolean = false

  def markSupported(): Boolean = false

  def mark(readAheadLimit: Int): Unit =
    throw new IOException("Mark not supported")

  def reset(): Unit =
    throw new IOException("Reset not supported")

  def close(): Unit

}
