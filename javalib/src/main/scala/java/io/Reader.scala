package java.io

import java.nio.CharBuffer

abstract class Reader private[this] (_lock: Option[Object])
    extends Readable with Closeable {

  protected val lock = _lock.getOrElse(this)

  protected def this(lock: Object) = this(Some(lock))
  protected def this() = this(None)

  def read(target: CharBuffer): Int = {
    if (!target.hasRemaining) 0
    else if (target.hasArray) {
      val charsRead = read(target.array,
          target.position + target.arrayOffset, target.remaining)
      if (charsRead != -1)
        target.position(target.position + charsRead)
      charsRead
    } else {
      val buf = new Array[Char](target.remaining)
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
    else if (read() == -1) 0
    else 1
  }

  def ready(): Boolean = false

  def markSupported(): Boolean = false

  def mark(readAheadLimit: Int): Unit =
    throw new IOException("Mark not supported")

  def reset(): Unit =
    throw new IOException("Reset not supported")

  def close(): Unit

}
