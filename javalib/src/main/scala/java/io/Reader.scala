package java.io

// Note: Don't extend Readable, otherwise we'll have to implement
// read(cb: CharBuffer), which we don't want to (depends on java.nio)
abstract class Reader private[this] (_lock: Option[Object]) extends Closeable {

  protected val lock = _lock.getOrElse(this)

  protected def this(lock: Object) = this(Some(lock))
  protected def this() = this(None)

  def close(): Unit
  def mark(readAheadLimit: Int): Unit = ()
  def markSupported(): Boolean = false

  def read(): Int = {
    val buf = new Array[Char](1)
    if (read(buf) == -1) -1
    else buf(0).toInt
  }

  def read(cbuf: Array[Char]): Int =
    read(cbuf, 0, cbuf.length)

  def read(cbuf: Array[Char], off: Int, len: Int): Int

  def ready(): Boolean = false

  def reset(): Unit = throw new IOException("Reset not supported")

  def skip(n: Long): Long = {
    if (n < 0)
      throw new IllegalArgumentException("Can't skip negative amount")
    else if (read() == -1) 0
    else 1
  }

}
