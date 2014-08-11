package java.io

abstract class Writer private[this] (_lock: Option[Object]) extends
    Appendable with Flushable with Closeable {

  protected val lock = _lock.getOrElse(this)

  protected def this(lock: Object) = this(Some(lock))
  protected def this() = this(None)

  def append(c: Char): Writer = {
    write(c.toInt)
    this
  }

  def append(csq: CharSequence): Writer = {
    write(csq.toString)
    this
  }

  def append(csq: CharSequence, start: Int, end: Int): Writer = {
    write(csq.subSequence(start, end).toString)
    this
  }

  def close(): Unit

  def flush(): Unit

  def write(cbuf: Array[Char], off: Int, len: Int): Unit

  def write(cbuf: Array[Char]): Unit = write(cbuf, 0, cbuf.length)

  def write(c: Int): Unit = write(Array(c.toChar))

  def write(str: String): Unit = write(str.toCharArray)

  def write(str: String, off: Int, len: Int): Unit =
    write(str.toCharArray, off, len)

}
