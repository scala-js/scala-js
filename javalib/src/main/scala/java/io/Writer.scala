package java.io

abstract class Writer private[this] (_lock: Option[Object]) extends
    Appendable with Closeable with Flushable {

  protected val lock = _lock.getOrElse(this)

  protected def this(lock: Object) = this(Some(lock))
  protected def this() = this(None)

  def write(c: Int): Unit =
    write(Array(c.toChar))

  def write(cbuf: Array[Char]): Unit =
    write(cbuf, 0, cbuf.length)

  def write(cbuf: Array[Char], off: Int, len: Int): Unit

  def write(str: String): Unit =
    write(str.toCharArray)

  def write(str: String, off: Int, len: Int): Unit =
    write(str.toCharArray, off, len)

  def append(csq: CharSequence): Writer = {
    write(if (csq == null) "null" else csq.toString)
    this
  }

  def append(csq: CharSequence, start: Int, end: Int): Writer = {
    val csq1 = if (csq == null) "null" else csq
    write(csq1.subSequence(start, end).toString)
    this
  }

  def append(c: Char): Writer = {
    write(c.toInt)
    this
  }

  def flush(): Unit

  def close(): Unit

}
