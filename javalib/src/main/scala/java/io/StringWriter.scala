package java.io

class StringWriter extends Writer {

  private[this] val buf = new StringBuffer

  def this(initialSize: Int) = this()

  override def write(c: Int): Unit =
    buf.append(c.toChar)

  def write(cbuf: Array[Char], off: Int, len: Int): Unit =
    buf.append(cbuf, off, len)

  override def write(str: String): Unit =
    buf.append(str)

  override def write(str: String, off: Int, len: Int): Unit =
    buf.append(str, off, off + len) // Third param is 'end', not 'len'

  override def append(csq: CharSequence): StringWriter = {
    buf.append(csq)
    this
  }

  override def append(csq: CharSequence, start: Int, end: Int): StringWriter = {
    buf.append(csq, start, end)
    this
  }

  override def append(c: Char): StringWriter = {
    buf.append(c)
    this
  }

  override def toString(): String = buf.toString

  def getBuffer(): StringBuffer = buf

  def flush(): Unit = ()

  def close(): Unit = ()

}
