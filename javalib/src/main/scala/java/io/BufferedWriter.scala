package java.io

class BufferedWriter(out: Writer, sz: Int) extends Writer {

  if (sz <= 0) throw new IllegalArgumentException("Buffer size <= 0")

  def this(out: Writer) = this(out, 4096)

  private val buffer: Array[Char] = new Array[Char](sz)
  private var pos: Int = 0
  private var closed: Boolean = false

  def close(): Unit = if (!closed) {
    flush()
    out.close()
    closed = true
  }

  def flush(): Unit = {
    ensureOpen()
    out.write(buffer, 0, pos)
    out.flush()
    pos = 0
  }

  def newLine(): Unit =
    write(System.lineSeparator(), 0, System.lineSeparator().length)

  override def write(c: Int): Unit =
    write(Array(c.toChar), 0, 1)

  override def write(s: String, off: Int, len: Int): Unit =
    write(s.toCharArray, off, len)

  def write(cbuf: Array[Char], off: Int, len: Int): Unit = {
    ensureOpen()
    val available = sz - pos
    if (available >= len) {
      System.arraycopy(cbuf, off, buffer, pos, len)
      pos += len
      if (pos == sz) flush()
    } else {
      write(cbuf, off, available)
      write(cbuf, off + available, len - available)
    }
  }

  private def ensureOpen(): Unit =
    if (closed) throw new IOException("Stream closed")
}
