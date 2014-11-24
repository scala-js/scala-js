package java.io

abstract class OutputStream extends Object with Closeable with Flushable {
  def write(b: Int): Unit

  def write(b: Array[Byte]): Unit =
    write(b, 0, b.length)

  def write(b: Array[Byte], off: Int, len: Int): Unit = {
    if (off < 0 || len < 0 || len > b.length - off)
      throw new IndexOutOfBoundsException()

    var n = off
    val stop = off + len
    while (n < stop) {
      write(b(n))
      n += 1
    }
  }

  def flush(): Unit = ()

  def close(): Unit = ()

}
