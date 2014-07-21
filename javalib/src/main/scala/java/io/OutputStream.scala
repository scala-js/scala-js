package java.io

abstract class OutputStream extends Object with Closeable with Flushable {
  def close(): Unit = {}

  def flush(): Unit = {}

  def write(b: Array[Byte]): Unit = {
    write(b, 0, b.length)
  }

  def write(b: Array[Byte], off: Int, len: Int): Unit = {
    var n = off;
    val stop = off+len
    while (n < stop) {
      write(b(n))
      n = n+1
    }
  }

  def write(b: Int): Unit
}
