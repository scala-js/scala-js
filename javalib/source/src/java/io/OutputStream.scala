package java.io

abstract class OutputStream extends Object with Closeable with Flushable {
  def close() {}

  def flush() {}

  def write(b: Array[Byte]) {
    write(b, 0, b.length)
  }

  def write(b: Array[Byte], off: Int, len: Int) {
    var n = off;
    val stop = off+len
    while (n < stop) {
      write(b(n))
      n = n+1
    }
  }

  def write(b: Int): Unit
}
