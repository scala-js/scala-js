package java.io

abstract class InputStream extends Closeable {
  def available(): Int = 0
  def close(): Unit = {}
  def mark(readlimit: Int): Unit = {}
  def markSupported(): Boolean = false
  def read(): Int
  def read(b: Array[Byte]): Int = read(b, 0, b.length)

  def read(b: Array[Byte], off: Int, len: Int): Int = {
    if (off < 0 || len < 0 || len > b.length - off)
      throw new IndexOutOfBoundsException

    if (len == 0) 0
    else {
      var bytesWritten = 0
      var next = 0

      while (bytesWritten < len && next != -1) {
        next = read()
        if (next != -1) {
          b(off + bytesWritten) = next.toByte
          bytesWritten += 1
        }
      }

      if (bytesWritten <= 0) -1
      else bytesWritten
    }
  }

  def reset(): Unit = throw new IOException("Reset not supported")

  def skip(n: Long): Long = {
    var skipped = 0
    while (skipped < n && read() != -1)
      skipped += 1
    skipped
  }
}
