package java.io

class FilterInputStream protected (
    protected val in: InputStream) extends InputStream {
  override def available(): Int = in.available()
  override def close(): Unit = in.close()
  override def mark(readlimit: Int): Unit = in.mark(readlimit)
  override def markSupported(): Boolean = in.markSupported()
  override def read(): Int = in.read()
  override def read(b: Array[Byte], off: Int, len: Int): Int =
    in.read(b, off, len)
  override def reset(): Unit = in.reset()
  override def skip(n: Long): Long = in.skip(n)
}
