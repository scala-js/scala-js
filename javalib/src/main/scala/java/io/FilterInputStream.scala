package java.io

class FilterInputStream protected (
    protected val in: InputStream) extends InputStream {

  override def read(): Int =
    in.read()

  override def read(b: Array[Byte]): Int =
    read(b, 0, b.length) // this is spec! must not do in.read(b)

  override def read(b: Array[Byte], off: Int, len: Int): Int =
    in.read(b, off, len)

  override def skip(n: Long): Long = in.skip(n)

  override def available(): Int = in.available()

  override def close(): Unit = in.close()

  override def mark(readlimit: Int): Unit = in.mark(readlimit)
  override def markSupported(): Boolean = in.markSupported()
  override def reset(): Unit = in.reset()
}
