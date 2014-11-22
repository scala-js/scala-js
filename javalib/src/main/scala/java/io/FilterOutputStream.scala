package java.io

class FilterOutputStream(protected val out: OutputStream) extends OutputStream {
  def write(b: Int): Unit =
    out.write(b)

  override def write(b: Array[Byte]): Unit =
    write(b, 0, b.length) // this is spec! it must not call out.write(b)

  override def write(b: Array[Byte], off: Int, len: Int): Unit =
    super.write(b, off, len) // calls this.write(Int) repeatedly

  override def flush(): Unit = out.flush()

  override def close(): Unit = out.close()
}
