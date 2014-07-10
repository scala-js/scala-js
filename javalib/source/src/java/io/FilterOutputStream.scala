package java.io

class FilterOutputStream(protected val out: OutputStream) extends OutputStream {
  override def close(): Unit = out.close()
  override def flush(): Unit = out.flush()
  override def write(b: Int): Unit = out.write(b)
}
