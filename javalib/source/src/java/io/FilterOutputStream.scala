package java.io

class FilterOutputStream(out: OutputStream) extends OutputStream {
  override def close() = out.close()
  override def flush() = out.flush()
  override def write(b: Int) = out.write(b)
}
