package java.io

class DataOutputStream(out: OutputStream)
    extends FilterOutputStream(out) with DataOutput {

  protected var written: Int = 0

  override def write(b: Int): Unit = {
    super.write(b)
    written += 1
  }

  override def write(b: Array[Byte], off: Int, len: Int): Unit = {
    out.write(b, off, len)
    written += len
  }

  override def flush(): Unit =
    super.flush()

  final def writeBoolean(v: Boolean): Unit =
    write(if (v) 1 else 0)

  final def writeByte(v: Int): Unit =
    write(v)

  final def writeShort(v: Int): Unit = {
    write(v >> 8)
    write(v)
  }

  final def writeChar(v: Int): Unit = {
    write(v >> 8)
    write(v)
  }

  final def writeInt(v: Int): Unit = {
    write(v >> 24)
    write(v >> 16)
    write(v >> 8)
    write(v)
  }

  @inline
  final def writeLong(v: Long): Unit = {
    writeInt((v >>> 32).toInt)
    writeInt(v.toInt)
  }

  final def writeFloat(v: Float): Unit =
    writeInt(java.lang.Float.floatToIntBits(v))

  final def writeDouble(v: Double): Unit =
    writeLong(java.lang.Double.doubleToLongBits(v))

  final def writeBytes(s: String): Unit = {
    for (c <- s)
      write(c.toInt)
  }

  final def writeChars(s: String): Unit = {
    for (c <- s)
      writeChar(c)
  }

  final def writeUTF(s: String): Unit = {
    val buffer = new Array[Byte](2 + 3*s.length)

    var idx = 2
    for (c <- s) {
      if (c <= 0x7f && c >= 0x01) {
        buffer(idx) = c.toByte
        idx += 1
      } else if (c < 0x0800) {
        buffer(idx) = ((c >> 6) | 0xc0).toByte
        buffer(idx + 1) = ((c & 0x3f) | 0x80).toByte
        idx += 2
      } else {
        buffer(idx) = ((c >> 12) | 0xe0).toByte
        buffer(idx + 1) = (((c >> 6) & 0x3f) | 0x80).toByte
        buffer(idx + 2) = ((c & 0x3f) | 0x80).toByte
        idx += 3
      }
    }

    val len = idx - 2

    if (len >= 0x10000)
      throw new UTFDataFormatException(s"encoded string too long: $len bytes")

    buffer(0) = (len >> 8).toByte
    buffer(1) = len.toByte

    write(buffer, 0, idx)
  }

  final def size(): Int = written
}
