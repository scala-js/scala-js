package java.io

class InputStreamReader(in: InputStream) extends Reader {

  private[this] var closed = false

  def this(in: InputStream, charsetName: String) = {
    this(in)

    if (charsetName.toUpperCase != "UTF-8" && charsetName.toUpperCase != "UTF8")
      throw new UnsupportedEncodingException("Only UTF8 is supported")
  }

  def close(): Unit = { closed = true }

  def getEncoding(): String = "UTF8"

  override def read(): Int = {
    ensureOpened()

    val init = in.read()

    if (init == -1) -1
    else {
      val addByteC = {
        // Number of leading bits
        var tmp = init
        var c = -1
        while ((tmp & 0x80) != 0) {
          c += 1
          tmp <<= 1
        }
        Math.max(c, 0)
      }

      if (addByteC > 3 || (init & 0xC0) == 0x80)
        failUTF(f"encountered $init%#x as first byte")

      // Take initial byte, zero out leftmost bits
      var acc = ((init << (addByteC + 1)) & 0xFF) >>> (addByteC + 1)

      // Read and accumulate additional bytes
      for (i <- 0 until addByteC) {
        val tmp = in.read()
        if (tmp == -1) failUTF(s"missing ${addByteC - i} bytes")
        acc = (acc << 6) | (tmp & 0x3F)
      }

      if (acc > 0xFFFF) failUTF(f"total value of $acc%#x (too high)")

      acc
    }
  }

  def read(cbuf: Array[Char], off: Int, len: Int): Int = {
    ensureOpened()

    if (off < 0 || len < 0 || len > cbuf.length - off)
      throw new IndexOutOfBoundsException

    if (len == 0) 0
    else {
      var bytesWritten = 0
      var next = 0

      while (bytesWritten < len && next != -1) {
        next = read()
        if (next != -1) {
          cbuf(off + bytesWritten) = next.toChar
          bytesWritten += 1
        }
      }

      if (bytesWritten <= 0) -1
      else bytesWritten
    }
  }

  override def ready(): Boolean = in.available() >= 4

  private def failUTF(msg: String): Nothing =
    throw new IOException("Invalid UTF8 sequence: " + msg)

  private def ensureOpened(): Unit =
    if (closed) throw new IOException("Stream closed")

}
