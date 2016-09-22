package java.io

import scala.scalajs.js.typedarray._

class DataInputStream(in: InputStream) extends FilterInputStream(in)
                                          with DataInput {

  /* Due to the method readLine, we need to be able to push back a byte (if we
   * read a \r and the following byte is NOT a \n). We implement this in the
   * read() and the consumePos() method.
   */
  private var pushedBack: Int = -1
  private var pushedBackMark: Int = -1

  /* ArrayBufferInputStream mode helpers
   *
   * These variables are used to special case on ArrayBufferInputStreams
   * They allow directly accessing the underlying ArrayBuffer rather than
   * creating byte arrays first.
   */
  private val inArrayBufferStream = in match {
    case in: ArrayBufferInputStream => in
    case _ => null
  }
  private val hasArrayBuffer = inArrayBufferStream != null
  private val bufDataView = {
    if (hasArrayBuffer) {
      val in = inArrayBufferStream
      new DataView(in.buffer, in.offset, in.length)
    } else {
      null
    }
  }

  private def consumePos(n: Int) = {
    val off = if (pushedBack != -1) 1 else 0
    val resultPos = inArrayBufferStream.pos - off
    val toSkip = n - off
    if (in.skip(toSkip) != toSkip) eof()
    resultPos
  }

  // General Helpers
  private def eof() = throw new EOFException()
  private def pushBack(v: Int) = { pushedBack = v }

  // Methods on DataInput
  def readBoolean(): Boolean = readByte() != 0

  def readByte(): Byte = {
    val res = read()
    if (res == -1) eof()
    res.toByte
  }

  def readChar(): Char = {
    if (hasArrayBuffer)
      bufDataView.getUint16(consumePos(2)).toChar
    else
      ((readByte() << 8) | readUnsignedByte()).toChar
  }

  def readDouble(): Double = {
    if (hasArrayBuffer)
      bufDataView.getFloat64(consumePos(8))
    else
      java.lang.Double.longBitsToDouble(readLong())
  }

  def readFloat(): Float = {
    if (hasArrayBuffer)
      bufDataView.getFloat32(consumePos(4))
    else
      java.lang.Float.intBitsToFloat(readInt())
  }

  def readFully(b: Array[Byte]): Unit = readFully(b, 0, b.length)

  def readFully(b: Array[Byte], off: Int, len: Int): Unit = {
    if (off < 0 || len < 0 || off + len > b.length)
      throw new IndexOutOfBoundsException()

    var remaining = len
    var offset = off
    while (remaining > 0) {
      val readCount = read(b, offset, remaining)
      if (readCount == -1) eof()
      remaining -= readCount
      offset += readCount
    }
  }

  def readInt(): Int = {
    if (hasArrayBuffer) {
      bufDataView.getInt32(consumePos(4))
    } else {
      (readUnsignedByte() << 24) | (readUnsignedByte() << 16) |
      (readUnsignedByte() << 8) | readUnsignedByte()
    }
  }

  def readLine(): String = {
    var cur = read()
    if (cur == -1) null
    else {
      var res = ""
      while (cur != -1 && cur != '\n' && cur != '\r') {
        res += cur.toChar
        cur = read()
      }
      if (cur == '\r') {
        // Discard a potential \n (from \r\n line endings)
        cur = read()
        if (cur != '\n') pushBack(cur)
      }
      res
    }
  }

  def readLong(): Long = {
    val hi = readInt().toLong
    val lo = readInt().toLong
    (hi << 32) | (lo & 0xFFFFFFFFL)
  }

  def readShort(): Short = {
    if (hasArrayBuffer)
      bufDataView.getInt16(consumePos(2))
    else
      ((readByte() << 8) | readUnsignedByte()).toShort
  }

  def readUnsignedByte(): Int = {
    val res = read()
    if (res == -1) eof()
    res
  }

  def readUnsignedShort(): Int = {
    if (hasArrayBuffer)
      bufDataView.getUint16(consumePos(2))
    else
      (readUnsignedByte() << 8) | readUnsignedByte()
  }

  def readUTF(): String = {
    val length = readShort()
    var res = ""
    var i = 0

    def badFormat(msg: String) = throw new UTFDataFormatException(msg)

    while (i < length) {
      val a = read()

      if (a == -1)
        badFormat(s"Unexpected EOF: ${length - i} bytes to go")

      i += 1

      val char = {
        if ((a & 0x80) == 0x00) { // 0xxxxxxx
          a.toChar
        } else if ((a & 0xE0) == 0xC0 && i < length) { // 110xxxxx
          val b = read()
          i += 1

          if (b == -1)
            badFormat(f"Expected 2 bytes, found: EOF (init: $a%#02x)")
          if ((b & 0xC0) != 0x80) // 10xxxxxx
            badFormat(f"Expected 2 bytes, found: $b%#02x (init: $a%#02x)")

          (((a & 0x1F) << 6) | (b & 0x3F)).toChar
        } else if ((a & 0xF0) == 0xE0 && i < length - 1) { // 1110xxxx
          val b = read()
          val c = read()
          i += 2

          if (b == -1)
            badFormat(f"Expected 3 bytes, found: EOF (init: $a%#02x)")

          if ((b & 0xC0) != 0x80)   // 10xxxxxx
            badFormat(f"Expected 3 bytes, found: $b%#02x (init: $a%#02x)")

          if (c == -1)
            badFormat(f"Expected 3 bytes, found: $b%#02x, EOF (init: $a%#02x)")

          if ((c & 0xC0) != 0x80)   // 10xxxxxx
            badFormat(
                f"Expected 3 bytes, found: $b%#02x, $c%#02x (init: $a%#02x)")

          (((a & 0x0F) << 12) | ((b & 0x3F) << 6) | (c & 0x3F)).toChar
        } else {
          val rem = length - i
          badFormat(f"Unexpected start of char: $a%#02x ($rem%d bytes to go)")
        }
      }

      res += char
    }

    res
  }

  def skipBytes(n: Int): Int = skip(n.toLong).toInt

  // Methods on FilterInputStream.
  // Overridden to track pushedBack / pushedBackMark
  override def available(): Int = {
    if (pushedBack != -1) in.available + 1
    else in.available
  }

  override def mark(readlimit: Int): Unit = {
    in.mark(readlimit + 1) // we need one more since we might read ahead
    pushedBackMark = pushedBack
  }

  override def markSupported(): Boolean = in.markSupported()

  override def read(): Int = {
    val res = {
      if (pushedBack != -1)
        pushedBack
      else
        in.read()
    }

    pushedBack = -1

    res
  }

  override def read(b: Array[Byte], off: Int, len: Int): Int = {
    if (len == 0)
      0
    else if (pushedBack != -1) {
      b(off) = pushedBack.toByte
      pushedBack = -1
      1
    } else {
      val count = in.read(b, off, len)
      count
    }
  }

  override def reset(): Unit = {
    in.reset()
    pushedBack = pushedBackMark
  }

  override def skip(n: Long): Long = {
    if (n == 0)
      0L
    else if (pushedBack != -1) {
      pushedBack = -1
      1L
    } else {
      val skipped = in.skip(n)
      skipped
    }
  }

}
