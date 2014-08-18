package java.io

import scala.scalajs.js

import scala.annotation.tailrec

class ByteArrayOutputStream(initBufSize: Int) extends OutputStream {

  protected var buf: Array[Byte] = new Array(initBufSize)
  protected var count: Int = 0

  def this() = this(32)

  override def close(): Unit = {}

  def reset(): Unit = { count = 0 }

  def size(): Int = count

  def toByteArray(): Array[Byte] = {
    val res = new Array[Byte](count)
    System.arraycopy(buf, 0, res, 0, count)
    res
  }

  override def toString(): String = {
    val r = new InputStreamReader(new ByteArrayInputStream(toByteArray))
    val out = new Array[Char](count)

    var res = ""

    @tailrec
    def loop(): Unit = {
      val c = r.read(out)
      if (c > 0) {
        res += new String(out, 0, c)
        loop()
      }
    }

    loop()

    res
  }

  override def write(b: Array[Byte], off: Int, len: Int): Unit = {
    if (off < 0 || len < 0 || off + len > b.length)
      throw new IndexOutOfBoundsException()

    if (count + len > buf.length) incBuf(len)

    System.arraycopy(b, off, buf, count, len)
    count += len
  }

  override def write(b: Int): Unit = {
    if (count >= buf.length) incBuf(1)

    buf(count) = b.toByte
    count += 1
  }

  def writeTo(out: OutputStream): Unit =
    out.write(buf, 0, count)

  private def incBuf(min: Int): Unit = {
    val newSize = Math.max(buf.length + min, buf.length * 2)
    val newBuf = new Array[Byte](newSize)
    System.arraycopy(buf, 0, newBuf, 0, count)
    buf = newBuf
  }

}
