package java.io

import scala.scalajs.js

import scala.annotation.tailrec

class ByteArrayOutputStream(initBufSize: Int) extends OutputStream {

  protected var buf: Array[Byte] = new Array(initBufSize)
  protected var count: Int = 0

  def this() = this(32)

  override def write(b: Int): Unit = {
    if (count >= buf.length)
      growBuf(1)

    buf(count) = b.toByte
    count += 1
  }

  override def write(b: Array[Byte], off: Int, len: Int): Unit = {
    if (off < 0 || len < 0 || len > b.length - off)
      throw new IndexOutOfBoundsException()

    if (count + len > buf.length)
      growBuf(len)

    System.arraycopy(b, off, buf, count, len)
    count += len
  }

  def writeTo(out: OutputStream): Unit =
    out.write(buf, 0, count)

  def reset(): Unit =
    count = 0

  def toByteArray(): Array[Byte] = {
    val res = new Array[Byte](count)
    System.arraycopy(buf, 0, res, 0, count)
    res
  }

  def size(): Int = count

  override def toString(): String =
    new String(buf, 0, count)

  def toString(charsetName: String): String =
    new String(buf, 0, count, charsetName)

  override def close(): Unit = ()

  private def growBuf(minIncrement: Int): Unit = {
    val newSize = Math.max(count + minIncrement, buf.length * 2)
    val newBuf = new Array[Byte](newSize)
    System.arraycopy(buf, 0, newBuf, 0, count)
    buf = newBuf
  }

}
