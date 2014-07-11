package scala.scalajs.js.typedarray

import java.io.InputStream

/** A java.io.InputStream wrapping a JavaScript ArrayBuffer
 * 
 *  This class is extremely similar to a ByteArrayInputStream, but
 *  uses ArrayBuffers as the underlying representation. Stream
 *  implementations may special case on this stream for better
 *  performance and access the underlying buffer directly. (They still
 *  need to make sure the internal pointers are properly aligned
 *  though).
 * 
 *  @param buffer Underlying ArrayBuffer
 *  @param offset Offset in bytes in [[buffer]]
 *  @param length Length in bytes in [[buffer]]
 */
class ArrayBufferInputStream(val buffer: ArrayBuffer, val offset: Int,
    val length: Int) extends InputStream {

  /** Convenience constructor. Strictly equivalent to
   *  {{new ArrayBufferInputStream(buffer, 0, buffer.byteLength)}
   */ 
  def this(buffer: ArrayBuffer) = this(buffer, 0, buffer.byteLength)

  private val uintView = new Uint8Array(buffer, offset, length)
  private val byteView = new Int8Array(buffer, offset, length)

  protected val count: Int = offset + length
  protected var mark: Int = offset
  protected var pos: Int = offset

  override def available(): Int = count - pos
  override def mark(readlimit: Int): Unit = { mark = pos }
  override def markSupported(): Boolean = true
  def read(): Int = {
    if (pos < count) {
      val res = uintView(pos)
      pos += 1
      res
    } else -1
  }

  override def read(b: Array[Byte], off: Int, reqLen: Int): Int = {
    if (off < 0 || reqLen < 0 || reqLen > b.length - off)
      throw new IndexOutOfBoundsException

    val len = Math.min(reqLen, count - pos)

    if (reqLen == 0)
       0 // 0 requested, 0 returned
    else if (len == 0)
      -1 // nothing to read at all
    else {
      var i = 0
      while (i < len) {
        b(i + off) = byteView(pos + i)
        i += 1
      }
      pos += len
      len
    }
  }
  override def reset(): Unit = { pos = mark }
  override def skip(n: Long): Long = {
    val k = Math.max(0, Math.min(n.toInt, count - pos))
    pos += k
    k.toLong
  }

}
