package java.io

import scala.annotation.tailrec

import java.nio._
import java.nio.charset._

class OutputStreamWriter(private[this] var out: OutputStream,
    private[this] var enc: CharsetEncoder) extends Writer {

  private[this] var closed: Boolean = false

  /** Incoming buffer: pending Chars that have been written to this instance
   *  of OutputStreamWriter, but not yet encoded.
   *  Normally, this should always be at most 1 Char, if it is a high surrogate
   *  which ended up alone at the end of the input of a write().
   */
  private[this] var inBuf: String = ""

  /** Outgoing buffer: Bytes that have been decoded (from `inBuf`), but not
   *  yet written to the underlying output stream.
   *  The valid bytes are between 0 and outBuf.position.
   */
  private[this] var outBuf: ByteBuffer = ByteBuffer.allocate(4096)

  def this(out: OutputStream, cs: Charset) =
    this(out,
        cs.newEncoder
          .onMalformedInput(CodingErrorAction.REPLACE)
          .onUnmappableCharacter(CodingErrorAction.REPLACE))

  def this(out: OutputStream) =
    this(out, Charset.defaultCharset)

  def this(out: OutputStream, charsetName: String) =
    this(out, Charset.forName(charsetName))

  def getEncoding(): String =
    if (closed) null else enc.charset.name

  override def write(c: Int): Unit =
    write(c.toChar.toString, 0, 1)

  override def write(cbuf: Array[Char], off: Int, len: Int): Unit =
    writeImpl(CharBuffer.wrap(cbuf, off, len))

  override def write(str: String, off: Int, len: Int): Unit =
    writeImpl(CharBuffer.wrap(str, off, off + len))

  private def writeImpl(cbuf: CharBuffer): Unit = {
    ensureOpen()

    val cbuf1 = if (inBuf != "") {
      val fullInput = CharBuffer.wrap(inBuf + cbuf.toString)
      inBuf = ""
      fullInput
    } else cbuf

    @inline
    @tailrec
    def loopEncode(): Unit = {
      val result = enc.encode(cbuf1, outBuf, false)
      if (result.isUnderflow) ()
      else if (result.isOverflow) {
        makeRoomInOutBuf()
        loopEncode()
      } else {
        result.throwException()
        throw new AssertionError("should not get here")
      }
    }

    loopEncode()
    if (cbuf1.hasRemaining)
      inBuf = cbuf1.toString
  }

  override def flush(): Unit = {
    ensureOpen()
    flushBuffer()
    out.flush()
  }

  override def close(): Unit = if (!closed) {
    // Finish up the input
    @inline
    @tailrec
    def loopEncode(): Unit = {
      val cbuf = CharBuffer.wrap(inBuf)
      val result = enc.encode(cbuf, outBuf, true)
      if (result.isUnderflow) {
        assert(!cbuf.hasRemaining,
            "CharsetEncoder.encode() should not have returned UNDERFLOW when "+
            "both endOfInput and inBuf.hasRemaining are true. It should have "+
            "returned a MalformedInput error instead.")
      } else if (result.isOverflow) {
        makeRoomInOutBuf()
        loopEncode()
      } else {
        result.throwException()
        throw new AssertionError("should not get here")
      }
    }

    @inline
    @tailrec
    def loopFlush(): Unit = {
      if (enc.flush(outBuf).isOverflow) {
        makeRoomInOutBuf()
        loopFlush()
      }
    }

    loopEncode()
    loopFlush()

    // Flush before closing
    flush()

    // Close the underlying stream
    out.close()

    // Clean up all the resources
    closed = true
    out = null
    enc = null
    inBuf = null
    outBuf = null
  }

  private def ensureOpen(): Unit = {
    if (closed)
      throw new IOException("Closed writer.")
  }

  private def makeRoomInOutBuf(): Unit = {
    if (outBuf.position != 0) {
      flushBuffer()
    } else {
      // Very unlikely (outBuf.capacity is not enough to encode a single code point)
      outBuf.flip()
      val newBuf = ByteBuffer.allocate(outBuf.capacity * 2)
      newBuf.put(outBuf)
      outBuf = newBuf
    }
  }

  /** Flushes the internal buffer of this writer, but not the underlying
   *  output stream.
   */
  private[io] def flushBuffer(): Unit = {
    ensureOpen()

    // Don't use outBuf.flip() first, in case out.write() throws
    // Hence, use 0 instead of position, and position instead of limit
    out.write(outBuf.array, outBuf.arrayOffset, outBuf.position)
    outBuf.clear()
  }

}
