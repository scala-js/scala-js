package java.io

import scala.annotation.tailrec

import java.nio._
import java.nio.charset._

class InputStreamReader(private[this] var in: InputStream,
    private[this] var decoder: CharsetDecoder) extends Reader {

  private[this] var closed: Boolean = false

  /** Buffer in which to read bytes from the underlying input stream.
   *
   *  Class invariant: contains bytes already read from `in` but not yet
   *  decoded.
   */
  private[this] var inBuf: ByteBuffer = ByteBuffer.allocate(4096)
  inBuf.limit(0)

  /** Tells whether the end of the underlying input stream has been reached.
   *  Class invariant: if true, then `in.read()` has returned -1.
   */
  private[this] var endOfInput: Boolean = false

  /** Buffer in which to decode bytes into chars.
   *  Usually, it is not used, because we try to decode directly to the
   *  destination array. So as long as we do not really need one, we share
   *  an empty buffer.
   *
   *  Class invariant: contains chars already decoded but not yet *read* by
   *  the user of this instance.
   */
  private[this] var outBuf: CharBuffer = InputStreamReader.CommonEmptyCharBuffer

  def this(in: InputStream, charset: Charset) =
    this(in,
        charset.newDecoder
               .onMalformedInput(CodingErrorAction.REPLACE)
               .onUnmappableCharacter(CodingErrorAction.REPLACE))

  def this(in: InputStream) =
    this(in, Charset.defaultCharset)

  def this(in: InputStream, charsetName: String) =
    this(in, Charset.forName(charsetName))

  def close(): Unit = {
    closed = true
    in = null
    decoder = null
    inBuf = null
    outBuf = null
  }

  def getEncoding(): String =
    if (closed) null else decoder.charset.name

  override def read(): Int = {
    ensureOpen()

    if (outBuf.hasRemaining) outBuf.get()
    else super.read()
  }

  def read(cbuf: Array[Char], off: Int, len: Int): Int = {
    ensureOpen()

    if (off < 0 || len < 0 || len > cbuf.length - off)
      throw new IndexOutOfBoundsException

    if (len == 0) {
      0
    } else if (outBuf.hasRemaining) {
      // Reuse chars decoded last time
      val available = Math.min(outBuf.remaining, len)
      outBuf.get(cbuf, off, available)
      available
    } else if (!endOfInput) {
      // Try and decode directly into the destination array
      val directOut = CharBuffer.wrap(cbuf, off, len)
      val result = readImpl(directOut)
      if (result != InputStreamReader.Overflow) {
        result
      } else {
        /* There's not enough space in the destination array to receive even
         * a tiny bit of output from the decoder. We need to decode to the
         * outBuf instead.
         * This happens typically when the next code point to decode is a
         * supplementary character, and the given `len` is 1.
         */
        readMoreThroughOutBuf(cbuf, off, len)
      }
    } else {
      -1
    }
  }

  // In a separate method because this is (hopefully) not a common case
  private def readMoreThroughOutBuf(cbuf: Array[Char], off: Int, len: Int): Int = {
    // Return outBuf to its full capacity
    outBuf.limit(outBuf.capacity)
    outBuf.position(0)

    @tailrec // but not inline, this is not a common path
    def loopWithOutBuf(desiredOutBufSize: Int): Int = {
      if (outBuf.capacity < desiredOutBufSize)
        outBuf = CharBuffer.allocate(desiredOutBufSize)
      val charsRead = readImpl(outBuf)
      if (charsRead == InputStreamReader.Overflow)
        loopWithOutBuf(desiredOutBufSize*2)
      else
        charsRead
    }

    val charsRead = loopWithOutBuf(2*len)
    assert(charsRead != 0) // can be -1, though
    outBuf.flip()

    if (charsRead == -1) -1
    else {
      val available = Math.min(charsRead, len)
      outBuf.get(cbuf, off, available)
      available
    }
  }

  @tailrec
  private def readImpl(out: CharBuffer): Int = {
    val initPos = out.position
    val result = decoder.decode(inBuf, out, endOfInput)

    if (out.position != initPos) {
      /* Good, we made progress, so we can return.
       * Note that the `result` does not matter. Whether it's an underflow,
       * an overflow, or even an error, if we read *something*, we can return
       * that.
       * The next invocation of read() will cause a new invocation of decode(),
       * which will necessarily return the same result (but without advancing
       * at all), which will cause one of the following cases to be handled.
       */
      out.position - initPos
    } else if (result.isUnderflow) {
      if (endOfInput) {
        assert(!inBuf.hasRemaining,
            "CharsetDecoder.decode() should not have returned UNDERFLOW when "+
            "both endOfInput and inBuf.hasRemaining are true. It should have "+
            "returned a MalformedInput error instead.")
        // Flush
        if (decoder.flush(out).isOverflow) {
          InputStreamReader.Overflow
        } else {
          // Done
          if (out.position == initPos) -1
          else out.position - initPos
        }
      } else {
        // We need to read more from the underlying input stream
        if (inBuf.limit == inBuf.capacity) {
          inBuf.compact()
          if (!inBuf.hasRemaining) {
            throw new AssertionError(
                "Scala.js implementation restriction: " +
                inBuf.capacity + " bytes do not seem to be enough for " +
                getEncoding + " to decode a single code point. " +
                "Please report this as a bug.")
          }
          inBuf.limit(inBuf.position)
          inBuf.position(0)
        }

        /* Note that this stores the new data after the limit of the buffer.
         * Further, note that we may read more bytes than strictly necessary,
         * according to the specification of InputStreamReader.
         */
        val bytesRead =
          in.read(inBuf.array, inBuf.limit, inBuf.capacity - inBuf.limit)

        if (bytesRead == -1)
          endOfInput = true
        else
          inBuf.limit(inBuf.limit + bytesRead)

        readImpl(out)
      }
    } else if (result.isOverflow) {
      InputStreamReader.Overflow
    } else {
      result.throwException()
      throw new AssertionError("should not get here")
    }
  }

  /* In theory, `in.available() > 0` is incorrect. We should return true only
   * if there are enough bytes available to read at least one code point.
   * However, this is how the JDK behaves, and even the JavaDoc suggests this
   * is the expected behavior.
   */
  override def ready(): Boolean =
    outBuf.hasRemaining || in.available() > 0

  private def ensureOpen(): Unit = {
    if (closed)
      throw new IOException("Stream closed")
  }

}

object InputStreamReader {
  private final val Overflow = -2

  /** Empty CharBuffer shared by all InputStreamReaders as long as they do
   *  not really need one.
   *  Since we do not use `mark()`, it is fine to share them, because `mark()`
   *  is the only piece of mutable state for an empty buffer. Everything else
   *  is effectively immutable (e.g., position and limit must always be 0).
   */
  private val CommonEmptyCharBuffer = CharBuffer.allocate(0)
}
