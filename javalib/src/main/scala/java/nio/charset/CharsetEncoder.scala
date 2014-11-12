package java.nio.charset

import scala.annotation.{switch, tailrec}

import java.nio._

abstract class CharsetEncoder protected (cs: Charset,
    _averageBytesPerChar: Float, _maxBytesPerChar: Float,
    private[this] var _replacement: Array[Byte]) {

  import CharsetEncoder._

  protected def this(cs: Charset, _averageBytesPerChar: Float,
      _maxBytesPerChar: Float) =
    this(cs, _averageBytesPerChar, _averageBytesPerChar, Array('?'.toByte))

  // Config

  private[this] var _malformedInputAction: CodingErrorAction =
    CodingErrorAction.REPORT
  private[this] var _unmappableCharacterAction: CodingErrorAction =
    CodingErrorAction.REPORT

  // Status

  private[this] var status: Int = INIT

  // Methods

  final def charset(): Charset = cs

  final def replacement(): Array[Byte] = _replacement

  final def replaceWith(newReplacement: Array[Byte]): CharsetEncoder = {
    if (newReplacement == null || newReplacement.length == 0 ||
        newReplacement.length > maxBytesPerChar ||
        !isLegalReplacement(newReplacement))
      throw new IllegalArgumentException

    _replacement = newReplacement
    implReplaceWith(newReplacement)
    this
  }

  protected def implReplaceWith(newReplacement: Array[Byte]): Unit = ()

  def isLegalReplacement(repl: Array[Byte]): Boolean = {
    val decoder = charset.newDecoder
    val replBuf = ByteBuffer.wrap(repl)

    @inline
    @tailrec
    def loop(outBufSize: Int): Boolean = {
      val result = decoder.decode(replBuf, CharBuffer.allocate(outBufSize), true)
      if (result.isOverflow) {
        loop(outBufSize * 2)
      } else {
        !replBuf.hasRemaining
      }
    }

    loop(2)
  }

  def malformedInputAction(): CodingErrorAction = _malformedInputAction

  final def onMalformedInput(newAction: CodingErrorAction): CharsetEncoder = {
    if (newAction == null)
      throw new IllegalArgumentException("null CodingErrorAction")
    _malformedInputAction = newAction
    implOnMalformedInput(newAction)
    this
  }

  protected def implOnMalformedInput(newAction: CodingErrorAction): Unit = ()

  def unmappableCharacterAction(): CodingErrorAction = _unmappableCharacterAction

  final def onUnmappableCharacter(newAction: CodingErrorAction): CharsetEncoder = {
    if (newAction == null)
      throw new IllegalArgumentException("null CodingErrorAction")
    _unmappableCharacterAction = newAction
    implOnUnmappableCharacter(newAction)
    this
  }

  protected def implOnUnmappableCharacter(newAction: CodingErrorAction): Unit = ()

  final def averageBytesPerChar(): Float = _averageBytesPerChar
  final def maxBytesPerChar(): Float = _maxBytesPerChar

  final def encode(in: CharBuffer, out: ByteBuffer,
      endOfInput: Boolean): CoderResult = {

    if (status == FLUSHED || (!endOfInput && status == END))
      throw new IllegalStateException

    status = if (endOfInput) END else ONGOING

    @inline
    @tailrec
    def loop(): CoderResult = {
      val result1 = try {
        encodeLoop(in, out)
      } catch {
        case ex: BufferOverflowException =>
          throw new CoderMalfunctionError(ex)
        case ex: BufferUnderflowException =>
          throw new CoderMalfunctionError(ex)
      }

      val result2 = if (result1.isUnderflow) {
        val remaining = in.remaining
        if (endOfInput && remaining > 0)
          CoderResult.malformedForLength(remaining)
        else
          result1
      } else {
        result1
      }

      if (result2.isUnderflow || result2.isOverflow) {
        result2
      } else {
        val action =
          if (result2.isUnmappable) unmappableCharacterAction
          else malformedInputAction

        action match {
          case CodingErrorAction.REPLACE =>
            if (out.remaining < replacement.length) {
              CoderResult.OVERFLOW
            } else {
              out.put(replacement)
              in.position(in.position + result2.length)
              loop()
            }
          case CodingErrorAction.REPORT =>
            result2
          case CodingErrorAction.IGNORE =>
            in.position(in.position + result2.length)
            loop()
        }
      }
    }

    loop()
  }

  final def flush(out: ByteBuffer): CoderResult = {
    (status: @switch) match {
      case END =>
        val result = implFlush(out)
        if (result.isUnderflow)
          status = FLUSHED
        result
      case FLUSHED =>
        CoderResult.UNDERFLOW
      case _ =>
        throw new IllegalStateException
    }
  }

  protected def implFlush(out: ByteBuffer): CoderResult =
    CoderResult.UNDERFLOW

  final def reset(): CharsetEncoder = {
    status = INIT
    implReset()
    this
  }

  protected def implReset(): Unit = ()

  protected def encodeLoop(arg1: CharBuffer, arg2: ByteBuffer): CoderResult

  final def encode(in: CharBuffer): ByteBuffer = {
    def grow(out: ByteBuffer): ByteBuffer = {
      if (out.capacity == 0) {
        ByteBuffer.allocate(1)
      } else {
        val result = ByteBuffer.allocate(out.capacity*2)
        out.flip()
        result.put(out)
        result
      }
    }

    if (in.remaining == 0) {
      ByteBuffer.allocate(0)
    } else {
      @inline
      @tailrec
      def loopEncode(out: ByteBuffer): ByteBuffer = {
        val result = encode(in, out, endOfInput = true)
        if (result.isUnderflow) {
          assert(!in.hasRemaining)
          out
        } else if (result.isOverflow) {
          loopEncode(grow(out))
        } else {
          result.throwException()
          throw new AssertionError("should not get here")
        }
      }

      @inline
      @tailrec
      def loopFlush(out: ByteBuffer): ByteBuffer = {
        val result = flush(out)
        if (result.isUnderflow) {
          out
        } else if (result.isOverflow) {
          loopFlush(grow(out))
        } else {
          result.throwException()
          throw new AssertionError("should not get here")
        }
      }

      reset()
      val initLength = (in.remaining * averageBytesPerChar).toInt
      val out = loopFlush(loopEncode(ByteBuffer.allocate(initLength)))
      out.flip()
      out
    }
  }
}

object CharsetEncoder {
  private final val INIT = 0
  private final val ONGOING = 1
  private final val END = 2
  private final val FLUSHED = 3
}
