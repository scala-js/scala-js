package java.nio.charset

import scala.annotation.{switch, tailrec}

import java.nio._

abstract class CharsetDecoder protected (cs: Charset,
    _averageCharsPerByte: Float, _maxCharsPerByte: Float) {

  import CharsetDecoder._

  // Config

  private[this] var _replacement: String = "\uFFFD"
  private[this] var _malformedInputAction: CodingErrorAction =
    CodingErrorAction.REPORT
  private[this] var _unmappableCharacterAction: CodingErrorAction =
    CodingErrorAction.REPORT

  // Status

  private[this] var status: Int = INIT

  // Methods

  final def charset(): Charset = cs

  final def replacement(): String = _replacement

  final def replaceWith(newReplacement: String): CharsetDecoder = {
    if (newReplacement == null || newReplacement == "")
      throw new IllegalArgumentException("Invalid replacement: "+newReplacement)
    if (newReplacement.length > maxCharsPerByte)
      throw new IllegalArgumentException(
          "Replacement string cannot be longer than maxCharsPerByte")
    _replacement = newReplacement
    implReplaceWith(newReplacement)
    this
  }

  protected def implReplaceWith(newReplacement: String): Unit = ()

  def malformedInputAction(): CodingErrorAction = _malformedInputAction

  final def onMalformedInput(newAction: CodingErrorAction): CharsetDecoder = {
    if (newAction == null)
      throw new IllegalArgumentException("null CodingErrorAction")
    _malformedInputAction = newAction
    implOnMalformedInput(newAction)
    this
  }

  protected def implOnMalformedInput(newAction: CodingErrorAction): Unit = ()

  def unmappableCharacterAction(): CodingErrorAction = _unmappableCharacterAction

  final def onUnmappableCharacter(newAction: CodingErrorAction): CharsetDecoder = {
    if (newAction == null)
      throw new IllegalArgumentException("null CodingErrorAction")
    _unmappableCharacterAction = newAction
    implOnUnmappableCharacter(newAction)
    this
  }

  protected def implOnUnmappableCharacter(newAction: CodingErrorAction): Unit = ()

  final def averageCharsPerByte(): Float = _averageCharsPerByte
  final def maxCharsPerByte(): Float = _maxCharsPerByte

  final def decode(in: ByteBuffer, out: CharBuffer,
      endOfInput: Boolean): CoderResult = {

    if (status == FLUSHED || (!endOfInput && status == END))
      throw new IllegalStateException

    status = if (endOfInput) END else ONGOING

    @inline
    @tailrec
    def loop(): CoderResult = {
      val result1 = try {
        decodeLoop(in, out)
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

  final def flush(out: CharBuffer): CoderResult = {
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

  protected def implFlush(out: CharBuffer): CoderResult =
    CoderResult.UNDERFLOW

  final def reset(): CharsetDecoder = {
    status = INIT
    implReset()
    this
  }

  protected def implReset(): Unit = ()

  protected def decodeLoop(in: ByteBuffer, out: CharBuffer): CoderResult

  final def decode(in: ByteBuffer): CharBuffer = {
    def grow(out: CharBuffer): CharBuffer = {
      if (out.capacity == 0) {
        CharBuffer.allocate(1)
      } else {
        val result = CharBuffer.allocate(out.capacity*2)
        out.flip()
        result.put(out)
        result
      }
    }

    @inline
    @tailrec
    def loopDecode(out: CharBuffer): CharBuffer = {
      val result = decode(in, out, endOfInput = true)
      if (result.isUnderflow) {
        assert(!in.hasRemaining)
        out
      } else if (result.isOverflow) {
        loopDecode(grow(out))
      } else {
        result.throwException()
        throw new AssertionError("should not get here")
      }
    }

    @inline
    @tailrec
    def loopFlush(out: CharBuffer): CharBuffer = {
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
    val initLength = (in.remaining.toDouble * averageCharsPerByte).toInt
    val out = loopFlush(loopDecode(CharBuffer.allocate(initLength)))
    out.flip()
    out
  }

  def isAutoDetecting(): Boolean = false

  def isCharsetDetected(): Boolean =
    throw new UnsupportedOperationException

  def detectedCharset(): Charset =
    throw new UnsupportedOperationException
}

object CharsetDecoder {
  private final val INIT = 1
  private final val ONGOING = 2
  private final val END = 3
  private final val FLUSHED = 4
}
